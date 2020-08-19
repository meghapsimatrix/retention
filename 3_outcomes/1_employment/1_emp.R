library(tidyverse)
library(estimatr)
library(broom)


load("Revised Datasets/R/cohort_ec.RData")

table(cohort_ec_dat$year)

glimpse(cohort_ec_dat)


covariates <- cohort_ec_dat %>%
  select(id2, trad, system, org_name, gender, ethnicity, degree_cd) %>%
  distinct(., .keep_all = TRUE)
 

year_dat <- cohort_ec_dat %>% 
  filter(ROLE == "087") %>%
  select(id2, year) %>%
  distinct(., .keep_all = TRUE) %>%
  mutate(status = ifelse(!is.na(year), 1, 0)) %>%  
  spread(year, status, fill = 0) %>% 
  gather(year, status, -1) 

table(year_dat$year)


# check_dups
check <- year_dat %>%
  group_by(year) %>%
  filter(duplicated(id2) | duplicated(id2, fromLast = TRUE))

year_dat %>%
  group_by(year) %>%
  count()


analyze_dat <- left_join(year_dat, covariates) %>%
  filter(year %in% c(11, 13, 15, 19)) %>%
  group_by(year) %>%
  ungroup() %>%
  rename(comp = trad)



# Traditional vs alt ------------------------------------------------------


estimate_ate <- function(equation = "status ~ comp + gender + ethnicity + degree_cd", dat){
  
  mod <- lm_robust(as.formula(equation), data = dat)
  
  tidy(mod) %>%
    filter(str_detect(term, "comp"))
}
  

res <- analyze_dat %>%
  group_by(year) %>%
  do(estimate_ate(.)) %>%
  ungroup() 

# descriptive stats

analyze_dat %>%
  group_by(year, org_name) %>%
  summarize(n = n(),
            p = mean(status)) %>%
  ungroup()



# UT Austin vs  -----------------------------------------------------------

analyze_dat_ut <- analyze_dat %>%
  mutate(comp = case_when(org_name == "UNIVERSITY OF TEXAS - AUSTIN", "UT Austin",
                             TRUE ~ system),
         comp = factor(comp),
         comp = relevel(comp, ref = "UT Austin")) %>%
  filter(!(comp %in% c("ALT", "Community College", "Other"))) 


res_ut <- analyze_dat_ut %>%
  group_by(year) %>%
  do(estimate_ate(.)) %>%
  ungroup() 

