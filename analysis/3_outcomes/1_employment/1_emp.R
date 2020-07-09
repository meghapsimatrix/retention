library(tidyverse)
#library(survival)
library(ggfortify)
#library(survminer)
library(stringr)
library(estimatr)
library(broom)


load("Revised Datasets/R/cohort_ec.RData")

table(cohort_ec_dat$year)

glimpse(cohort_ec_dat)


covariates <- cohort_ec_dat %>%
  select(id2, gender, ethnicity, org_name, org_type, cert_type, cert_field, cert_field_group, degree_cd) %>%
  distinct(., .keep_all = TRUE)
 

year_dat <- cohort_ec_dat %>% 
  filter(ROLE == "087") %>%
  select(id2, year) %>%
  distinct(., .keep_all = TRUE) %>%
  mutate(status = ifelse(!is.na(year), 1, 0)) %>%  
  spread(year, status, fill = 0) %>% # 1 is censored 2 is not employed anymore
  select(1:11) %>%
  gather(year, status, -1) %>%
  mutate(year = as.numeric(year))

table(year_dat$year)


# check_dups
check <- year_dat %>%
  group_by(year) %>%
  filter(duplicated(id2) | duplicated(id2, fromLast = TRUE))

year_dat %>%
  group_by(year) %>%
  count()



analyze_dat <- left_join(year_dat, covariates) %>%
  filter(year %in% c(10, 12, 14, 19)) %>%
  group_by(year) %>%
  mutate(org_name = factor(org_name),
         org_name = fct_lump_min(org_name, min = 40)) %>%
  ungroup() %>%
  filter()

table(analyze_dat$org_name)



# Linear Probability  -----------------------------------------------------

estimate_fe <- function(dat){
  
  mod <- lm_robust(status ~ 0 + org_name + org_type + gender + ethnicity + cert_field + degree_cd, data = dat, se_type = "HC0")
  
  tidy(mod) %>%
    filter(str_detect(term, "org_name"))
}
  

# estimate_fe <- function(dat){
#   
#   mod <- lm_robust(status ~ 0 + org_name, data = dat, se_type = "HC0")
#   
#   tidy(mod) %>%
#     filter(str_detect(term, "org_name"))
# }


res <- analyze_dat %>%
  #ungroup() %>%
  group_by(year) %>%
  do(estimate_fe(.)) %>%
  ungroup() %>%
  filter(str_detect(term, "UNIVERSITY OF TEXAS - AUSTIN"))


# coxph(Surv(year, status) ~ gender + ethnicity + org_name, data = analyze_dat)

analyze_dat %>%
  group_by(year, org_name) %>%
  summarize(n = n(),
            p = mean(status)) %>%
  ungroup() %>%
  filter(org_name == "UNIVERSITY OF TEXAS - AUSTIN")
