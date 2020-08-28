library(tidyverse)
library(survival)
library(survminer)
library(coxphw)
library(broom)


load("Revised Datasets/R/emp_yrs_dat.RData")

glimpse(emp_yrs_dat)

emp_yrs_dat <- emp_yrs_dat %>%
  mutate(event = ifelse(status == 1, 0, 1)) %>%
  mutate(year = as.numeric(year),
         time = year - 11)


check <- emp_yrs_dat %>%
  select(id2, time, not_emp =  event) %>%
  distinct()


# when they first got employed --------------------------------------------

first_time_empl <- check %>%
  filter(not_emp == 0) %>%
  group_by(id2) %>%
  filter(time == min(time) | time == max(time)) %>%
  ungroup()

max_emp <- first_time_empl %>%
  group_by(id2) %>%
  filter(time == max(time)) %>%
  ungroup() %>%
  select(id2, max_emp = time)


# when they quit ----------------------------------------------------------

quit <- left_join(check, max_emp, by = "id2") %>%
  filter(not_emp == 1) %>%
  group_by(id2) %>%
  filter(time > max_emp) %>%
  filter(time == min(time) | time == max(time)) %>%
  select(-max_emp) %>%
  mutate(time1 = min(time),
         time2 = max(time)) %>%
  select(id2, time1, time2, not_emp) %>%
  ungroup() %>%
  distinct()

emp <- first_time_empl %>%
  group_by(id2) %>%
  mutate(time1 = min(time),
         time2 = max(time)) %>%
  select(id2, time1, time2, not_emp) %>%
  ungroup() %>%
  distinct()



# data --------------------------------------------------------------------

all_dat <- bind_rows(quit, emp) %>%
  arrange(id2, not_emp) %>%
  left_join(emp_yrs_dat %>% select(id, trad) %>% distinct(), by = c("id2")) %>%
  ungroup() %>%
  rename(D = trad)


load("Revised Datasets/R/cohort_ec.RData")

table(cohort_ec_dat$year)
glimpse(cohort_ec_dat)

covariates <- cohort_ec_dat %>%
  select(id2, trad, system, org_name, gender, ethnicity, degree_cd) %>%
  distinct(., .keep_all = TRUE) %>%
  mutate(org_name = ifelse(is.na(org_name), "Missing", org_name),
         degree_cd = ifelse(is.na(degree_cd), "Missing", degree_cd))


cov_all_dat <- left_join(all_dat, covariates, by = c("id2"))

nrow(all_dat)
nrow(cov_all_dat)

# Cox ph models -----------------------------------------------------------


# regular cox ph hazard
fit <- coxph(Surv(time1, time2, not_emp) ~ D + gender + ethnicity + degree_cd, data = cov_all_dat)
fit
exp(coefficients(fit))


surv_res <- tidy(fit) %>%
  mutate(exp_coef = exp(estimate))

write_csv(surv_res, "Revised Datasets/surv_res.csv")


# Weighted cox proportional hazard model
fit_coxphw <- coxphw(Surv(time1, time2, not_emp) ~ D, data = all_dat)
fit_coxphw
exp(coefficients(fit_coxphw))


# graph - ugly lol but we have very few data for the mock example
ggsurvplot(survfit(Surv(time1, time2, not_emp) ~ D, data = all_dat))

ggsave("Revised Datasets/surv_plot.png", device = "png", dpi = 500, height = 5, width = 7)