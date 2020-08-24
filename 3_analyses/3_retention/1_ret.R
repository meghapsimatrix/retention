library(tidyverse)
library(survival)
library(survminer)
library(coxphw)


load("Revised Datasets/R/emp_dat.RData")

emp_dat <- emp_dat %>%
  mutate(event = ifelse(status == 1, 0, 1)) %>%
  mutate(time = year - 11)


check <- dat %>%
  select(id, time, not_emp =  event) %>%
  distinct()


# when they first got employed --------------------------------------------

first_time_empl <- check %>%
  filter(not_emp == 0) %>%
  group_by(id) %>%
  filter(time == min(time) | time == max(time)) %>%
  ungroup()

max_emp <- first_time_empl %>%
  group_by(id) %>%
  filter(time == max(time)) %>%
  ungroup() %>%
  select(id, max_emp = time)


# when they quit ----------------------------------------------------------

quit <- left_join(check, max_emp, by = "id") %>%
  filter(not_emp == 1) %>%
  group_by(id) %>%
  filter(time > max_emp) %>%
  filter(time == min(time) | time == max(time)) %>%
  select(-max_emp) %>%
  mutate(time1 = min(time),
         time2 = max(time)) %>%
  select(id, time1, time2, not_emp) %>%
  ungroup() %>%
  distinct()

emp <- first_time_empl %>%
  group_by(id) %>%
  mutate(time1 = min(time),
         time2 = max(time)) %>%
  select(id, time1, time2, not_emp) %>%
  ungroup() %>%
  distinct()



# data --------------------------------------------------------------------

all_dat <- bind_rows(quit, emp) %>%
  arrange(id, not_emp) %>%
  left_join(emp_dat %>% select(id, trad) %>% distinct(), by = c("id")) %>%
  ungroup() %>%
  rename(D = trad)


  
  # Cox ph models -----------------------------------------------------------


# regular cox ph hazard
fit <- coxph(Surv(time1, time2, not_emp) ~ D + x + z, data = all_dat)
fit
exp(coefficients(fit))


# Weighted cox proportional hazard model
fit_coxphw <- coxphw(Surv(time1, time2, not_emp) ~ D + x + z, data = all_dat)
fit_coxphw
exp(coefficients(fit_coxphw))


# graph - ugly lol but we have very few data for the mock example
ggsurvplot(survfit(Surv(time1, time2, not_emp) ~ D, data = all_dat))