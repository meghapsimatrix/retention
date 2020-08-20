library(tidyverse)

source("CodeToArchive/Megha/exploratory/masking/Masking-functions.R")
load("Revised Datasets/R/cohort_cert.RData")


cohort_cert <- cohort_cert %>%
  mutate(ethnicity = ifelse(ethnicity == "African American", "Black/African Amer", ethnicity),
         ethnicity = ifelse(ethnicity == "Hispanic", "Hipanic/Latino", ethnicity)) 

save(cohort_cert, file = "Revised Datasets/R/cohort_cert.RData")

# Check duplicates --------------------------------------------------------


dups_check <- cohort_cert %>%
  filter(duplicated(id2) | duplicated(id2, fromLast = TRUE))


# Age ---------------------------------------------------------------------

# we need age 


# Gender ------------------------------------------------------------------

gender_trad <- cohort_cert %>%
  mutate(gender = factor(gender)) %>%
  group_by(trad) %>%
  summarize(N = n(),
            gender = pct_fct(gender)
  ) %>%
  unnest(gender) %>%
  gather(var, percent, -c(1:2))

gender_system <- cohort_cert %>%
  filter(system %in% c("Texas AM", "Texas State", "Texas Tech", "UH", "UT")) %>%
  mutate(gender = factor(gender)) %>%
  group_by(system) %>%
  summarize(N = n(),
            gender = pct_fct(gender)
  ) %>%
  unnest(gender) %>%
  gather(var, percent, -c(1:2))

gender_ut <- cohort_cert %>%
  filter(org_name == "UNIVERSITY OF TEXAS - AUSTIN") %>%
  mutate(gender = factor(gender)) %>%
  group_by(org_name) %>%
  summarize(N = n(),
            gender = pct_fct(gender)
  ) %>%
  unnest(gender) %>%
  gather(var, percent, -c(1:2))


# Ethnicity ---------------------------------------------------------------

ethnicity_trad <- cohort_cert %>%
  mutate(ethnicity = factor(ethnicity)) %>%
  group_by(trad) %>%
  summarize(N = n(),
            ethnicity = pct_fct(ethnicity)
  ) %>%
  unnest(ethnicity) %>%
  gather(var, percent, -c(1:2))


ethnicity_system <- cohort_cert %>%
  filter(system %in% c("Texas AM", "Texas State", "Texas Tech", "UH", "UT")) %>%
  mutate(ethnicity = factor(ethnicity)) %>%
  group_by(system) %>%
  summarize(N = n(),
            ethnicity = pct_fct(ethnicity)
  ) %>%
  unnest(ethnicity) %>%
  gather(var, percent, -c(1:2))


ethnicity_ut <- cohort_cert %>%
  filter(org_name == "UNIVERSITY OF TEXAS - AUSTIN") %>%
  mutate(ethnicity = factor(ethnicity)) %>%
  group_by(org_name) %>%
  summarize(N = n(),
            ethnicity = pct_fct(ethnicity)
  ) %>%
  unnest(ethnicity) %>%
  gather(var, percent, -c(1:2))



# degree ------------------------------------------------------------------

# check what the name of the var is for prior degree earned

degree_trad <- cohort_cert %>%
  mutate(degree_cd = factor(degree_cd)) %>%
  group_by(trad) %>%
  summarize(N = n(),
            degree_cd = pct_fct(degree_cd)
  ) %>%
  unnest(degree_cd) %>%
  gather(var, percent, -c(1:2))


degree_system <- cohort_cert %>%
  filter(system %in% c("Texas AM", "Texas State", "Texas Tech", "UH", "UT")) %>%
  mutate(degree_cd = factor(degree_cd)) %>%
  group_by(system) %>%
  summarize(N = n(),
            degree_cd = pct_fct(degree_cd)
  ) %>%
  unnest(degree_cd) %>%
  gather(var, percent, -c(1:2))


degree_ut <- cohort_cert %>%
  filter(org_name == "UNIVERSITY OF TEXAS - AUSTIN") %>%
  mutate(degree_cd = factor(degree_cd)) %>%
  group_by(org_name) %>%
  summarize(N = n(),
            degree_cd = pct_fct(degree_cd)
  ) %>%
  unnest(degree_cd) %>%
  gather(var, percent, -c(1:2))




# combine -----------------------------------------------------------------

trad_dem <- bind_rows(gender_trad, ethnicity_trad, degree_trad)
system_dem <- bind_rows(gender_system, ethnicity_system, degree_system)
ut_dem <- bind_rows(gender_ut, ethnicity_ut, degree_ut)

write_csv(trad_dem, "Revised Datasets/trad_dem.csv")
write_csv(system_dem, "Revised Datasets/system_dem.csv")
write_csv(ut_dem, "Revised Datasets/ut_dem.csv")
