library(tidyverse)

source("CodeToArchive/Megha/exploratory/masking/Masking-functions.R")
load("Revised Datasets/R/cohort_cert.RData")



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
  unnest(gender)

gender_system <- cohort_cert %>%
  mutate(gender = factor(gender)) %>%
  group_by(system) %>%
  summarize(N = n(),
            gender = pct_fct(gender)
  ) %>%
  unnest(gender)

gender_ut <- cohort_cert %>%
  filter(org_name == "UNIVERSITY OF TEXAS - AUSTIN") %>%
  mutate(gender = factor(gender)) %>%
  summarize(N = n(),
            gender = pct_fct(gender)
  ) %>%
  unnest(gender)


# Ethnicity ---------------------------------------------------------------

ethnicity_trad <- cohort_cert %>%
  mutate(ethnicity = factor(ethncity)) %>%
  group_by(trad) %>%
  summarize(N = n(),
            ethnicity = pct_fct(ethnicity)
  ) %>%
  unnest(ethnicity)


ethnicity_system <- cohort_cert %>%
  mutate(ethnicity = factor(ethncity)) %>%
  group_by(system) %>%
  summarize(N = n(),
            ethnicity = pct_fct(ethnicity)
  ) %>%
  unnest(ethnicity)


ethnicity_ut <- cohort_cert %>%
  filter(org_name == "UNIVERSITY OF TEXAS - AUSTIN") %>%
  mutate(ethnicity = factor(ethncity)) %>%
  group_by(system) %>%
  summarize(N = n(),
            ethnicity = pct_fct(ethnicity)
  ) %>%
  unnest(ethnicity)



# degree ------------------------------------------------------------------

# check what the name of the var is for prior degree earned

degree_trad <- cohort_cert %>%
  mutate(degree= factor(degree)) %>%
  group_by(trad) %>%
  summarize(N = n(),
            degree = pct_fct(degree)
  ) %>%
  unnest(degree)


degree_system <- cohort_cert %>%
  mutate(degree = factor(ethncity)) %>%
  group_by(system) %>%
  summarize(N = n(),
            degree = pct_fct(degree)
  ) %>%
  unnest(degree)


degree_ut <- cohort_cert %>%
  filter(org_name == "UNIVERSITY OF TEXAS - AUSTIN") %>%
  mutate(degree = factor(ethncity)) %>%
  group_by(system) %>%
  summarize(N = n(),
            degree = pct_fct(degree)
  ) %>%
  unnest(degree)
