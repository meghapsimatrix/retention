library(tidyverse)


# Read data and functions and clean ---------------------------------------

load("Revised Datasets/R/cohort_cert.RData")
source("CodeToArchive/Megha/analysis/masking/Masking-functions.R")

table(cohort_cert$trad, useNA = "ifany")
table(cohort_cert$system, useNA = "ifany")

corhot_cert <- cohort_cert %>%
  mutate(D = if_else(trad == 1, "Traditional", "Alternative"))



# by everything -----------------------------------------------------------

count_org_name <- cohort_cert %>%
  group_by(D, system, cert_pgm_cd, cert_type_cd, org_name) %>%
  summarize(n = n()) %>%
  mutate(n_masked = mask_counts(n)) %>%
  select(-n) 


count_org_name %>%
  View()

count_org_name %>%
  filter(org_name == "STATE BOARD FOR EDUCATOR CERTIFICATION") %>%
  View()


# By system and alt/trad classification -----------------------------------


count_overall <- cohort_cert %>%
  group_by(system, D) %>%
  summarize(n = n()) %>%
  mutate(n_masked = mask_counts(n)) %>%
  select(-n) 


# by alt trad classification ----------------------------------------------

D_overall <- cohort_cert %>%
  group_by(D) %>%
  summarize(n = n()) %>%
  mutate(n_masked = mask_counts(n)) %>%
  select(-n) 

D_overall

# UT Austin ---------------------------------------------------------------


ut_austin <- cohort_cert %>%
  filter(org_name == "UNIVERSITY OF TEXAS - AUSTIN") %>%
  summarize(n = n()) %>%
  mutate(n_masked = mask_counts(n)) %>%
  select(-n) 



# Write csv ---------------------------------------------------------------


write_csv(count_org_name, "Revised Datasets/count_org_name.csv")
write_csv(count_overall, "Revised Datasets/count_overall.csv")
write_csv(D_overall, "Revised Datasets/D_overall.csv")
write_csv(ut_austin, "Revised Datasets/ut_austin.csv")

