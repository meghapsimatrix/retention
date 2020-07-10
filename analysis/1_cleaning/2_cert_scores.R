library(tidyverse)
library(lubridate)


# Read data ---------------------------------------------------------------

cert_dat <- read_csv("NewFilesReleased/StBrdEdCert/sbec_test_2019_20191101_f.txt")
load("Revised Datasets/R/sbec_cohort.RData")

names(sbec_cohort)

# Join data ---------------------------------------------------------------

cohort_cert <- left_join(sbec_cohort, cert_dat, by = "id2")  %>% # join the clean cohort data to the certification data raw - it will only keep records thatmatch with the cohort
  distinct(., .keep_all = FALSE) %>%  #delete any completely duplicated records
  filter(str_detect(test_descr, "Pedagogy")) %>%   # keep only ped scores
  mutate(admin_dt = mdy(admin_dt)) %>% # turn date into date format from character format
  group_by(id2) %>% # for each person
  filter(admin_dt == min(admin_dt)) %>% # keep the initial score
  filter(scorenum == max(scorenum)) %>% # from dups keep the max score like matt said
  ungroup()


# Duplicates --------------------------------------------------------------

# checking duplicates 
cohort_cert %>%
  ungroup() %>%
  filter(duplicated(id2) | duplicated(id2, fromLast = TRUE)) %>% #in the data filter any that are duplicated id2 and show me all the duplicated values
  arrange(id2, admin_dt) %>%
  View()

# no duplicates 

 

# Check those without pedagogy --------------------------------------------

no_pedag <- anti_join(sbec_cohort, cohort_cert, by = "id2")
check_nopedag <- semi_join(cert_dat, no_ped, by = "id2")



# Save --------------------------------------------------------------------

names(cohort_cert)

save(cohort_cert, file = "Revised Datasets/R/cohort_cert.RData")
  