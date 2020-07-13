# TITLE: Demographic Tables
# AUTHORS: Janet S. Rodriguez & Megha Joshi 
# FILE LOCATION: 'E:/projects/122-Retention/CodeToArchive/Megha/analysis/4_descriptive/'

# DATE STARTED: 7/12/2020
# DATE UPDATED: 

# DESCRIPTION: 
# This script will provide relative demographic proportions for teachers considered to have gone through a 'trad' teaching route 
# (i.e., graduated from a higher ed institute, received a 'STD' cert_type, and who went through a cert_pgm considered 'STD' or 'PB'). 

#Reading in cohort_cert data
library(tidyverse)
library(dplyr)

load("Revised Datasets/R/cohort_cert.RData")

names(cohort_cert)
cohort_cert$system ## QUESTION - how do I get the names of the rows in the 'system' variable?
str(cohort_cert$scorenum)

#Grouping by and providing relative demographic proportions
demog_tables <- cohort_cert %>%
  group_by(trad, ethnicity, gender) %>%
  summarize(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  summarize(mean_scorenum = mean(cohort_cert$scorenum), sd_scorenum = sd(cohort_cert$scorenum)) %>% #getting mean and sd
  # of scorenum (i.e., certification score) for each of the groups
  filter(system == UH) ### NEED TO WORK ON FILTERING COHORT_CERT BY SYSTEM AND UT AUSTIN BY ITSELF
  
  
  