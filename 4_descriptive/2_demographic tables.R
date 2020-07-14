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
# do table here
table(cohort_cert$system)

str(cohort_cert$scorenum)

#Grouping by and providing relative demographic proportions
demog_tables <- cohort_cert %>%
  group_by(trad, ethnicity, gender) %>%
  summarize(n = n()) %>%
  mutate(freq = n / sum(n)) 

# it's easier to do things one by one  - but you have the right idea 
# if you group by trad and gender and ethncity- it is going to give freq of people who are femalemale and white and in trad route -
# we normally don't care about joint dist like that
# we want dist of gender by trad only 
gender <- cohort_cert %>%
  group_by(trad, gender) %>%
  summarize(n = n()) %>%
  mutate(freq = n / sum(n)) 

ethnicity <- cohort_cert %>%
  group_by(trad, gender) %>%
  summarize(n = n()) %>%
  mutate(freq = n / sum(n))


score_num <- cohort_cert %>%
  group_by(trad) %>%
  summarize(m = mean(score_num),
            sd = sd(score_num))
  
  
  