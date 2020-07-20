library(tidyverse)

source("CodeToArchive/Megha/exploratory/masking/Masking-functions.R")
load("Revised Datasets/R/cohort_ec.RData")



# Check -------------------------------------------------------------------

cohort_ec %>%
  group_by(year, id2) %>%
  summarize(n = n()) %>%
  filter(n > 1)
  
