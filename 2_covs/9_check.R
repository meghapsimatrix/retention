library(tidyverse)



# Read data ---------------------------------------------------------------

load("Revised Datasets/R/final_dat.RData")

final_dat %>%
  group_by(id2) %>%
  summarize(n = n()) %>%
  filter(n != 10)


glimpse(final_dat)