library(tidyverse)


# Read in the data --------------------------------------------------------


load("Revised Datasets/R/cohort_ec.RData")
load("Revised Datasets/R/nonclass_dat.RData")



# Join --------------------------------------------------------------------

cohort_ecn_dat <- left_join(cohort_ec_dat, nonclass_dat, by = c("id2" = "ID2", "year"))
names(cohort_ecn_dat)


save(final_dat, file = "Revised Datasets/R/final_dat.RData")