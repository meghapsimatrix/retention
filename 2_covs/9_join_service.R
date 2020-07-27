library(tidyverse)


load("Revised Datasets/R/final_dat.RData")

p_service <- read_csv()


# Join --------------------------------------------------------------------

final_dat <- left_join(final_dat, p_service, by = "SERVICE")

save(final_dat, file = "Revised Datasets/R/final_dat.RData")