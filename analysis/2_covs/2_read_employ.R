library(tidyverse)

load("Revised Datasets/R/cohort_cert.RData")

library(tidyverse)
library(lubridate)
library(purrr)



# Read data ---------------------------------------------------------------

# get to the TEA folder and extract all files with class in it
files <- list.files("NewFilesReleased/TEA", pattern = "p_employ", full.names = TRUE)[11:20]

# go through each files and read in the data and select particular columns
read_dat <- function(path, type){
  
  dat <- read_delim(path, delim = type) 
  
  return(dat)
  
}

# create a tibble (modern data frame) with path and separator (comma or tab)
params <- tibble(path = files, type = c(",", rep("\t", 9)))


# for each file read in the data, create a big data frame of all the datasets together
employ_dat <- params %>%
  mutate(
    res = pmap(., .f = read_dat)
  ) %>%
  unnest(cols = res)


employ_dat <- employ_dat %>%
  mutate(year = parse_number(path))

save(employ_dat, file =  "Revised Datasets/R/employ_dat.RData")


# check year
load("Revised Datasets/R/employ_dat.RData")
