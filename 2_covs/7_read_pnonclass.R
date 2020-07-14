library(tidyverse)
library(lubridate)
library(purrr)

# code for principal is 020

# Read data ---------------------------------------------------------------

# get to the TEA folder and extract all files with class in it
files <- list.files("NewFilesReleased/TEA", pattern = "p_nonclass", full.names = TRUE)

# go through each files and read in the data and select particular columns
read_dat <- function(path, type){
  
  dat <- read_delim(path, delim = type) %>%
    select(ID2, DISTRICT, CAMPUS, ROLE, SERVICE, POPSERV, STUDENTS, PFTE, PTIME, PBASEPAY, ASSIGN_ORG)
  
  return(dat)
  
}

# create a tibble (modern data frame) with path and separator (comma or tab)
params <- tibble(path = files, type = c(",", rep("\t", 9)))


# for each file read in the data, create a big data frame of all the datasets together
nonclass_dat <- params %>%
  mutate(
    res = pmap(., .f = read_dat)
  ) %>%
  unnest(cols = res)


nonclass_dat <- nonclass_dat %>%
  mutate(year = parse_number(path))

save(employ_dat, file =  "Revised Datasets/R/nonclass_dat.RData")
