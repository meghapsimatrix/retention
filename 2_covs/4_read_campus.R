library(tidyverse)


# Read data ---------------------------------------------------------------

# get to the TEA folder and extract all files with class in it
files <- list.files("NewFilesReleased/TEA", pattern = "p_campus", full.names = TRUE)
files <- files[!str_detect(files, "f")][10:19]

# go through each files and read in the data and select particular columns
read_dat <- function(path, type){
  
  dat <- read_delim(path, delim = type) 
  
  return(dat)
  
}

# create a tibble (modern data frame) with path and separator (comma or tab)
params <- tibble(path = files, type = c(rep("\t", 8), ","))


# for each file read in the data, create a big data frame of all the datasets together
campus_dat <- params %>%
  mutate(
    res = pmap(., .f = read_dat)
  ) %>%
  unnest(cols = res)

# parse out the year from the path
campus_dat <- campus_dat %>%
  mutate(year = parse_number(path))

save(campus_dat, file =  "Revised Datasets/R/campus_dat.RData")
