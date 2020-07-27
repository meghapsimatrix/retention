library(tidyverse)



# Read data ---------------------------------------------------------------

# get to the TEA folder and extract all files with class in it
files <- list.files("NewFilesReleased/TEA", pattern = "p_class", full.names = TRUE)[12:20]

# go through each files and read in the data and select particular columns
read_dat <- function(path, type){
  
  dat <- read_delim(path, delim = type) %>%
    select(ID2, DISTRICT, CAMPUS, ROLE, SERVICE, POPSERV, STUDENTS, PFTE, PTIME, PBASEPAY, ASSIGN_ORG)
  
  return(dat)
  
}

# create a tibble (modern data frame) with path and separator (comma or tab)
params <- tibble(path = files, type = c(rep("\t", 9)))


# for each file read in the data, create a big data frame of all the datasets together
class_dat <- params %>%  # take the params file (with path and type)
  mutate(
    res = pmap(., .f = read_dat)  # for each line read the data with the right type, and then save it as a compressed data in a column called res
  ) %>%
  unnest(cols = res)# the stuff above nests the datasets in to a row and this function unnests it so we have the full data; data from each year is stacked on top of another


#class_dat1 <- map_df(files[1:11], read_csv)

class_dat <- class_dat %>%
  mutate(year = parse_number(path)) # extract the year from the path

names(class_dat)

save(class_dat, file = "Revised Datasets/R/class_dat.RData")
