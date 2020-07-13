library(tidyverse)
library(lubridate)
library(purrr)
library(LaF)
library(fastDummies)



# load campus data  -------------------------------------------------------
load("Revised Datasets/R/cohort_ec.RData")

# get cohort - 2019 - only select the campus and keep only unique campuses
campus_dat <- cohort_ec_dat %>%
  filter(year == 19) %>%
  select(CAMPUS) %>%
  distinct(., .keep_all = TRUE)


# read in one p_attend data -----------------------------------------------
  
path <- "NewFilesReleased/TEA/p_attend_demog19.txt"
type <- ","


# detect the data model
dm <- detect_dm_csv(path, sep = type, header = TRUE)
dm$columns <- "string"  # change all to string when reading it in

dat <- laf_open(dm, skip = 1)

head(dat)
dm

campuses <- campus_dat %>% pull(CAMPUS)
str(campuses)
campuses

# conversion to initial failed

# read in attend data where the campus matches campus in our cohort 
system.time(attend_dat <- dat[dat$CAMPUS_ACCNT[] %in% campuses, ])


# clean the names of the data and select just the demographic chracteristics that we need
names(attend_dat) <- tolower(names(attend_dat))

names(attend_dat)

attend_dat <- attend_dat %>%
  select(id2, id1, district, sex, ethnic, disability_flag, campus, bil_esl_attend, gifted_attend, se_attend, lep_attend, title1_flag, econ_attend)

# dummy code sex and ethnicity
attend_dat_dum <- dummy_cols(attend_dat, select_columns = c('sex', 'ethnic'))


glimpse(attend_dat_dum)


summary_dat <- attend_dat_dum %>%
  group_by(CAMPUS) %>%
  summarize_at(vars())


# Read data ---------------------------------------------------------------

# get to the TEA folder and extract all files with class in it
files <- list.files("NewFilesReleased/TEA", pattern = "p_attend_demog", full.names = TRUE) 
# only keep 09-10 to 18-19

# go through each files and read in the data and select particular columns
read_dat <- function(path, type, cohort_dat){
  
  campus_dat <- cohort_dat %>%
    select(CAMPUS) %>%
    distinct(., .keep_all = TRUE)
  
  dm <- detect_dm_csv(path, sep = type, header = TRUE)
  dat <- laf_open(dm, skip = 1)
  dm$columns <- "string"
  
  attend_dat <- dat[dat$CAMPUS_ACCNT[] %in% campus_dat$CAMPUS, ]
  
  names(attend_dat) <- tolower(names(attend_dat))
  
  attend_dat <- attend_dat %>%
    select(id2, id1, district, sex, ethnic, disability_flag, campus, bil_esl_attend, gifted_attend, se_attend, lep_attend, title1_flag)
  
  attend_dat_dum <- dummy_cols(attend_dat, select_columns = c('sex', 'ethnic'))
  
  summary_dat <- attend_dat_dum %>%
    group_by(campus)
  
  return(attend_dat)
  
}


# cohort_dat 

camp_dat <- cohort_dat_ec %>%
  select(year, CAMPUS) %>%
  distinct(., .keep_all = TRUE) %>%
  group_by(year) %>%
  nest(cohort_data = CAMPUS)


files <- list.files("NewFilesReleased/TEA", pattern = "p_attend_demog", full.names = TRUE)


# create a tibble (modern data frame) with path and separator (comma or tab)
params <- tibble(path = files, type = rep(",", 10)) # need to check 



# for each file read in the data, create a big data frame of all the datasets together
attend_dat <- params %>%  # take the params file (with path and type)
  mutate(
    res = pmap(., .f = read_dat)  # for each line read the data with the right type, and then save it as a compressed data in a column called res
  ) %>%
  unnest(cols = res)# the stuff above nests the datasets in to a row and this function unnests it so we have the full data; data from each year is stacked on top of another


#class_dat1 <- map_df(files[1:11], read_csv)

attend_dat <- attend_dat %>%
  mutate(year = parse_number(path)) %>% # extract the year from the path
  filter(year > 8) # just keep years 09-19

names(attend_dat)

save(attend_dat, file = "Revised Datasets/R/cohort_final.RData")
