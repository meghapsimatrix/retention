library(tidyverse)
library(LaF)
library(fastDummies)



# load campus data  -------------------------------------------------------
load("Revised Datasets/R/cohort_ec.RData")


# Read data ---------------------------------------------------------------

# get to the TEA folder and extract all files with class in it
files <- list.files("NewFilesReleased/TEA", pattern = "p_attend_demog", full.names = TRUE)[2:11] 
# only keep 09-10 to 18-19

# go through each files and read in the data and select particular columns
summarize_attend_dat <- function(path, type, campuses){
  
  dm <- detect_dm_csv(path, sep = type, header = TRUE, row.names = NULL)
  dm$columns[, "type"] <- "string"
  dat <- laf_open(dm, skip = 1)
  
  attend_dat <- dat[dat$CAMPUS_ACCNT[] %in% campuses, ]
  
  names(attend_dat) <- tolower(names(attend_dat))
  
  attend_dat_clean <- attend_dat %>%
    mutate(economic = case_when(economic == "00" ~ "nd",
                                economic == "01" ~ "free",
                                economic == "02" ~ "reduced",
                                economic == "99" ~ "other"))
  
  
  attend_dat_clean <- attend_dat_clean %>%
    select(id2, id1, district, sex, ethnic, economic, economic_2,
           campus_accnt, bil_esl_attend, gifted_attend, se_attend,
           lep_attend, title1_flag)
  
  # dummy code sex and ethnicity
  attend_dat_dum <- dummy_cols(attend_dat_clean, 
                               select_columns = c('sex', 'ethnic', 'economic'))
  
  summary_dat <- attend_dat_dum %>%
    group_by(campus_accnt) %>%
    summarize_at(vars(sex_F:economic_other, bil_esl_attend:title1_flag), mean) %>%
    ungroup()
  
  return(summary_dat)
  
}


# cohort_dat 
camp_dat <- cohort_dat_ec %>%
  select(year, CAMPUS) %>%
  distinct(., .keep_all = TRUE) %>%
  group_by(year) %>%
  nest(cohort_dat = CAMPUS)

files <- list.files("NewFilesReleased/TEA", pattern = "p_attend_demog", full.names = TRUE)

# create a tibble (modern data frame) with path and separator (comma or tab)
params <- tibble(path = files, type = rep(",", 10), cohort_dat = camp_dat$cohort_dat) # need to check 

# for each file read in the data, create a big data frame of all the datasets together
attend_dat <- params %>%  # take the params file (with path and type)
  mutate(
    res = pmap(., .f = summarize_attend_dat)  # for each line read the data with the right type, and then save it as a compressed data in a column called res
  ) %>%
  unnest(cols = res)# the stuff above nests the datasets in to a row and this function unnests it so we have the full data; data from each year is stacked on top of another


#class_dat1 <- map_df(files[1:11], read_csv)

attend_dat <- attend_dat %>%
  mutate(year = parse_number(path)) 


save(attend_dat, file = "Revised Datasets/R/cohort_final.RData")
