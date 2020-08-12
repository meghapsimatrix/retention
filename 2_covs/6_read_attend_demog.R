library(tidyverse)
library(LaF)
library(fastDummies)



# load campus data  -------------------------------------------------------
load("Revised Datasets/R/cohort_ec.RData")


# Read data ---------------------------------------------------------------

# get to the TEA folder and extract all files with class in it
files <- list.files("NewFilesReleased/TEA", pattern = "p_attend_demog", full.names = TRUE)[3:11] 
# only keep 10-11 to 18-19
files

# go through each files and read in the data and select particular columns
summarize_attend_dat <- function(path, type, campuses){
  
  campuses <- unnest(campuses)
  
  dm <- detect_dm_csv(path, sep = type, header = TRUE, row.names = NULL)
  dm$columns[, "type"] <- "string"
  dat <- laf_open(dm, skip = 1)
  
  attend_dat <- dat[dat$CAMPUS_ACCNT[] %in% campuses$CAMPUS, ]
  
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
  
  num_students <- attend_dat_dum %>%
    group_by(campus_accnt) %>%
    summarize(n = n_distinct(id2)) %>%
    ungroup()
  
  summary_dat <- attend_dat_dum %>%
    group_by(campus_accnt) %>%
    distinct(id2, .keep_all = TRUE) %>%  # there will be only one or two people duplicated in each campus per year so prob won't matter
    summarize_at(vars(sex_F:economic_other, bil_esl_attend:title1_flag), mean) %>%
    ungroup() %>%
    left_join(num_students, by = "campus_accnt")
  
  return(summary_dat)
  
}


# cohort_dat 
camp_dat <- cohort_ec_dat %>%
  select(year, CAMPUS) %>%
  distinct(., .keep_all = TRUE) %>%
  filter(!is.na(year)) %>%
  group_by(year) %>%
  nest(cohort_dat = CAMPUS) %>%
  ungroup()

camp_dat

dim(camp_dat)
length(files)

# create a tibble (modern data frame) with path and separator (comma or tab)
params <- tibble(path = files, type = c(rep("\t", 8), ","), campuses = camp_dat$data) # need to check 
params

# for each file read in the data, create a big data frame of all the datasets together
attend_dat <- params %>%  # take the params file (with path and type)
  mutate(
    res = pmap(., .f = summarize_attend_dat)  # for each line read the data with the right type, and then save it as a compressed data in a column called res
  ) %>%
  unnest(cols = res)# the stuff above nests the datasets in to a row and this 
 #function unnests it so we have the full data; 
 #data from each year is stacked on top of another


attend_dat <- attend_dat %>%
  mutate(year = parse_number(path)) 

glimpse(attend_dat)
table(attend_dat$year) # 11 - 19

table(is.na(attend_dat$sex_F))
table(is.na(attend_dat$ethnic_W))
table(is.na(attend_dat$ethnic_H))
table(is.na(attend_dat$ethnic_B))
table(is.na(attend_dat$economic_free))
table(is.na(attend_dat$economic_reduced))

save(attend_dat, file = "Revised Datasets/R/attend_dat.RData")



