library(tidyverse)
library(lubridate)
library(readxl)


# Read in files -----------------------------------------------------------

# read the raw full sbec data
sbec_dat_c <- read_csv("NewFilesReleased/StBrdEdCert/sbec_2019c_20191101_f.txt")
sbec_dat_nc <- read_csv("NewFilesReleased/StBrdEdCert/sbec_2019nc_20191101_f.txt")

# bind the files together
sbec_dat_full <- bind_rows(sbec_dat_c, sbec_dat_nc)

# check how many rows
nrow(sbec_dat_full)


# check type and program
table(sbec_dat_full$cert_type_cd)
table(sbec_dat_full$cert_pgm_cd)



# Clean and get the cohort  -----------------------------------------------

sbec_dat <- sbec_dat_full %>%
  mutate(cert_effective_dt_clean = mdy_hms(cert_effective_dt),  # transform the effective cert date to date format
         cert_year = year(cert_effective_dt_clean),  # extract year and create a new var cert_year
         cert_month = month(cert_effective_dt_clean), # extract month and create cert_month
         cohort_year = if_else(cert_month < 9, cert_year, cert_year + 1)) %>% 
  group_by(id2) %>%
  filter(cert_effective_dt_clean == min(cert_effective_dt_clean)) %>%
  ungroup() %>%
  filter(cert_type_cd %in% c("STD", "INT", "PRO"),   # only keep where std and prob and int and keep where cert_pgm is not HB
         cert_pgm_cd %in% c("ALT", "PB", "STD")) %>%
  filter(cohort_year == 2010)  # 09-10


nrow(sbec_dat)
table(sbec_dat$cert_year, sbec_dat$cert_month)


# check tables
table(sbec_dat$cert_type_cd)
table(sbec_dat$cert_pgm_cd)
table(sbec_dat$cert_effective_dt_clean)


# take the sbec dat
# then mutate means create another variable called IHE
# str_detect detects college, College, University, university in org_name tag them as true for IHE
# converts true to 1 and false to 0 
sbec_dat <- sbec_dat %>%
  mutate(IHE = as.numeric(str_detect(org_name, "COLLEGE|UNIV|COLL")))


ihe_dat <- sbec_dat %>%  #take sbec dat
  select(org_name, IHE) %>%  # select these two columns only 
  distinct(., .keep_all = TRUE) # removes any duplicates , keep all the variables 

ihe_dat %>%
  View() 

View(ihe_dat)

write_csv(ihe_dat, "Revised Datasets/ihe_dat.csv")


# Duplicates --------------------------------------------------------------

sbec_dat_clean <- sbec_dat

# count if they have more than 1 case in the data
dups <- sbec_dat_clean %>% 
  group_by(id2) %>%
  count() %>%
  ungroup() 


sbec_dat_no_dup <- sbec_dat_clean %>%
  left_join(dups) %>%
  mutate(cert_field = ifelse(n > 1, "Multiple", cert_field)) %>%  # if they have more than one obs count as multiple
  distinct(id2, .keep_all = TRUE) # only keep distinct ids


table(sbec_dat_no_dup$cert_type_cd)


# Cohort data -------------------------------------------------------------

sbec_cohort <- sbec_dat_no_dup 


# cleaned IHE outside ERC so taking out IHE and then going to join it below
sbec_cohort <- sbec_cohort %>%
  select(-IHE)

names(sbec_cohort)
nrow(sbec_cohort)

# check duplicates
sbec_cohort %>%
  filter(duplicated(id2))


# check the cert_fields that are included 
included_fields <- tibble(inc = unique(sbec_cohort$cert_field))


# System classification ------------------------------------------------------

# will need to join the systems classification

ihe_dat <- read_xlsx("Revised Datasets/grouped_dat.xlsx")

sbec_cohort <- left_join(sbec_cohort, ihe_dat, by = "org_name")
check <- anti_join(sbec_cohort, ihe_dat, by = "org_name")

names(sbec_cohort)

table(sbec_cohort$system)

sbec_cohort %>%
  group_by(IHE, cert_type_cd, cert_pgm_cd) %>%
  count()


sbec_cohort <- sbec_cohort %>%
  filter(system != "Community College") %>% # dropping comm college
  mutate(IHE = if_else(is.na(IHE), 1, IHE),
         trad = if_else(IHE == 1 & 
                        cert_type_cd == "STD" & 
                        cert_pgm_cd %in% c("STD", "PB"), 
                        1, 0))

table(sbec_cohort$trad)

sbec_cohort %>%
  group_by(trad, IHE, cert_type_cd, cert_pgm_cd) %>%
  count()


sbec_cohort %>%
  group_by(org_name) %>%
  count() %>%
  filter(org_name %in% c("HARRIS COUNTY DEPT OF ED", "STATE BOARD FOR EDUCATOR CERTIFICATION",
                         "TEXAS DEPARTMENT OF HUMAN RESOURCES", "TEXAS EDUCATION AGENCY")) %>%
  View()




# Checks ------------------------------------------------------------------


dups_check <- sbec_cohort %>%
  filter(duplicated(id2) | duplicated(id2, fromLast = TRUE))

table(dups_check$cycle)


nrow(sbec_cohort)
names(sbec_cohort)

# Save the data -----------------------------------------------------------
save(sbec_cohort, file = "Revised Datasets/R/sbec_cohort.RData")
