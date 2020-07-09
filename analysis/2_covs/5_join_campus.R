library(tidyverse)


# Read in the data --------------------------------------------------------


load("Revised Datasets/R/employ_cohort.RData")
load("Revised Datasets/R/campus_dat.RData")


# Join with the cohort data -----------------------------------------------

# join information on all schools with our data 
cohort_ec_dat <- left_join(employ_cohort, campus_dat, by = c("CAMPUS", "year"))

names(cohort_ec_dat)


save(cohort_ec_dat, file = "Revised Datasets/R/cohort_ec.RData")

load("Revised Datasets/R/cohort_ec.RData")

no_match <- anti_join(employ_cohort, campus_dat, by = c("CAMPUS", "year"))


no_match %>%
  select(id2, DISTRICT.x, DISTRICT.y, CAMPUS, year, EMPLOY_TYPE, DAYSEMP) %>%
  head(n = 200) %>%
  View()

cohort_ec_dat %>%
  filter(is.na(CAMPUS)) %>%
  summarize(n = n())


# campus_dat %>%
#  filter(year == 11)


table(cohort_ec_dat$CAMP_CHARTTYPE)


# cohort_ec_dat <- cohort_ec_dat %>%
#   mutate(charter = if_else(CAMP_CHARTTYPE == "00", 0, 1))


#table(cohort_ec_dat$charter, useNA = "ifany")
table(cohort_ec_dat$CAMP_CHARTTYPE, useNA = "ifany")
table(cohort_ec_dat$EMPLOY_TYPE, useNA = "ifany")
table(cohort_ec_dat$DEGREE, useNA = "ifany")

cohort_ec_dat <- cohort_ec_dat %>%
  distinct(., .keep_all = TRUE)

save(cohort_ec_dat, file = "Revised Datasets/R/cohort_ec.RData")


cohort_ec_dat %>%
  select(id2, cert_type, cert_field, org_name, org_type, institution_name, year, TENURE, DAYSEMP, EXPER, DISTRICT, CAMPUS, CAMPNAME, ROLE, SERVICE, PBASEPAY) %>%
  arrange(id2) %>%
  head(n = 200) %>%
  View()

load("Revised Datasets/R/cohort_ec.RData")

