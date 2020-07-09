library(tidyverse)



# Read data ---------------------------------------------------------------

load("Revised Datasets/R/employ_dat.RData")
load("Revised Datasets/R/cohort_cert.RData")
load("Revised Datasets/R/class_dat.RData")


# Join --------------------------------------------------------------------

names(employ_dat)
names(class_dat)

# join employ data with our cohort data 
employ_cohort <- left_join(cohort_cert, employ_dat, by = c("id2" = "ID2"))

# join the class data - the school characteristics with the above data 
employ_cohort <- employ_cohort %>%
  left_join(class_dat, by = c("id2" = "ID2", "year"))

# 
# DEGREE,DEGREEX,DTUPDATE,DATE_UPDATE
# ,UNKNOWN,21NOV1989,08/01/2008
# 0,NO BACHELOR'S DEGREE OR HIGHER,07SEP2006,08/01/2008
# 1,BACHELOR'S,21NOV1989,08/01/2008
# 2,MASTER'S,21NOV1989,08/01/2008
# 3,DOCTORATE,21NOV1989,08/01/2008

names(employ_cohort)

# just checking
employ_cohort %>%
  select(id2, gender, ethnicity, test_descr, scorenum, DISTRICT.x, TENURE, EXPER, DEGREE, year, DISTRICT.y, CAMPUS, ROLE, SERVICE, POPSERV, STUDENTS) %>%
  head() %>%
  View()


# 704 teachers who had no record in the employ data
no_emp_rec <- anti_join(cohort_cert, employ_dat, by = c("id2" = "ID2"))


no_emp_rec %>%
 # select(id2, gender, ethnicity, cert_pgm, cert_field, city, test_descr, scorenum) %>%
  View()

# save the data
save(employ_cohort, file = "Revised Datasets/R/employ_cohort.RData")

#demog_emply <- read_csv("NewFilesReleased/TEA/p_demog_employssn10f.txt")
