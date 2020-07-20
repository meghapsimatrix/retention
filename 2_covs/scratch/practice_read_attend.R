# get cohort - 2019 - only select the campus and keep only unique campuses
campus_dat <- cohort_ec_dat %>%
  filter(year == 19) %>%
  select(CAMPUS) %>%
  distinct(., .keep_all = TRUE)


# read in one p_attend data -----------------------------------------------

path <- "NewFilesReleased/TEA/p_attend_demog19.txt"
type <- ","


# detect the data model
dm <- detect_dm_csv(path, sep = type, header = TRUE, row.names = NULL)
dm$columns[, "type"] <- "string"

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

glimpse(attend_dat)

table(attend_dat$economic, useNA = "ifany")


attend_dat_clean <- attend_dat %>%
  mutate(economic = case_when(economic == "00" ~ "nd",
                              economic == "01" ~ "free",
                              economic == "02" ~ "reduced",
                              economic == "99" ~ "other"))

table(attend_dat_clean$economic, useNA = "ifany")


attend_dat_clean <- attend_dat_clean %>%
  select(id2, id1, district, sex, ethnic, economic, 
         campus_accnt, bil_esl_attend, gifted_attend, se_attend,
         lep_attend, title1_flag)

# dummy code sex and ethnicity
attend_dat_dum <- dummy_cols(attend_dat_clean, 
                             select_columns = c('sex', 'ethnic', 'economic'))


glimpse(attend_dat_dum)



# 3000 campuses 
summary_dat <- attend_dat_dum %>%
  group_by(campus_accnt) %>%
  summarize_at(vars(sex_F:economic_other, bil_esl_attend:title1_flag), mean)
