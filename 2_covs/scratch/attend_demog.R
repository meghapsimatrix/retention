path <- params %>% pull(path)[1]
type <- "/t"
campuses <- params %>%
  filter(year == 11) %>%
  pull(campuses)

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

num_students <- attend_dat_dum %>%
  group_by(campus_accnt) %>%
  summarize(n = n_distinct(id2)) %>%
  ungroup()

summary_dat <- attend_dat_dum %>%
  group_by(campus_accnt) %>%
  summarize_at(vars(sex_F:economic_other, bil_esl_attend:title1_flag), mean) %>%
  ungroup() %>%
  left_join(num_students, by = "campus_accnt")