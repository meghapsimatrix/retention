library(tidyverse)

source("CodeToArchive/Megha/exploratory/masking/Masking-functions.R")
load("Revised Datasets/R/cohort_cert.RData")

# group by trad/alt and get the % of gender, race ethncity, in cohort_cert file


# Gender ------------------------------------------------------------------

get_masked_pct <- function(dat, group, var){

  dat %>%
    group_by({{group}}) %>%
    summarize(N = n(),
              pct_var = pct_fct({{var}})
    ) %>%
    unnest(pct_var)

}


get_masked_pct(gender)


# if that doesn't work do the following - there might be issue with version of dplyr but just wanted to check if we can run functions

cohort_cert %>%
  group_by(trad) %>%
  summarize(N = n(),
            gender = pct_fct(gender)
  ) %>%
  unnest(gender)
