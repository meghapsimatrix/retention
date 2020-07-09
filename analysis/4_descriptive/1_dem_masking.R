library(tidyverse)

source("CodeToArchive/Megha/exploratory/masking/Masking-functions.R")
load("Revised Datasets/R/cohort_cert.RData")

# group by trad/alt and get the % of gender, race ethncity, in cohort_cert file


# Gender ------------------------------------------------------------------

get_mask <- function(dat = sbec_cohort, var){

  dat %>%
    group_by(cert_pgm_cd) %>%
    summarize(N = n(),
              pct_var = pct_fct({{var}})
    ) %>%
    unnest(pct_var)

}


get_mask(gender)


# if that doesn't work do the following - there might be issue with version of dplyr but just wanted to check if we can run functions

sbec_cohort %>%
  group_by(cert_pgm_cd) %>%
  summarize(N = n(),
            gender = pct_fct(gender)
  ) %>%
  unnest(gender)
