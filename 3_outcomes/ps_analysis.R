library(tidyverse)
library(twang)
library(cobalt)

load("Revised Datasets/R/cohort_cert.RData")

cohort_cert

get_weight <- function(equation, dat){
  
  gbm_mod <- ps(as.formula(equation), 
                data = dat,
                n.trees = 5000,
                interaction.depth = 3,
                shrinkage = 0.01,
                estimand = "ATE",
                stop.method = c("es.mean", "es.max"))
  
  ps_dat <- gbm_mod$ps
  
  dat$gbm_wt <- get.weights(gbm_mod, stop.method = "es.mean")  
  dat <- bind_cols(dat, ps_dat)
  
  return(dat)
  
}


cohort_cert <- cohort_cert %>%
  mutate_at(vars(gender, ethnicity, degree_cd), as.factor)

system.time(trad_wts <- get_weight(equation = "trad ~ gender + ethnicity + degree_cd", dat = cohort_cert))


save(ps_dat, file = "Revised Datasets/ps_dat.RData")
save(gbm_mod, file = "Revised Datasets/gbm_mod.RData")


balance_tab <- bal.tab(gbm_mod, estimand = "ATE")
love.plot(balance_tab)


