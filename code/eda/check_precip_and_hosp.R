# Check relationship between precipitation and CVD and respiratory 

pacman::p_load(here, tidyverse, arrow, mgcv)

an_dat <- read_rds(here('data', 'an_dat_urgent_hosp.RDS'))

an_dat <- na.omit(an_dat)
# run a model to see if the relationship between precipitation and hosp is 
# non-linear, using penalized spline and controlling for temp
m1 <- gam(n_resp ~ s(max_temp) + s(precip), offset = n_benes, 
                 family = poisson, 
                 data = an_dat)



