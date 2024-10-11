# Create analytic dataset from exposure and outcome data

# Libraries ---------------------------------------------------------------

pacman::p_load(tidyverse, data.table, here, arrow, gnm)

# Read --------------------------------------------------------------------

an_dat <- read_rds(here('data', 'an_dat.RDS'))
an_dat <- an_dat %>% mutate(all_hosp = n_cvd + n_resp)


m1 <-
  gnm(
    all_hosp ~ exposed_8_hrs_0.01 + max_temp + precip + 
      wind_speed + lag_1 + lag_2 + lag_3,
    offset = n_benes,
    family = 'poisson',
    eliminate = as.factor(stratum),
    data = an_dat
  )
