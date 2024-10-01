# Pull gridmet data already cleaned by NSAPH

# Libraries ---------------------------------------------------------------

library(arrow)
library(tidyverse)
library(here)

# Read --------------------------------------------------------------------

meteo_vars <- read_parquet(paste0("/n/dominici_nsaph_l3/Lab/data/",
"data_warehouse/dw_dorieh_climate/data/county/year=2018/part-0.parquet"))

# Select cols, write ------------------------------------------------------

meteo_vars <- meteo_vars %>%
  select(state, county, observation_date, tmmn, tmmx, pr, vs)

write_rds(meteo_vars, here('data', 'meteo_vars.RDS'))

