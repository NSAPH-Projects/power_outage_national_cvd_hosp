# pull gridmet data

library(arrow)
library(tidyverse)
library(here)

i <- read_parquet(
  "/n/dominici_nsaph_l3/Lab/data/data_warehouse/dw_dorieh_climate/data/county/year=2018/part-0.parquet")

glimpse(i)

i <- i %>% select(state, county, observation_date, tmmn, tmmx, pr, vs)

write_parquet(i, "/n/dominici_nsaph_l3/Lab/projects/power_outage_national_cvd_hosp/data/meteo_vars.parquet")

