# Pull gridmet data already cleaned by NSAPH

# Libraries ---------------------------------------------------------------

pacman::p_load(arrow, tidyverse, here, sf)

# Read --------------------------------------------------------------------

meteo_vars <- read_parquet(paste0("/n/dominici_nsaph_l3/Lab/data/",
"data_warehouse/dw_dorieh_climate/data/county/year=2018/part-0.parquet"))

county_backbone <- read_rds(here("data", "cotus_county_shp_w_fips.RDS"))

# Select cols, write ------------------------------------------------------

meteo_vars <- meteo_vars %>%
  select(state, five_digit_fips = county, observation_date, tmmn, tmmx, pr, vs)

meteo_vars <-
  meteo_vars %>% 
  filter(five_digit_fips %in% county_backbone$five_digit_fips) %>%
  rename(
    min_temp = tmmn,
    max_temp = tmmx,
    precip = pr,
    wind_speed = vs
  )

write_parquet(meteo_vars, here('meteo_vars.parquet'))
