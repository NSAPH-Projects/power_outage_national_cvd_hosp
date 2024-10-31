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

# add four category wind speed - by decile based on plot
meteo_vars <-
  meteo_vars %>%
  mutate(p_wind_speed = ntile(wind_speed, 10)) %>%
  mutate(
    four_cat_wind_speed = case_when(
      p_wind_speed <= 7 ~ 1,
      p_wind_speed == 8 ~ 2,
      p_wind_speed == 9 ~ 3,
      p_wind_speed == 10 ~ 4
    )
  )

write_parquet(meteo_vars, here('meteo_vars.parquet'))
