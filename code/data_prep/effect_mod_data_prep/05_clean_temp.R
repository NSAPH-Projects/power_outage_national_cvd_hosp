# This file finds days that are extremely hot or cold, using PRISM temp
# data from 1981-2010 to form the 'norm' and then looks for hot or cold days
# in 2018-2020. 
# This script uses both a threshold and an absolute cutoff to identify those 
# days

# Author: Heather, adapted from Alex and Brittany's code
# Date: Nov 6th, 2024

pacman::p_load(here, tidyverse, data.table, fst)

# Read in temp data -------------------------------------------------------

temp_data <-
  rbindlist(lapply(FUN = read_rds, X = list.files(
    here("local_data", "effect_mod_data", "temp_data"),
    full.names = T
  )))

# Do ----------------------------------------------------------------------

# get dates in correct class, and make a week number, but also if the year has 
# more than 52 weeks just include those days in the 52nd week 
temp_data  <- temp_data  %>%
  select(date, five_digit_fips = fips, tmean) %>%
  mutate(date = lubridate::dmy(date),
         week_num = lubridate::epiweek(date)) %>%
  mutate(week_num = ifelse(week_num == 53, 52, week_num))

# split into years we use for establishing baseline, and years we want to ID 
temp_data_1981_2010 <- 
  temp_data %>%
  filter(date > "1980-12-30" & date < "2011-01-01")

temp_data_2018_2020 <- 
  temp_data %>%
  filter(date > "2017-12-30") %>%
  select(five_digit_fips, date, week_num, tmean)

# group the baseline years by week and county, and calculate 85th and 15th 
# percentiles using weekly long-term average to establish cutoffs for anom hot 
# and cold days by county and week 

percentiles <- temp_data_1981_2010 %>% 
  group_by(five_digit_fips, week_num) %>%
  summarise(hot_85th = quantile(tmean, probs = 0.85), 
            cold_15th = quantile(tmean, probs = 0.15)) 


# group the years we want to id by week and county, and find days that were 
# over the threshold temps (over a threshold (>24°C) and under (<0°C))
# note from alex: changed this threshold to account for power lines: 
# https://ieeexplore.ieee.org/abstract/document/9123900
# since using tmean, this likely includes freezing temperature during some 
# point of the day 

days_over_thresholds <- temp_data_2018_2020 %>%
  mutate(over_24 = ifelse(tmean > 24, 1, 0),
         under_0 = ifelse(tmean < 0, 1, 0))

# join threshold info to days in 2018-2020
days_over_thresholds <- 
  days_over_thresholds %>%
  left_join(percentiles)

# make a new col to see which days meet both criteria to be anom hot or cold
days_over_thresholds <- days_over_thresholds %>% 
  mutate(anomhot = ifelse(over_24 == 1 & tmean >= hot_85th, 1, 0),
         anomcold = ifelse(under_0 == 1 & tmean <= cold_15th, 1,0))

# Write -------------------------------------------------------------------

write_fst(
  days_over_thresholds,
  here(
    'data_for_upload',
    'effect_mod_data',
    'hot_and_cold_days.fst'
  )
)
