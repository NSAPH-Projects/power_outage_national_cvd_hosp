# Make a dataset of power outages only on cold days and power outages only on 
# hot days 

# Libraries ---------------------------------------------------------------

pacman::p_load(fst, tidyverse, here, arrow)


# Read --------------------------------------------------------------------

temp_dat <- read_fst(here(
  'data_for_upload',
  'effect_mod_data',
  'hot_and_cold_days.fst'
)) %>% 
  filter(lubridate::year(date) == 2018) %>%
  mutate(day = date)


po <- arrow::read_parquet(
  here(
    "data_for_upload",
    "power_outage_exposure_data_cleaning_output",
    "analytic_exposure_data_2018.parquet"
  )
)

po <- po %>% left_join(temp_dat)

po <- po %>% 
  mutate(
  exposed_8_hrs_0.01_hot = ifelse(anomhot == 1, exposed_8_hrs_0.01, 0),
  exposed_8_hrs_0.01_cold = ifelse(anomcold == 1, exposed_8_hrs_0.01, 0),
  exposed_8_hrs_0.03_hot = ifelse(anomhot == 1, exposed_8_hrs_0.03, 0),
  exposed_8_hrs_0.03_cold = ifelse(anomcold == 1, exposed_8_hrs_0.03, 0),
  exposed_8_hrs_0.05_hot = ifelse(anomhot == 1, exposed_8_hrs_0.05, 0),
  exposed_8_hrs_0.05_cold = ifelse(anomcold == 1, exposed_8_hrs_0.05, 0)
)

po <- po %>% select(-c(year:anomcold))

write_rds(po, here("data_for_upload", 'hot_and_cold_pos.RDS'))
