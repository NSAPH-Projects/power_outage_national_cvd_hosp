# Add wildfire smoke concentrations to the dataset to control for as well. 


# Libraries ---------------------------------------------------------------

pacman::p_load(tidyverse, here, arrow)


po_dat <- read_parquet(
  here(
    "data_for_upload",
    "power_outage_exposure_data_cleaning_output",
    "analytic_exposure_hot_cold_urban_rural.parquet"
  )
)

wf_smoke <- read_csv(
  here(
    'local_data',
    'effect_mod_data',
    'smokePM2pt5_predictions_daily_county_20060101-20231231.csv'
  )
)

wf_smoke <- wf_smoke %>% 
  mutate(day = ymd(date)) %>%
  filter(lubridate::year(day) == 2018) %>%
  rename(five_digit_fips = GEOID)



po_dat <- po_dat %>% left_join(wf_smoke) %>%
  mutate(smoke = replace_na(smokePM_pred, 0)) %>%
  mutate(any_smoke = ifelse(smoke > 0, 1, 0))


write_rds(po_dat,
  here(
    "data_for_upload",
    "power_outage_exposure_data_cleaning_output",
    "analytic_exposure_hot_cold_urban_rural_smoke.parquet"
  )
)