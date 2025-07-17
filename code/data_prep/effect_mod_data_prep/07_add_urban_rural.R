# Add urban/rural classifications to the dataset

# Libraries ---------------------------------------------------------------

pacman::p_load(fst, tidyverse, here, arrow, stringr)


# Read --------------------------------------------------------------------

po <- read_rds(here("data_for_upload", 'hot_and_cold_pos.RDS'))

nchs_ur <- 
  read_csv(here("local_data", "effect_mod_data", "NCHSurb-rural-codes.csv"))


# Do ----------------------------------------------------------------------

nchs_ur <- nchs_ur %>% mutate(
  CTYFIPS = str_pad(
    CTYFIPS,
    width = 3,
    side = 'left',
    pad = '0'
  ),
  STFIPS = str_pad(
    STFIPS,
    width = 2,
    side = 'left',
    pad = '0'
  ),
  five_digit_fips = paste0(STFIPS, CTYFIPS),
  urban = case_when(CODE2023 %in% c(1, 2, 3, 4) ~ 1, 
                    TRUE ~ 0)
)

nchs_ur <- nchs_ur %>% select(five_digit_fips, urban)

po <- po %>% left_join(nchs_ur)

write_parquet(po,
  here(
    "data_for_upload",
    "power_outage_exposure_data_cleaning_output",
    "analytic_exposure_hot_cold_urban_rural.parquet"
  )
)
