# Load and clean the ACS data that we'll use as poverty measures.

# Author: Heather
# Date: Nov. 6th, 2024


# Libraires ---------------------------------------------------------------

pacman::p_load(tidyverse, here)

source(
  here(
    'code',
    'data_prep',
    'effect_mod_data_prep',
    'effect_mod_data_cleaning_helpers.R'
  )
)

# Read --------------------------------------------------------------------

fips <- read_rds(here('data_for_upload', 'cotus_county_list_of_fips.RDS'))

# SVI
svi <- read_csv(here('local_data', 'effect_mod_data', 'svi_interactive_map.csv'))
svi <- svi %>% select(five_digit_fips = FIPS, all_themes_ranking = RPL_THEMES)

# poverty
pov <- read_rds(here("local_data", "effect_mod_data", 'pov_measures.RDS'))

# rurality
nchs_ur <-
  read_csv(here("local_data", "effect_mod_data", "NCHSurb-rural-codes.csv"))

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
  urban = case_when(CODE2023 %in% c(1, 2, 3, 4) ~ 1, TRUE ~ 0)
)

nchs_ur <- nchs_ur %>% select(five_digit_fips, urban)


# Combine -----------------------------------------------------------------

fips <- fips %>% left_join(svi) %>% left_join(pov) %>% left_join(nchs_ur)

# Climate zones:
cz <- read_csv(here('local_data', 'effect_mod_data', 'climate_zones.csv'))
cz <- cz %>% mutate(state_fips = str_pad(Fips, width = 2, side = 'left', pad = '0'))
cz <- cz %>% select(state_fips, climate_zone = `climate zone`)

fips <- fips %>% mutate(state_fips = substr(five_digit_fips, start = 1, stop = 2)) %>% left_join(cz)

# Write -------------------------------------------------------------------

write_rds(
  fips,
  here(
    'data_for_upload',
    'effect_mod_data',
    'included_excluded_county_chars.RDS'
  )
)
