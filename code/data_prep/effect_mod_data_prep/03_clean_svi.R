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

svi <- read_csv(here('local_data', 'effect_mod_data', 'svi_interactive_map.csv'))

svi <- svi %>% select(five_digit_fips = FIPS, all_themes_ranking = RPL_THEMES)

fips <- fips %>% left_join(svi)

fips <- fips %>% filter(!is.na(all_themes_ranking)) %>% filter(all_themes_ranking > 0)

# Write -------------------------------------------------------------------

write_rds(fips, here('data_for_upload', 'effect_mod_data', 'svi.RDS'))
