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

acs_dat <- read_csv(here(
  'local_data',
  'effect_mod_data',
  'nhgis0011_csv',
  'nhgis0011_ds244_20195_county.csv'
))

# going to calculate total number of people making below poverty line
# as a percentage of total county pop
# calculate quartiles of that
# and then compare 1 to 4th quartile 

acs_dat <- acs_dat %>% 
  mutate(total_below_pov = ALWVE002 + ALWVE003,
         p_below_pov = total_below_pov / ALWVE001,
         five_digit_fips = paste0(STATEA, COUNTYA)) %>%
  select(five_digit_fips, total_below_pov, p_below_pov) 

acs_dat <- 
  acs_dat %>%
  mutate(pov_quartile = find_quartiles(p_below_pov))

acs_dat <- acs_dat %>% filter(five_digit_fips %in% fips$five_digit_fips)
  
write_rds(acs_dat,
          here('local_data', 'effect_mod_data', 'pov_measures.RDS'))
