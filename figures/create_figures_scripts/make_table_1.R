# Make a table 1


# Libraries ---------------------------------------------------------------

pacman::p_load(tidyverse, data.table, here)

source(here("code", "run_models", "run_models_helper_functions.R"))


# Read --------------------------------------------------------------------

an_dat <- read_rds(here('data', 'an_dat_urgent_hosp_nov_5.RDS'))

dme_by_county <-
  read_rds(here(
    "data_for_upload",
    "effect_mod_data",
    "dme_use_by_county.RDS"
  ))

pov_by_county <-
  read_rds(here(
    "data_for_upload",
    "effect_mod_data",
    "pov_measures.RDS"
  ))

an_dat <- an_dat %>%
  left_join(dme_by_county) %>%
  left_join(pov_by_county)


