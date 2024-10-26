# Plot county person-coverage for the POUS dataset in 2018 

# Author: Heather 
# Last updated: Oct 7th, 2024

# This runs on Heather's computer. 

# Libraries ---------------------------------------------------------------

pacman::p_load(here, tidyverse, sf, ggthemes, viridis, data.table, fst)

# Read --------------------------------------------------------------------

# pous
cols_to_read <- c(
  "clean_state_name",
  "clean_county_name",
  "five_digit_fips",
  "year",
  "customers_served_estimate_to_use",
  "p_present"
)

pous <- read_fst(
  here(
    "data",
    "power_outage_exposure_data_cleaning_output",
    "hourly_data_with_coverage_exclusions.fst"
  ),
  columns = cols_to_read
) |> distinct()
 
# us counties
us_counties <- readRDS(here("data", "cotus_county_shp_w_fips.RDS"))
us_counties <- us_counties |> left_join(pous)

# Plot --------------------------------------------------------------------

us_counties <- us_counties %>% mutate(p_present = ifelse(p_present > 1, 1, p_present))

under_layer <- st_geometry(us_counties)

p1 <- us_counties |>
  filter(year == 2018) |>
  #filter(p_present > 0.5) |>
  ggplot() +
  geom_sf(data = under_layer) +
  geom_sf(aes(fill = percent_served)) +
  scale_fill_viridis_c(
    name = "Proportion of person-time not missing in POUS dataset; 
grey areas are missing more than 50% of ten-minute person-periods",
    limits = c(0.5, 1)
  ) +
  theme_map() +
  ggtitle(
    "2018 person-time coverage in the POUS dataset"
  ) 
  

ggsave(
  here('figures', 'figures_output', 'person_time_coverage_hrs_oct_26.pdf'),
  device = "pdf",
  width = 14,
  height = 12
)


# try P missing -----------------------------------------------------------

oo <- read_fst(here("data", "power_outage_exposure_data_cleaning_output", "county_customer_denoms_and_p_missing.fst"))
View(oo)

oo <- oo %>% filter(year == 2018)
us_counties <- us_counties %>% left_join(oo)


