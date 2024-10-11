# This script plots the number of hours without power by county in the US

# Author: Heather
# Last updated: Oct 9th, 2024

# Runs on Heather's computer

# Libraries ---------------------------------------------------------------

pacman::p_load(here, tidyverse, data.table, fst, sf, ggthemes)

# Read --------------------------------------------------------------------

# denoms
denoms <- read_fst(
  here(
    'data',
    'power_outage_exposure_data_cleaning_output',
    'county_customer_denoms_and_p_missing.fst'
  )
)

# us counties
us_counties <- readRDS(here("data", "cotus_county_shp_w_fips.RDS"))

# exposure data tabulating the number of hours without power by county for
# different cut points of % of people out
daily_hrs_out <-
  read_fst(here(
    'data',
    'power_outage_exposure_data_cleaning_output',
    "daily_hrs_out.fst"
  ))

# Plot --------------------------------------------------------------------

# calculate total hrs out over the year 2018
total_hrs_out <- daily_hrs_out %>%
  mutate(year = lubridate::year(day)) %>%
  group_by(five_digit_fips, year) %>%
  filter(year == 2018) %>%
  summarize(across(starts_with("n_hrs"), sum, na.rm = TRUE))

# take out some counties in texas with outlying values bc of tiny populations
total_hrs_out <- 
  total_hrs_out %>%
  filter(five_digit_fips != 48173 &
           five_digit_fips != 48301)

us_counties <- us_counties %>% left_join(total_hrs_out)

cols_outage <-
  c("n_hrs_out_0.005",
    "n_hrs_out_0.01",
    "n_hrs_out_0.03",
    "n_hrs_out_0.05")

pdf(here("figures", "figures_output", "power_outage_total_hrs_out_2018.pdf"),
    width = 14,
    height = 12,
    compress = T)


for (out_col in cols_outage) {
  p1 <- us_counties |>
    #filter(p_present > 0.5) |>
    ggplot() +
    #geom_sf(data = under_layer) +
    geom_sf(aes(fill = !!sym(out_col)), color = NA) +
    scale_fill_viridis_c(name = "Number of power outages in 2018", ) +
    theme_map() +
    ggtitle(paste0("Number of power outages in 2018 for ", out_col))
  
  print(p1)
}

dev.off()

# exclude low coverage and low pop counties
us_counties <- us_counties %>% left_join(denoms)

# plot with only counties we have data for 
# set these to NA when person coverage < 50%
columns_to_na <- cols_outage

us_counties <- us_counties %>%
  mutate(across(
    all_of(columns_to_na),
    ~ if_else(percent_served < 0.5, NA_real_, .)
  ))

# also exclude counties w < 500 customers 
us_counties <- us_counties %>%
  mutate(across(
    all_of(columns_to_na),
    ~ if_else(county_customers < 500, NA_real_, .)
  ))

# replot 
pdf(
  here(
    "figures",
    "figures_output",
    "power_outage_total_hrs_out_2018_without_low_cov.pdf"
  ),
  width = 14,
  height = 12,
  compress = T
)

for (out_col in cols_outage) {
  p1 <- us_counties |>
    #filter(p_present > 0.5) |>
    ggplot() +
    #geom_sf(data = under_layer) +
    geom_sf(aes(fill = !!sym(out_col)), color = NA) +
    scale_fill_viridis_c(name = 
"Number of power outages in 2018 among counties covering >50% customers", ) +
    theme_map() +
    ggtitle(paste0("Number of power outages in 2018 for ", out_col))
  
  print(p1)
}

dev.off()

