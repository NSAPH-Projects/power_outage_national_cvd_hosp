# Plot the number of hours without power throughout 2018 and also the number of
# discrete power outages based on the definition we've used 

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

# exposure data based on binary definition at different durations and cut
# points
all_days <- read_fst(
  here(
    "data",
    "power_outage_exposure_data_cleaning_output",
    "all_days_exposed_unexposed.fst"
  )
)

all_days <- all_days %>%
  filter(lubridate::year(day) == 2018) %>%
  group_by(five_digit_fips) %>%
  summarise(across(starts_with("exposed"), sum, na.rm = TRUE))

# us counties
us_counties <- readRDS(here("data", "cotus_county_shp_w_fips.RDS"))
us_counties <- us_counties |> left_join(all_days)
us_counties <- us_counties |> left_join(denoms)

# Plot --------------------------------------------------------------------

under_layer <- st_geometry(us_counties)
dif_exposure <- colnames(us_counties)[5:16]

pdf(
  here("figures", "figures_output", "power_outage_rates_2018.pdf"),
  width = 14,
  height = 12,
  compress = T
)

for (exp_col in dif_exposure){
  
p1 <- us_counties |>
  #filter(p_present > 0.5) |>
  ggplot() +
  #geom_sf(data = under_layer) +
  geom_sf(aes(fill = !!sym(exp_col)), color = NA) +
  scale_fill_viridis_c(
    name = "Number of power outages in 2018",
  ) +
  theme_map() +
  ggtitle(
    paste0("Number of power outages in 2018 for ", exp_col)
  )

print(p1)
}
dev.off()

# plot with only counties we have data for 
# set these to NA when person coverage < 50%
columns_to_na <- colnames(us_counties)[5:16]  

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

pdf(
  here(
    "figures",
    "figures_output",
    "power_outage_rates_2018_without_low_cov_counties.pdf"
  ),
  width = 14,
  height = 12,
  compress = T
)

for (exp_col in dif_exposure) {
  p1 <- us_counties |>
    #filter(p_present > 0.5) |>
    ggplot() +
    #geom_sf(data = under_layer) +
    geom_sf(aes(fill = !!sym(exp_col)), color = NA) +
    scale_fill_viridis_c(name = 
"Number of power outages in 2018 among counties with good coverage", ) +
    theme_map() +
    ggtitle(paste0("Number of power outages in 2018 for ", exp_col))
  
  print(p1)
}
dev.off()

