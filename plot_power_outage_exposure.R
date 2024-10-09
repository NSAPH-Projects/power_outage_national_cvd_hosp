
# Libraries ---------------------------------------------------------------

pacman::p_load(here, tidyverse, data.table, fst, sf, ggthemes)


# Read --------------------------------------------------------------------

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

# reproject
epsg_code <- 5070
us_counties <- st_transform(us_counties, crs = epsg_code)


# Plot --------------------------------------------------------------------

under_layer <- st_geometry(us_counties)
dif_exposure <- colnames(us_counties)[5:16]

pdf(here("figures", "power_outage_rates_2018.pdf"))
for (exp_col in dif_exposure){
  
p1 <- us_counties |>
  #filter(p_present > 0.5) |>
  ggplot() +
  geom_sf(data = under_layer) +
  geom_sf(aes(fill = !!sym(exp_col))) +
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