# Plot gridmet data already cleaned by NSAPH

# Libraries ---------------------------------------------------------------

pacman::p_load(arrow, tidyverse, here, sf, ggthemes)

# Read --------------------------------------------------------------------

meteo_vars <- read_rds(here('data', 'meteo_vars.RDS'))

county_backbone <- read_rds(here("data", "cotus_county_shp_w_fips.RDS"))

# Plot real quick ---------------------------------------------------------

meteo_vars <- meteo_vars %>%
  group_by(five_digit_fips) %>%
  summarize(
    mean_min_temp = mean(min_temp),
    mean_max_temp = mean(max_temp),
    max_precip = max(precip),
    max_wind = max(wind_speed)
  )


county_backbone <- county_backbone %>% left_join(meteo_vars)

pdf(here("figures", "figures_output", "weather_vars.pdf"))

p1 <- county_backbone |>
  ggplot() +
  geom_sf(aes(fill = mean_min_temp, color = NA)) +
  scale_fill_viridis_c(name = "Mean minimum temperature gridmet for 2018") +
  theme_map() +
  ggtitle(paste0('Mean minimum temp'))

ggsave(p1, here('figures', 'figures_output'))

print(p1)

p2 <- county_backbone |>
  ggplot() +
  geom_sf(aes(fill = mean_max_temp, color = NA)) +
  scale_fill_viridis_c(name = "Mean maximum temperature gridmet for 2018") +
  theme_map() +
  ggtitle(paste0('Mean maximum temp'))

print(p2)

p3 <- county_backbone |>
  ggplot() +
  geom_sf(aes(fill = max_precip, color = NA)) +
  scale_fill_viridis_c(name = "Maximum daily precipitation gridmet for 2018") +
  theme_map() +
  ggtitle(paste0('Max precip'))

print(p3)

p3 <- county_backbone |>
  ggplot() +
  geom_sf(aes(fill = max_precip, color = NA)) +
  scale_fill_viridis_c(name = "Maximum wind speed gridmet for 2018") +
  theme_map() +
  ggtitle(paste0('Max wind speed'))

print(p4)

dev.off()
