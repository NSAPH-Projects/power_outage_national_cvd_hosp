
pacman::p_load(arrow, data.table, tidyverse, here, fst)

outages <- read_parquet(
  here(
    'data',
    'power_outage_exposure_data_cleaning_output',
    'analytic_exposure_data_all_years.parquet'
  )
)

denoms <- read_fst(
  here(
    'data',
    'power_outage_exposure_data_cleaning_output',
    "county_customer_denoms_and_p_missing.fst"
  )
)

outages <- outages %>% left_join(denoms)

outages <- outages %>% filter(percent_served > 0.5 & county_customers > 1000)

p1 <-
  outages %>%
  ggplot() +
  geom_point(aes(x = day, y = n_hrs_out_0.01), alpha = 0.01, size = 5)

ggsave(p1, filename= here('figures', 'figures_output', 'outages_by_day.png'))

p2 <- outages %>%
  mutate(month = lubridate::month(day)) %>%
  group_by(five_digit_fips, year, month) %>% 
  summarize(mean_outages = mean(exposed_8_hrs_0.005))

p2 <- p2 %>%
  ggplot() +
  geom_point(aes(x = month, y = mean_outages)) + facet_grid( ~ year)

ggsave(p2,
    filename = here('figures', 'figures_output', 'mean_outages_by_month.png'))

  
