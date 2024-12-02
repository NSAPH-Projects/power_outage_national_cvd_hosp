# Plot CVD and respiratory hospitalization rates by county for 2018. etc.
# Heather, September 20th, 2024

# This runs on the FASSE cluster with the associated data there. 

# Libraries ---------------------------------------------------------------

pacman::p_load(here, tidyverse, sf, ggthemes, viridis, data.table, patchwork)

# Read --------------------------------------------------------------------

# medicare
hosp <- readRDS(here("data", "an_dat_urgent_hosp_nov_5.RDS"))

# us counties
us_counties <- readRDS(here("data", "counties_sf.RDS"))


# Plot --------------------------------------------------------------------

to_plot <- hosp %>%
  group_by(five_digit_fips) %>%
  summarize(
    n_benes = max(n_benes, na.rm = T),
    n_cvd = sum(n_cvd_no_hem_no_hyp, na.rm = T),
    n_resp = sum(n_resp, na.rm = T),
    n_po = sum(exposed_8_hrs_0.01, na.rm = T)
  ) %>%
  mutate(cvd_rate = n_cvd / n_benes * 100000,
         resp_rate = n_resp / n_benes * 100000)

to_plot <- us_counties %>%
  mutate(five_digit_fips = county_fips) %>%
  left_join(to_plot)

to_plot <- to_plot %>% filter(state_abbv != 'HI' & state_abbv != 'AK')

p1 <-
  to_plot |>
  ggplot() +
  geom_sf(aes(fill = cvd_rate), color = NA) +
  scale_fill_viridis_c(name = "Hospitalization rate") +
  theme_map() +
  ggtitle(
    paste0(
    "2018 CVD hospitalization rate per 100,000\n",
    "Medicare part A and B beneficiaries, not\n",
    "including hemorrhagic stroke or hypertension\n",
    "hospitalizations"
  )) +
  theme(legend.position = 'top')

# ggsave(
#   plot = p1,
#   here("figures", 'figures_output', "cvd_hosp_rate_2018_nov_25.pdf"),
#   width = 7,
#   height = 7
# ) 

p2 <-
  to_plot |>
  ggplot() +
  geom_sf(aes(fill = resp_rate), color = NA) +
  scale_fill_viridis_c(
    name = "Hospitalization rate",
    limits = c(0, 1000),
    oob = scales::squish,
    labels = c("0", "250", "500", "750", ">=1000")
  ) +
  theme_map() +
  ggtitle(
   paste0("2018 respiratory hospitalization rate\n",
          "per 100,000 Medicare part A and B\n",
          "beneficiaries"
  )) +
  theme(legend.position = 'top')

# ggsave(
#   plot = p2,
#   here(
#     "figures",
#     'figures_output',
#     "resp_hosp_rate_2018_nov_25.pdf"
#   ),
#   width = 7,
#   height = 7
# )


p3 <-
  to_plot |>
  ggplot() +
  geom_sf(aes(fill = n_po), color = NA) +
  scale_fill_viridis_c(
    name = "Number of days affected\nby 8+ hour power outage",
    limits = c(0, 100),
    oob = scales::squish,
    labels = c("0", '25', '50', '75', ">=100")
  ) +
  theme_map() +
  ggtitle(
    "Number of days in 2018 affected by\n8+ hour power outages"
  ) +
  theme(legend.position = 'top')

# ggsave(
#   plot = p3,
#   here("figures", 'figures_output', "po_count_2018_nov_25.pdf"),
#   width = 7,
#   height = 7
# )

combined_plot <- p1 + p2 + p3 + plot_layout(ncol = 3) & 
  theme(
    plot.title = element_text(size = 20),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 15)
  ) &
  theme(legend.key.size = unit(2.5, "lines"))

ggsave(plot = combined_plot,
  filename = here(
    'figures_for_upload',
    'combined_cvd_resp_po_rates.pdf'
  ),
  width = 21,
  height = 21
)
