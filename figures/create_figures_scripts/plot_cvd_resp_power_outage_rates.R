# Plot CVD and respiratory hospitalization rates by county for 2018. etc.
# Heather, September 20th, 2024

# This runs on the FASSE cluster with the associated data there. 

# Libraries ---------------------------------------------------------------

pacman::p_load(here, tidyverse, sf, ggthemes, viridis, data.table, patchwork)

# Read --------------------------------------------------------------------

# medicare
hosp <- readRDS(here("data", "an_dat_urgent_hosp_jan_28.RDS"))

# us counties
us_counties <- readRDS(here("data", "cotus_county_shp_w_fips.RDS"))


# Plot --------------------------------------------------------------------

to_plot <- hosp %>%
  group_by(five_digit_fips) %>%
  summarize(
    n_benes = max(n_benes, na.rm = T),
    n_cvd = sum(n_all_cvd, na.rm = T),
    n_resp = sum(n_resp, na.rm = T),
    n_po = sum(exposed_8_hrs_0.01, na.rm = T)
  ) %>%
  mutate(cvd_rate = n_cvd / n_benes * 10000,
         resp_rate = n_resp / n_benes * 10000)

to_plot <- us_counties %>%
  left_join(to_plot)

to_plot <- to_plot %>% filter(state_fips != '15' & state_fips != '02')

mean(to_plot$cvd_rate, na.rm = T)
mean(to_plot$resp_rate, na.rm = T)


p1 <-
  to_plot |>
  ggplot() +
  geom_sf(aes(fill = cvd_rate), color = NA) +
  scale_fill_viridis_c(name = paste0("CVD\n", "hospitalization rate")) +
  theme_map() +
  ggtitle(
    paste0(
    "2018 annual CVD hospitalization rate per 10,000\n",
    "Medicare Fee-For-Service beneficiaries"
  )) +
  theme(legend.position = 'top',
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 13)) 

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
  scale_fill_viridis_c(name = paste0("Respiratory\n", "hospitalization rate")) +
  theme_map() +
  ggtitle(
    paste0(
      "2018 annual respiratory hospitalization rate\n",
      "per 10,000 Medicare Fee-For-Service beneficiaries\n"
    )
  ) +
  theme(legend.position = 'top',
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 13)) 

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


to_plot <- to_plot %>%
  mutate(n_po_cat = cut(
    n_po,
    breaks = c(-Inf, 5, 15, 30, 50, Inf),
    labels = c("0-5", "6-15", "16-30", "31-50", "51+")
  ))

grey_color <- "#999999"  #grey color

p3 <-
  to_plot |>
  ggplot() +
  geom_sf(aes(fill = n_po_cat), color = NA) +
  scale_fill_manual(
    name = "N power outage days",
    values = c("0-5" = viridis::viridis(5)[1],
               "6-15" = viridis::viridis(5)[2],
               "16-30" = viridis::viridis(5)[3],
               "31-50" = viridis::viridis(5)[4],
               "51+" = viridis::viridis(5)[5]),
    na.value = grey_color  # Set the grey color for NA values
  ) +
  theme_map() +
  ggtitle(
    "Number of county-days in 2018 affected by\n8+ hour power outages"
  ) +
  theme(legend.position = 'top',
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 13))

 p3 <-
   to_plot |>
   ggplot() +
   geom_sf(aes(fill = n_po), color = NA) +
   scale_fill_viridis_c(
     name = "Number of days affected\nby 8+ hour power outage",
     limits = c(0, 100),
     oob = scales::squish,
     labels = c("0", '25', '50', '75', "≥100")
   ) +
   theme_map() +
   ggtitle(
     "Number of county-days in 2018 affected by\n8+ hour power outages"
   ) +
   theme(legend.position = 'top',
         legend.text = element_text(size = 13),
         legend.title = element_text(size = 13))

 ggsave(
   plot = p3,
   here("figures", 'figures_output', "po_count_2018_nov_25.pdf"),
   width = 7,
   height = 7
 )

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

ggsave(plot = p3,
       filename = here(
         'figures_for_upload',
         'PO.pdf'
       ),
       width = 7,
       height = 7
)

