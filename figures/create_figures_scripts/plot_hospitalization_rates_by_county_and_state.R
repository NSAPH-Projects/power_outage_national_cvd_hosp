# Plot CVD and respiratory hospitalization rates by county for 2018. etc.
# Heather, September 20th, 2024

# This runs on the FASSE cluster with the associated data there. 

# Libraries ---------------------------------------------------------------

pacman::p_load(here, tidyverse, sf, ggthemes, viridis, data.table)

# Read --------------------------------------------------------------------

# medicare
hosp <- readRDS(here("data", "an_dat_urgent_hosp_nov_5.RDS"))

# us counties
us_counties <- readRDS(here("data", "counties_sf.RDS"))


# Plot --------------------------------------------------------------------

k <- us_counties %>%
  mutate(five_digit_fips = county_fips) %>%
  left_join(hosp) %>%
  left_join(benes)

k <- k %>%
  mutate(cvd_rate = n_cvd_no_hem_no_hyp / n_benes * 100000,
         resp_rate = n_resp / n_benes * 100000) %>%
  select(five_digit_fips, cvd_rate, resp_rate) %>%
  distinct()

p1 <-
  k |>
  ggplot() +
  geom_sf(aes(fill = cvd_rate), color = NA) +
  scale_fill_viridis_c(name = "Hospitalization rate") +
  theme_map() +
  ggtitle("2018 CVD hospitalization rate per 100,000 among medicare
beneficiaries, not including hem stroke or hypertension related
hospitalizations")

ggsave(p1,
       here("figures", 'figures_output', "cvd_hosp_ate_2018_nov_25.pdf"))

p1 <-
  k |>
  ggplot() +
  geom_sf(aes(fill = resp_rate), color = NA) +
  scale_fill_viridis_c(name = "Hospitalization rate") +
  theme_map() +
  ggtitle("2018 respiratory-related hospitalization rate per 100,000 among 
part A and B medicare beneficiaries")

ggsave(p1,
       here("figures", 'figures_output', "resp_hosp_ate_2018_nov_25.pdf"))

# Clean -------------------------------------------------------------------

# clean us county data to conus only
# define the FIPS and letter codes for Alaska, Hawaii, and other territories
excluded_states <- #c("02", "15", 
                     c("60", "66", "69", "72", "78")
excluded_states_letters <- #c("AK", "HI", 
                             c("AS", "GU", "MP", "PR", "VI")

# clean slightly
conus_counties <- us_counties |>
  rename(county = county_fips) |>
  filter(!(state_fips %in% excluded_states))

# summarize county daily hospitalizations to state
# state_hosp <- hosp |>
#   group_by(state, county) |>
#   summarize(
#     n_cvd_no_hem_no_hyp = sum(n_cvd_no_hem_no_hyp),
#     n_resp = sum(n_resp)
#   )

# filter hospitalization counts and beneficiaries to conus
state_hosp <- state_hosp |>
  filter(!(state %in% excluded_states_letters))
benes <- benes |>
  filter(!(state %in% excluded_states_letters))

# join
plot_hosp_county <-
  conus_counties |>
  left_join(state_hosp) |>
  left_join(benes)

# calculate relevant rates
plot_hosp_county <- plot_hosp_county |>
  mutate(
    cvd_hosp_rate = n_cvd / n_benes * 100000,
    resp_hosp_rate = n_resp / n_benes * 100000,
    all_cvd_hosp_rate = all_cvd / n_benes * 100000,
    stroke_hosp_rate = n_stroke / n_benes * 100000
  )

# Plot and save -----------------------------------------------------------

# inspired by lauren at hack week to put them all in a pdf
# open a PDF device
pdf(here("figures", "cvd_resp_hosp_rate_2018_plots_by_county.pdf"))

p1 <-
  plot_hosp_county |>
  ggplot() +
  geom_sf(aes(fill = cvd_hosp_rate), color = NA) +
  scale_fill_viridis_c(name = "Hospitalization rate") +
  theme_map() +
  ggtitle("2018 CVD hospitalization rate per 100,000 among medicare
beneficiaries, not including hem stroke or hypertension related
hospitalizations")

print(p1)

p2 <-
  plot_hosp_county |>
  ggplot() +
  geom_sf(aes(fill = resp_hosp_rate), color = NA) +
  scale_fill_viridis_c(name = "Hospitalization rate") +
  theme_map() +
  ggtitle("2018 Respiratory disease-related hospitalization rate per 100,000
among medicare beneficiaries")

print(p2)

p3 <-
  plot_hosp_county |>
  filter(county != '51685') |> # one county with 8 people was screwing up scale
  ggplot() +
  geom_sf(aes(fill = all_cvd_hosp_rate), color = NA) +
  scale_fill_viridis_c(name = "Hospitalization rate") +
  theme_map() +
  ggtitle("2018 CVD disease-related hospitalization rate per 100,000
among medicare beneficiaries, including all CVD except hypertension")

print(p3)

p4 <- plot_hosp_county |>
  filter(county != '51685') |>
  ggplot() +
  geom_sf(aes(fill = stroke_hosp_rate), color = NA) +
  scale_fill_viridis_c(name = "Hospitalization rate") +
  theme_map() +
  ggtitle("2018 stroke-related hospitalization rate per 100,000
among medicare beneficiaries")

print(p4)

dev.off()

# make plots again excluding counties with less than 100 people

# exclude
plot_hosp_county <-
  plot_hosp_county |> mutate(
    cvd_hosp_rate = case_when(n_benes < 400 ~ NA_real_,
                              T ~ cvd_hosp_rate),
    resp_hosp_rate = case_when(n_benes < 400 ~ NA_real_,
                               T ~ resp_hosp_rate),
    all_cvd_hosp_rate = case_when(n_benes < 400 ~ NA_real_,
                                  T ~ all_cvd_hosp_rate),
    stroke_hosp_rate = case_when(n_benes < 400 ~ NA_real_,
                                 T ~ stroke_hosp_rate)
  )

# open PDF device
pdf(here("figures", "cvd_resp_hosp_rate_2018_plots_by_county_with_excl.pdf"))

p1 <-
  plot_hosp_county |>
  ggplot() +
  geom_sf(aes(fill = cvd_hosp_rate), color = NA) +
  scale_fill_viridis_c(name = "Hospitalization rate") +
  theme_map() +
  ggtitle("2018 CVD hospitalization rate per 100,000 among medicare
beneficiaries, not including hem stroke or hypertension related
hospitalizations")

print(p1)

p2 <-
  plot_hosp_county |>
  ggplot() +
  geom_sf(aes(fill = resp_hosp_rate), color = NA) +
  scale_fill_viridis_c(name = "Hospitalization rate") +
  theme_map() +
  ggtitle("2018 Respiratory disease-related hospitalization rate per 100,000
among medicare beneficiaries")

print(p2)

p3 <-
  plot_hosp_county |>
  filter(county != '51685') |> # one county with 8 people was screwing up scale
  ggplot() +
  geom_sf(aes(fill = all_cvd_hosp_rate), color = NA) +
  scale_fill_viridis_c(name = "Hospitalization rate") +
  theme_map() +
  ggtitle("2018 CVD disease-related hospitalization rate per 100,000
among medicare beneficiaries, including all CVD except hypertension")

print(p3)

p4 <- plot_hosp_county |>
  filter(county != '51685') |>
  ggplot() +
  geom_sf(aes(fill = stroke_hosp_rate), color = NA) +
  scale_fill_viridis_c(name = "Hospitalization rate") +
  theme_map() +
  ggtitle("2018 stroke-related hospitalization rate per 100,000
among medicare beneficiaries")

print(p4)

dev.off()

