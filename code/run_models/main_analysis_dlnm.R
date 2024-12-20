# Do main analysis with constrained lag terms 

# Libraries ---------------------------------------------------------------

pacman::p_load(tidyverse, data.table, here, arrow, gnm, splines,
               dlnm)

source(here("code", "run_models", "run_models_helper_functions.R"))


# Read --------------------------------------------------------------------

an_dat <- read_rds(here('data', 'an_dat_urgent_hosp_dec_17.RDS'))

# Models ------------------------------------------------------------------

# want to do cvd and respiratory for all lags and all cut points but not for 
# durations 
exposure_columns <-
  c('exposed_8_hrs_0.01',
    'exposed_8_hrs_0.03',
    'exposed_8_hrs_0.05')

cvd_dlnms <- run_dlnm_models(
  po_data = an_dat,
  outcome_col = 'n_cvd_no_hyp',
  exposure_cols = exposure_columns,
  offset_col = 'n_benes',
  precip_dfs = 2,
  po_dfs = 4
)

resp_dlnms <- run_dlnm_linear_precip(
  po_data = an_dat,
  outcome_col = 'n_resp',
  exposure_cols = exposure_columns,
  offset_col = 'n_benes',
  po_dfs = 3
)

# extract results 
cvd_dlnm_preds <- rbindlist(get_dlnm_pred(cvd_dlnms))
resp_dlnm_preds <- rbindlist(get_dlnm_pred(resp_dlnms))

# add outcome type labels
cvd_dlnm_preds <- cvd_dlnm_preds %>%
  mutate(outcome_type = 'Cardiovascular-related hosp, excluding hypertension')
resp_dlnm_preds <- resp_dlnm_preds %>%
  mutate(outcome_type = 'Respiratory-related hosp')

# combine results
all_results <- rbindlist(list(cvd_dlnm_preds, resp_dlnm_preds))

# rename m_name values
all_results <- all_results %>% 
  mutate(m_name = recode(m_name, 
                         'exposed_8_hrs_0.01' = '1%',
                         'exposed_8_hrs_0.03' = '3%',
                         'exposed_8_hrs_0.05' = '5%'))
# plot 
dlnm_main_analysis_plot <- 
  all_results %>%
  ggplot() +
  geom_hline(aes(yintercept = 1)) +
  geom_point(
    size = 3.5, 
    aes(x = lags, y = est, color = m_name),
    position = position_dodge(width = 0.5)) +
  geom_errorbar(
    width = 0.5,
    size = 1,
    position = position_dodge(width = 0.5),
    aes(x = lags, ymin = ci_low, ymax = ci_high, color = m_name)) +
  facet_grid( ~ outcome_type) +
  theme_minimal(base_size = 15) +
  labs(
    x = "Lag (days)", 
    y = "Rate ratio", 
    color = "Size of power outage:\n X% of county out or more") + 
  ggtitle(paste0("Association between power outage exposure and ",
  "hospitalizations\nin older adults (age 65+) in fee-for-service Medicare")) +
  theme(
    panel.spacing = unit(1, "lines"),
    panel.border = element_rect(
      color = "grey",
      fill = NA,
      size = 1)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_color_brewer(palette = "Set1")


ggsave(
  dlnm_main_analysis_plot,
  filename = here(
    'figures_for_upload',
    'main_analysis_dlnm_dec_17.pdf'
  ),
  width = 14,
  height = 7
)

# create table 

tables <- all_results %>% 
  mutate(est = round(est, digits = 3),
         ci_low = round(ci_low, digits = 3),
         ci_high = round(ci_high), digits = 3) %>%
  mutate(est_w_ci = paste0(est, ', [', ci_low, ', ', ci_high, ']')) %>%
  select(m_name, lags, outcome_type, est_w_ci) %>%
  pivot_wider(names_from = lags,
              values_from = c('est_w_ci'),names_prefix = 'Lag day ')

write_csv(tables,
          here("figures_for_upload", "supplement", "main_results_table.csv"))
