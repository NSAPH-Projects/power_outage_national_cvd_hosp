# Do main analysis with constrained lag terms 

# Libraries ---------------------------------------------------------------

pacman::p_load(tidyverse, data.table, here, arrow, gnm, splines,
               dlnm)

source(here("code", "run_models", "run_models_helper_functions.R"))


# Read --------------------------------------------------------------------

an_dat <- read_rds(here('data', 'an_dat_urgent_hosp_jan_28.RDS'))


urban <- an_dat %>% filter(urban == 1)
rural <- an_dat %>% filter(urban != 1)
# Models ------------------------------------------------------------------

# Want to stratify by urban/rural status 

# Urban first -------------------------------------------------------------
exposure_columns <-
  c('exposed_8_hrs_0.01',
    'exposed_8_hrs_0.03',
    'exposed_8_hrs_0.05')

cvd_dlnms_urban <- run_dlnm_models(
  po_data = urban,
  outcome_col = 'n_all_cvd',
  exposure_cols = exposure_columns,
  offset_col = 'n_benes',
  precip_dfs = 2,
  po_dfs = 4
)

resp_dlnms_urban <- run_dlnm_linear_precip(
  po_data = urban,
  outcome_col = 'n_resp',
  exposure_cols = exposure_columns,
  offset_col = 'n_benes',
  po_dfs = 3
)

# extract results 
cvd_dlnm_preds_urban <- rbindlist(get_dlnm_pred(cvd_dlnms_urban))
resp_dlnm_preds_urban <- rbindlist(get_dlnm_pred(resp_dlnms_urban))

# add outcome type labels
cvd_dlnm_preds_urban <- cvd_dlnm_preds_urban %>%
  mutate(outcome_type = 'Cardiovascular-related hosp')
resp_dlnm_preds_urban <- resp_dlnm_preds_urban %>%
  mutate(outcome_type = 'Respiratory-related hosp')

# combine results
all_results_urban <- rbindlist(list(cvd_dlnm_preds_urban, resp_dlnm_preds_urban))

# rename m_name values
all_results_urban <- all_results_urban %>% 
  mutate(m_name = recode(m_name, 
                         'exposed_8_hrs_0.01' = '1%',
                         'exposed_8_hrs_0.03' = '3%',
                         'exposed_8_hrs_0.05' = '5%'))
# plot 
dlnm_main_analysis_plot <- 
  all_results_urban %>%
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
  theme_minimal(base_size = 17) +
  labs(
    x = "Lag (days)", 
    y = "Rate ratio", 
    color = "Power outage size") + 
  ggtitle(paste0("Association between power outage exposure and ",
                 "hospitalizations\nin older adults (age 65+) in fee-for-service Medicare\n URBAN AREAS ONLY")) +
  theme(
    panel.spacing = unit(1, "lines"),
    panel.border = element_rect(
      color = "grey",
      fill = NA,
      size = 1)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  #scale_color_brewer(palette = "Set1") +
  scale_color_manual(labels = c("1%", "3%", "5%"), 
                     values = c("#482677FF", "#2D708EFF", "#29AF7FFF")) +
  theme(strip.text = element_text(size = 17))


ggsave(
  dlnm_main_analysis_plot,
  filename = here(
    'figures_for_upload',
    'supplement',
    'urban_analysis_dlnm.pdf'
  ),
  width = 14,
  height = 7
)

# create table 

tables <- all_results_urban %>% 
  mutate(est = round(est, digits = 3),
         ci_low = round(ci_low, digits = 3),
         ci_high = round(ci_high, digits = 3)) %>%
  mutate(est_w_ci = paste0(est, ', [', ci_low, ', ', ci_high, ']')) %>%
  select(m_name, lags, outcome_type, est_w_ci) %>%
  pivot_wider(names_from = lags,
              values_from = c('est_w_ci'),names_prefix = 'Lag day ')

write_csv(tables,
          here("figures_for_upload", "supplement", "urban_analysis_results_table.csv"))

# Models ------------------------------------------------------------------

# Rural -------------------------------------------------------------------

exposure_columns <-
  c('exposed_8_hrs_0.01',
    'exposed_8_hrs_0.03',
    'exposed_8_hrs_0.05')

cvd_dlnms_rural <- run_dlnm_models(
  po_data = rural,
  outcome_col = 'n_all_cvd',
  exposure_cols = exposure_columns,
  offset_col = 'n_benes',
  precip_dfs = 2,
  po_dfs = 4
)

resp_dlnms_rural <- run_dlnm_linear_precip(
  po_data = rural,
  outcome_col = 'n_resp',
  exposure_cols = exposure_columns,
  offset_col = 'n_benes',
  po_dfs = 3
)

# extract results 
cvd_dlnm_preds_rural <- rbindlist(get_dlnm_pred(cvd_dlnms_rural))
resp_dlnm_preds_rural <- rbindlist(get_dlnm_pred(resp_dlnms_rural))

# add outcome type labels
cvd_dlnm_preds_rural <- cvd_dlnm_preds_rural %>%
  mutate(outcome_type = 'Cardiovascular-related hosp')
resp_dlnm_preds_rural <- resp_dlnm_preds_rural %>%
  mutate(outcome_type = 'Respiratory-related hosp')

# combine results
all_results_rural <- rbindlist(list(cvd_dlnm_preds_rural, resp_dlnm_preds_rural))

# rename m_name values
all_results_rural <- all_results_rural %>% 
  mutate(m_name = recode(m_name, 
                         'exposed_8_hrs_0.01' = '1%',
                         'exposed_8_hrs_0.03' = '3%',
                         'exposed_8_hrs_0.05' = '5%'))
# plot 
dlnm_main_analysis_plot <- 
  all_results_rural %>%
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
  theme_minimal(base_size = 17) +
  labs(
    x = "Lag (days)", 
    y = "Rate ratio", 
    color = "Power outage size") + 
  ggtitle(paste0("Association between power outage exposure and ",
                 "hospitalizations\nin older adults (age 65+) in fee-for-service Medicare\n RURAL AREAS ONLY")) +
  theme(
    panel.spacing = unit(1, "lines"),
    panel.border = element_rect(
      color = "grey",
      fill = NA,
      size = 1)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  #scale_color_brewer(palette = "Set1") +
  scale_color_manual(labels = c("1%", "3%", "5%"), 
                     values = c("#482677FF", "#2D708EFF", "#29AF7FFF")) +
  theme(strip.text = element_text(size = 17))


ggsave(
  dlnm_main_analysis_plot,
  filename = here(
    'figures_for_upload',
    'supplement',
    'rural_analysis_dlnm.pdf'
  ),
  width = 14,
  height = 7
)

# create table 

tables <- all_results_rural %>% 
  mutate(est = round(est, digits = 3),
         ci_low = round(ci_low, digits = 3),
         ci_high = round(ci_high, digits = 3)) %>%
  mutate(est_w_ci = paste0(est, ', [', ci_low, ', ', ci_high, ']')) %>%
  select(m_name, lags, outcome_type, est_w_ci) %>%
  pivot_wider(names_from = lags,
              values_from = c('est_w_ci'),names_prefix = 'Lag day ')

write_csv(tables,
          here("figures_for_upload", "supplement", "rural_analysis_results_table.csv"))
