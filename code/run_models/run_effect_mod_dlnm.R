# Run effect modification analyses with constrained lag terms 

# Libraries ---------------------------------------------------------------

pacman::p_load(tidyverse, data.table, here, arrow, gnm, splines, dlnm)

source(here("code", "run_models", "run_models_helper_functions.R"))


# Read --------------------------------------------------------------------

an_dat <- read_rds(here('data', 'an_dat_urgent_hosp_dec_17.RDS'))

dme_by_county <-
  read_rds(here(
    "data_for_upload",
    "effect_mod_data",
    "dme_use_by_county.RDS"
  ))

pov_by_county <-
  read_rds(here(
    "data_for_upload",
    "effect_mod_data",
    "pov_measures.RDS"
  ))

an_dat <- an_dat %>%
  left_join(dme_by_county) %>%
  left_join(pov_by_county)

q1_pov <- an_dat %>% filter(pov_quartile == 1)
q4_pov <- an_dat %>% filter(pov_quartile == 4)

q1_dme <- an_dat %>% filter(dme_quartile == 1)
q4_dme <- an_dat %>% filter(dme_quartile == 4)

# Run ---------------------------------------------------------------------

effect_mod_models_cvd <- list(
  cvd_75_over = run_dlnm_po_model_copilot(
    po_data = an_dat,
    outcome_col = 'n_cvd_no_hyp_1_age',
    exposure_col = 'exposed_8_hrs_0.01',
    offset_col = 'n_benes_older_75',
    precip_dfs = 2,
    po_dfs = 4
  ),
  cvd_less_75 = run_dlnm_po_model_copilot(
    outcome_col = 'n_cvd_no_hyp_0_age',
    offset_col = 'n_benes_under_75',
    po_data = an_dat,
    exposure_col = 'exposed_8_hrs_0.01',
    precip_dfs = 2,
    po_dfs = 4
  ),
  cvd_male_benes = run_dlnm_po_model_copilot(
    outcome_col = 'n_cvd_no_hyp_1_sex',
    offset_col = 'n_benes_sex_1',
    po_data = an_dat,
    exposure_col = 'exposed_8_hrs_0.01',
    precip_dfs = 2,
    po_dfs = 4
  ),
  cvd_female_benes = run_dlnm_po_model_copilot(
    outcome_col = 'n_cvd_no_hyp_2_sex',
    offset_col = 'n_benes_sex_2',
    an_dat,
    exposure_col = 'exposed_8_hrs_0.01',
    precip_dfs = 2,
    po_dfs = 4
  ),
  cvd_pov_q1 = run_dlnm_po_model_copilot(
    outcome_col = 'n_cvd_no_hyp',
    offset_col = 'n_benes',
    q1_pov,
    exposure_col = 'exposed_8_hrs_0.01',
    precip_dfs = 2,
    po_dfs = 4
  ),
  cvd_pov_q4 = run_dlnm_po_model_copilot(
    outcome_col = 'n_cvd_no_hyp',
    offset_col = 'n_benes',
    q4_pov,
    exposure_col = 'exposed_8_hrs_0.01',
    precip_dfs = 2,
    po_dfs = 4
  ),
  cvd_dme_q1 = run_dlnm_po_model_copilot(
    outcome_col = 'n_cvd_no_hyp',
    offset_col = 'n_benes',
    q1_dme,
    exposure_col = 'exposed_8_hrs_0.01',
    precip_dfs = 2,
    po_dfs = 4
  ),
  cvd_dme_q4 = run_dlnm_po_model_copilot(
    outcome_col = 'n_cvd_no_hyp',
    offset_col = 'n_benes',
    q4_dme,
    exposure_col = 'exposed_8_hrs_0.01',
    precip_dfs = 2,
    po_dfs = 4
  )
)

effect_mod_models_resp = list(
  resp_75_over = run_dlnm_po_model_linear_precip(
    outcome_col = 'n_resp_1_age',
    offset_col = 'n_benes_older_75',
    po_data = an_dat,
    exposure_col = 'exposed_8_hrs_0.01',
    po_dfs = 3
  ),
  resp_less_75 = run_dlnm_po_model_linear_precip(
    outcome_col = 'n_resp_0_age',
    offset_col = 'n_benes_under_75',
    po_data = an_dat,
    exposure_col = 'exposed_8_hrs_0.01',
    po_dfs = 3
  ),
  resp_male_benes = run_dlnm_po_model_linear_precip(
    outcome_col = 'n_resp_1_sex',
    offset_col = 'n_benes_sex_1',
    po_data = an_dat,
    exposure_col = 'exposed_8_hrs_0.01',
    po_dfs = 3
  ),
  resp_female_benes = run_dlnm_po_model_linear_precip(
    outcome_col = 'n_resp_2_sex',
    offset_col = 'n_benes_sex_2',
    an_dat,
    exposure_col = 'exposed_8_hrs_0.01',
    po_dfs = 3
  ),
  resp_pov_q1 = run_dlnm_po_model_linear_precip(
    outcome_col = 'n_resp',
    offset_col = 'n_benes',
    q1_pov,
    exposure_col = 'exposed_8_hrs_0.01',
    po_dfs = 3
  ),
  resp_pov_q4 = run_dlnm_po_model_linear_precip(
    outcome_col = 'n_resp',
    offset_col = 'n_benes',
    q4_pov,
    exposure_col = 'exposed_8_hrs_0.01',
    po_dfs = 3
  ),
  resp_dme_q1 = run_dlnm_po_model_linear_precip(
    outcome_col = 'n_resp',
    offset_col = 'n_benes',
    q1_dme,
    exposure_col = 'exposed_8_hrs_0.01',
    po_dfs = 3
  ),
  resp_dme_q4 = run_dlnm_po_model_linear_precip(
    outcome_col = 'n_resp',
    offset_col = 'n_benes',
    q4_dme,
    exposure_col = 'exposed_8_hrs_0.01',
    po_dfs = 3
  )
)

cvd_effect_mod_dlnm_preds <- rbindlist(get_dlnm_pred(effect_mod_models_cvd))
resp_effect_mod_dlnm_preds <- rbindlist(get_dlnm_pred(effect_mod_models_resp))

cvd_effect_mod_dlnm_preds <- cvd_effect_mod_dlnm_preds %>%
  mutate(outcome_type = 'Cardiovascular-related hosp,\nexcluding hypertension')
resp_effect_mod_dlnm_preds <- resp_effect_mod_dlnm_preds %>%
  mutate(outcome_type = 'Respiratory-related hosp')

# bind results 
all_results <- 
  rbindlist(list(cvd_effect_mod_dlnm_preds, resp_effect_mod_dlnm_preds))

# recode names 
all_results <- all_results %>% 
  mutate(effect_mod_type = case_when(
    grepl('75', m_name) ~ 'Age',
    grepl('male', m_name) ~ 'Sex',
    grepl('dme', m_name) ~ 'DME',
    grepl('pov', m_name) ~ 'Poverty'
  )) %>% 
  mutate(cat = case_when(
  grepl('75_over', m_name) ~ '75 and over',
  grepl('less_75', m_name) ~ 'Age 65 - 75',
  grepl('female_benes', m_name) ~ 'Female',
  grepl('male_benes', m_name) ~ 'Male',
  grepl('pov_q1', m_name) ~ '1st quartile poverty',
  grepl('pov_q4', m_name) ~ '4th quartile poverty',
  grepl('dme_q1', m_name) ~ '1st quartile DME use',
  grepl('dme_q4', m_name) ~ '4th quartile DME use'))

all_results$effect_mod_type <-
  factor(all_results$effect_mod_type, levels = c("Age", "Sex", "Poverty", "DME"))


# plot 
effect_mod_plot <- 
  all_results %>%
  ggplot() +
  geom_hline(aes(yintercept = 1)) +
  geom_point(
    size = 3.5, 
    aes(x = lags, y = est, color = cat),
    position = position_dodge(width = 0.5)) +
  geom_errorbar(
    width = 0.5,
    size = 1,
    position = position_dodge(width = 0.5),
    aes(x = lags, ymin = ci_low, ymax = ci_high, color = cat)) +
  facet_grid(effect_mod_type ~ outcome_type, scales = 'free_y') +
  theme_minimal(base_size = 25) +
  labs(
    x = "Lag (days)", 
    y = "Rate ratio", 
    color = "") + 
  ggtitle(
    paste0("Association between power outage exposure and ",
           "hospitalizations\nin older adults (age 65+) in ",
           "fee-for-service Medicare")) +
  labs(subtitle = 'Stratified by potential effect modifiers') +
  theme(
    panel.spacing = unit(1, "lines"),
    panel.border = element_rect(
      color = "grey",
      fill = NA,
      size = 1)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) 

ggsave(
  effect_mod_plot,
  filename = here('figures_for_upload', 'effect_mod_dlnm_dec_17.pdf'),
  width = 17,
  height = 15
)

tables <- all_results %>% 
  mutate(est = round(est, digits = 3),
         ci_low = round(ci_low, digits = 3),
         ci_high = round(ci_high), digits = 3) %>%
  mutate(est_w_ci = paste0(est, ', [', ci_low, ', ', ci_high, ']')) %>%
  select(effect_mod_type, cat, lags, outcome_type, est_w_ci) %>%
  pivot_wider(names_from = lags,
              values_from = c('est_w_ci'),names_prefix = 'Lag day ',
              id_cols = c("effect_mod_type", "cat", "outcome_type"))

write_csv(tables,
          here("figures_for_upload", "supplement", "effect_mod_table.csv"))

