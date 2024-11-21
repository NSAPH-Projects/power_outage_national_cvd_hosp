library(ggplot2)
library(dplyr)
library(data.table)
# Run effect modification analyses with constrained lag terms 

# Libraries ---------------------------------------------------------------

pacman::p_load(tidyverse, data.table, here, arrow, gnm, splines,
               dlnm)

source(here("code", "run_models", "run_models_helper_functions.R"))


# Read --------------------------------------------------------------------

an_dat <- read_rds(here('data', 'an_dat_urgent_hosp_nov_5.RDS'))

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

q4_pov <- an_dat %>% filter(pov_quartile == 4)
q1_pov <- an_dat %>% filter(pov_quartile == 1)

q4_dme <- an_dat %>% filter(dme_quartile == 4)
q1_dme <- an_dat %>% filter(dme_quartile == 1)

# Run ---------------------------------------------------------------------

effect_mod_models_cvd <- list(
  cvd_75_over = run_dlnm_po_model(
    outcome_col = 'n_cvd_no_hem_no_hyp_1_age',
    offset_col = 'n_benes_older_75',
    po_data = an_dat,
    exposure_col = 'exposed_8_hrs_0.01'
  ),
  cvd_less_75 = run_dlnm_po_model(
    outcome_col = 'n_cvd_no_hem_no_hyp_0_age',
    offset_col = 'n_benes_under_75',
    po_data = an_dat,
    exposure_col = 'exposed_8_hrs_0.01'
  ),
  cvd_male_benes = run_dlnm_po_model(
    outcome_col = 'n_cvd_no_hem_no_hyp_1_sex',
    offset_col = 'n_benes_sex_1',
    po_data = an_dat,
    exposure_col = 'exposed_8_hrs_0.01'
  ),
  cvd_female_benes = run_dlnm_po_model(
    outcome_col = 'n_cvd_no_hem_no_hyp_2_sex',
    offset_col = 'n_benes_sex_2',
    an_dat,
    exposure_col = 'exposed_8_hrs_0.01'
  ),
  cvd_pov_q1 = run_dlnm_po_model(
    outcome_col = 'n_cvd_no_hem_no_hyp',
    offset_col = 'n_benes',
    q1_pov,
    exposure_col = 'exposed_8_hrs_0.01'
  ),
  cvd_pov_q4 = run_dlnm_po_model(
    outcome_col = 'n_cvd_no_hem_no_hyp',
    offset_col = 'n_benes',
    q4_pov,
    exposure_col = 'exposed_8_hrs_0.01'
  ),
  cvd_dme_q1 = run_dlnm_po_model(
    outcome_col = 'n_cvd_no_hem_no_hyp',
    offset_col = 'n_benes',
    q1_dme,
    exposure_col = 'exposed_8_hrs_0.01'
  ),
  cvd_dme_q4 = run_dlnm_po_model(
    outcome_col = 'n_cvd_no_hem_no_hyp',
    offset_col = 'n_benes',
    q4_dme,
    exposure_col = 'exposed_8_hrs_0.01'
  )
)

effect_mod_models_resp = list(
  resp_75_over = run_dlnm_po_model(
    outcome_col = 'n_resp_1_age',
    offset_col = 'n_benes_older_75',
    po_data = an_dat,
    exposure_col = 'exposed_8_hrs_0.01'
  ),
  resp_less_75 = run_dlnm_po_model(
    outcome_col = 'n_resp_0_age',
    offset_col = 'n_benes_under_75',
    po_data = an_dat,
    exposure_col = 'exposed_8_hrs_0.01'
  ),
  resp_male_benes = run_dlnm_po_model(
    outcome_col = 'n_resp_1_sex',
    offset_col = 'n_benes_sex_1',
    po_data = an_dat,
    exposure_col = 'exposed_8_hrs_0.01'
  ),
  resp_female_benes = run_dlnm_po_model(
    outcome_col = 'n_resp_2_sex',
    offset_col = 'n_benes_sex_2',
    an_dat,
    exposure_col = 'exposed_8_hrs_0.01'
  ),
  resp_pov_q1 = run_dlnm_po_model(
    outcome_col = 'n_resp',
    offset_col = 'n_benes',
    q1_pov,
    exposure_col = 'exposed_8_hrs_0.01'
  ),
  resp_pov_q4 = run_dlnm_po_model(
    outcome_col = 'n_resp',
    offset_col = 'n_benes',
    q4_pov,
    exposure_col = 'exposed_8_hrs_0.01'
  ),
  resp_dme_q1 = run_dlnm_po_model(
    outcome_col = 'n_resp',
    offset_col = 'n_benes',
    q1_dme,
    exposure_col = 'exposed_8_hrs_0.01'
  ),
  resp_dme_q4 = run_dlnm_po_model(
    outcome_col = 'n_resp',
    offset_col = 'n_benes',
    q4_dme,
    exposure_col = 'exposed_8_hrs_0.01'
  )
)

cvd_effect_mod_dlnm_preds <- rbindlist(get_dlnm_pred(effect_mod_models_cvd))
resp_effect_mod_dlnm_preds <- rbindlist(get_dlnm_pred(effect_mod_models_resp))

cvd_effect_mod_dlnm_preds <- cvd_effect_mod_dlnm_preds %>%
  mutate(outcome_type = 'Cardiovascular-related hosp, excluding hypertension')
resp_effect_mod_dlnm_preds <- resp_effect_mod_dlnm_preds %>%
  mutate(outcome_type = 'Respiratory-related hosp')



# Copilot plot ------------------------------------------------------------
# Combine results
all_results <-
  rbindlist(list(cvd_effect_mod_dlnm_preds, resp_effect_mod_dlnm_preds))

# Recode names
all_results <- all_results %>% 
  mutate(
    effect_mod_type = case_when(
      grepl('75', m_name) ~ 'Age',
      grepl('male', m_name) ~ 'Sex',
      grepl('dme', m_name) ~ 'DME',
      grepl('pov', m_name) ~ 'Poverty'
    ),
    cat = case_when(
      grepl('75_over', m_name) ~ '75 and over',
      grepl('less_75', m_name) ~ 'Younger than 75',
      grepl('female_benes', m_name) ~ 'Female',
      grepl('male_benes', m_name) ~ 'Male',
      grepl('pov_q1', m_name) ~ '1st quartile poverty',
      grepl('pov_q4', m_name) ~ '4th quartile poverty',
      grepl('dme_q1', m_name) ~ '1st quartile DME use',
      grepl('dme_q4', m_name) ~ '4th quartile DME use'
    )
  )

# Create a function to generate plots
create_plot <- function(results_df, effect_modifier, outcome_to_plot) {
    results_df %>%
    filter(effect_mod_type == effect_modifier) %>% 
    filter(outcome_type == outcome_to_plot) %>%
    ggplot() +
    geom_hline(aes(yintercept = 1), linetype = "dashed", size = 0.5) +
    geom_point(
      size = 3.5, 
      aes(x = lags, y = est, color = cat),
      position = position_dodge(width = 0.5)
    ) +
    geom_errorbar(
      width = 0.5,
      size = 1,
      position = position_dodge(width = 0.5),
      aes(x = lags, ymin = ci_low, ymax = ci_high, color = cat)
    ) +
    theme_minimal(base_size = 15) +
    # labs(
    #   x = "Lag", 
    #   y = "Risk ratio", 
    #   color = "Size of power outage:\n X% of county out or more"
    # ) + 
    # ggtitle(
    #   paste0("Association between power outage exposure and ",
    #          "hospitalizations\nin older adults (age >=65) in",
    #          " Medicare parts A and B")
    # ) +
    # labs(subtitle = paste('Stratified by', effect_mod_type)) +
    theme(
      panel.spacing = unit(1, "lines"),
      panel.border = element_rect(
        color = "grey",
        fill = NA,
        size = 1
      )
    ) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
}

# Generate plots for each combination of effect_mod_type and outcome_type
plots <- list()
effect_mod_types <- unique(all_results$effect_mod_type)
outcome_types <- unique(all_results$outcome_type)

for (effect_modifier in effect_mod_types) {
  for (outcome_to_plot in outcome_types) {
    plot <- create_plot(
      results_df = all_results,
      effect_modifier =
        effect_modifier,
      outcome_to_plot = outcome_to_plot
    )
    plots <- append(plots, list(plot))
  }
}

# Combine the plots using cowplot
combined_plot <- plot_grid(plotlist = plots, ncol = 2)

# Print the combined plot
print(combined_plot)