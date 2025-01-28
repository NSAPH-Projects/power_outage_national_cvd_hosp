# Do analysis of continuous number of hours of power outage 

# Libraries ---------------------------------------------------------------

pacman::p_load(tidyverse, data.table, here, arrow, gnm, splines, dlnm)

source(here("code", "run_models", "run_models_helper_functions.R"))


# Read --------------------------------------------------------------------

an_dat <- read_rds(here('data', 'an_dat_urgent_hosp_jan_28.RDS'))

# Do ----------------------------------------------------------------------

# same for both 
temp_crossbasis_ns <- crossbasis(
  an_dat$max_temp,
  lag = 6,
  argvar = list(fun = "ns", df = 3),
  arglag = list(fun = "ns", df = 3)
)


# linear for continuous PO exposure, lag df 4 - for cvd
power_outage_crossbasis_linear_lag_4_df <- crossbasis(
  an_dat$n_hrs_out_0.01,
  lag = 6,
  argvar = list(fun = "lin"),
  arglag = list(fun = "ns", df = 4)
)

# linear for continuous PO exposure, lag df 3 - for resp
power_outage_crossbasis_linear_lag_3_df <- crossbasis(
  an_dat$n_hrs_out_0.01,
  lag = 6,
  argvar = list(fun = "lin"),
  arglag = list(fun = "ns", df = 3)
)


# Run ---------------------------------------------------------------------

model_linear_lag_df_4_cvd <- gnm(
  n_all_cvd ~ 
    power_outage_crossbasis_linear_lag_4_df +
    temp_crossbasis_ns +
    ns(precip, df = 2) +
    ns(wind_speed, df = 3) + 
    offset(log(n_benes)),
  family = quasipoisson(),
  eliminate = as.factor(an_dat$stratum),
  data = an_dat
)

model_linear_lag_df_3_resp <- gnm(
  n_resp ~ 
    power_outage_crossbasis_linear_lag_3_df +
    temp_crossbasis_ns +
    precip +
    ns(wind_speed, df = 3) + 
    offset(log(n_benes)),
  family = quasipoisson(),
  eliminate = as.factor(an_dat$stratum),
  data = an_dat
)

# get preds
pred_po_cvd <- crosspred(
  basis = power_outage_crossbasis_linear_lag_4_df,  
  # the model
  model= model_linear_lag_df_4_cvd,
  at = 0:24, 
  bylag = 1)

pred_po_resp <- crosspred(
  basis = power_outage_crossbasis_linear_lag_3_df,  
  # the model
  model= model_linear_lag_df_3_resp,
  at = 0:24, 
  bylag = 1)

# plot --------------------------------------------------------------------
x = 0:24
y = 0:6
m = as.matrix(pred_po_resp$matfit)

png(here('figures', 'figures_output', 'dlnm_n_hrs_out_resp_linear_3df_lag.png'),  
    width = 1000, height = 400)

par(mar = c(5,7,4,2) + 0.5)

persp(y, x, z = t(m), theta = 30, phi = 30, col = "lightblue", 
      xlab = " ", ylab = " ", zlab = " ", 
      ticktype = "detailed")
dev.off()

x = 0:24
y = 0:6
m = as.matrix(pred_po_cvd$matfit)

png(here('figures', 'figures_output', 'dlnm_n_hrs_out_cvd_linear_4df_lag.png'),  
    width = 1000, height = 400)

par(mar = c(5,7,4,2) + 0.5)

persp(y, x, z = t(m), theta = 30, phi = 30, col = "lightblue", 
      xlab = " ", ylab = " ", zlab = " ", 
      ticktype = "detailed")
dev.off()


# CVD 
cvd_results <- data.frame(n_hrs = seq(0:24),
  est = pred_po_cvd$matRRfit,
                          low = pred_po_cvd$matRRlow,
                          high = pred_po_cvd$matRRhigh)

cvd_long <- cvd_results %>%
  pivot_longer(
    cols = -n_hrs,
    names_to = c(".value", "lag"),
    names_pattern = "(.*)\\.(lag\\d+)"
  ) %>%
  mutate(lag = case_when(lag == 'lag0' ~ 'Lag day 0',
                   lag == 'lag1' ~ 'Lag day 1',
                   lag == 'lag2' ~ 'Lag day 2',
                   lag == 'lag3' ~ 'Lag day 3',
                   lag == 'lag4' ~ 'Lag day 4',
                   lag == 'lag5' ~ 'Lag day 5',
                   lag == 'lag6' ~ 'Lag day 6'))

# Create the plot
cvd <- ggplot(cvd_long, aes(x = n_hrs, y = est)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.2) +
  geom_hline(yintercept = 1, linetype = 'dashed') +
  facet_grid(~ lag) +
  labs(title = "Effect of power outage (number of hours without power) 
on CVD hospitalization risk among Mediare fee-for-service 
beneficiaries",
       x = "N hours without power within 24-hr period (day)",
       y = "Rate ratio") +
  theme_minimal(base_size = 20) +
  theme(panel.border = element_rect(color = "black", fill = NA),
        axis.ticks = element_line(color = "black")) +
  scale_x_continuous(limits = c(0, 24), breaks = seq(0, 24, by = 6))

ggsave(
  cvd,
  filename = here("figures_for_upload", "supplement", "n_hrs_out_cvd.pdf"),
  width = 15,
  height = 5
)

# resp
resp_results <- data.frame(n_hrs = seq(0:24),
                          est = pred_po_resp$matRRfit,
                          low = pred_po_resp$matRRlow,
                          high = pred_po_resp$matRRhigh)

resp_long <- resp_results %>%
  pivot_longer(
    cols = -n_hrs,
    names_to = c(".value", "lag"),
    names_pattern = "(.*)\\.(lag\\d+)"
  ) %>%
  mutate(lag = case_when(lag == 'lag0' ~ 'Lag day 0',
                         lag == 'lag1' ~ 'Lag day 1',
                         lag == 'lag2' ~ 'Lag day 2',
                         lag == 'lag3' ~ 'Lag day 3',
                         lag == 'lag4' ~ 'Lag day 4',
                         lag == 'lag5' ~ 'Lag day 5',
                         lag == 'lag6' ~ 'Lag day 6'))

# create the plot
resp <- ggplot(resp_long, aes(x = n_hrs, y = est)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.2) +
  geom_hline(yintercept = 1, linetype = 'dashed') +
  facet_grid(~ lag) +
  labs(title = "Effect of power outage (number of hours without power) 
on respiratory hospitalization risk among Mediare fee-for-service 
beneficiaries",
       x = "N hours without power within 24-hr period (day)",
       y = "Rate ratio") +
  theme_minimal(base_size = 20) +
  theme(panel.border = element_rect(color = "black", fill = NA),
        axis.ticks = element_line(color = "black")) +
  scale_x_continuous(limits = c(0, 24), breaks = seq(0, 24, by = 6))


ggsave(
  resp,
  filename = here("figures_for_upload", "supplement", "n_hrs_out_resp.pdf"),
  width = 15,
  height = 5
)

# plotted mat fit. 