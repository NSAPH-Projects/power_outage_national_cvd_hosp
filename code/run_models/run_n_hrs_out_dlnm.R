# Do analysis of continuous number of hours of power outage 

# Libraries ---------------------------------------------------------------

pacman::p_load(tidyverse, data.table, here, arrow, gnm, splines, dlnm)

source(here("code", "run_models", "run_models_helper_functions.R"))


# Read --------------------------------------------------------------------

an_dat <- read_rds(here('data', 'an_dat_urgent_hosp_dec_1.RDS'))

# Do ----------------------------------------------------------------------

# same for both 
temp_crossbasis_ns <- crossbasis(
  an_dat$max_temp,
  lag = 6,
  argvar = list(fun = "ns", df = 3),
  arglag = list(fun = "ns", df = 3)
)

# 3 df for continuous PO exposure, lag dim 5
power_outage_crossbasis_3_df <- crossbasis(
  an_dat$n_hrs_out_0.01,
  lag = 6,
  argvar = list(fun = "ns", df = 3, knots = c(2, 5)),
  arglag = list(fun = "ns", df = 5)
)

# linear for continuous PO exposure, lag df 5
power_outage_crossbasis_linear_lag_5_df <- crossbasis(
  an_dat$n_hrs_out_0.01,
  lag = 6,
  argvar = list(fun = "lin"),
  arglag = list(fun = "ns", df = 5)
)

# linear for continuous PO exposure, lag df 4
power_outage_crossbasis_linear_lag_4_df <- crossbasis(
  an_dat$n_hrs_out_0.01,
  lag = 6,
  argvar = list(fun = "lin"),
  arglag = list(fun = "ns", df = 4)
)

# linear for continuous PO exposure, lag df 3
power_outage_crossbasis_linear_lag_3_df <- crossbasis(
  an_dat$n_hrs_out_0.01,
  lag = 6,
  argvar = list(fun = "lin"),
  arglag = list(fun = "ns", df = 3)
)

# models

model_linear <- gnm(
  n_cvd_no_hem_no_hyp ~ 
  power_outage_crossbasis_linear +
  temp_crossbasis_ns +
    ns(precip, df = 2) +
    ns(wind_speed, df = 3) + 
    offset(log(n_benes)),
  family = quasipoisson(),
  eliminate = as.factor(an_dat$stratum),
  data = an_dat
)

model_3_df <- gnm(
  n_cvd_no_hem_no_hyp ~ 
    power_outage_crossbasis_3_df +
    temp_crossbasis_ns +
    ns(precip, df = 2) +
    ns(wind_speed, df = 3) + 
    offset(log(n_benes)),
  family = quasipoisson(),
  eliminate = as.factor(an_dat$stratum),
  data = an_dat
)

# compare models 
anova(model_3_df, model_linear, test ='F')

# not a better fit according to the anova

# get preds 
pred_po_linear <- crosspred(
  basis = power_outage_crossbasis_linear,  
  # the model
  model= model_linear,
  at = 0:24, 
  bylag = 1)

plot(pred_po_linear)



pred_po_3_df <- crosspred(
  basis = power_outage_crossbasis_3_df,  
  model= model_3_df,
  at =0:24, 
  bylag = 1)

plot(pred_po_3_df)

# Plot results and save ---------------------------------------------------

# 3 dfs
x = 0:24
y = 0:6
m = as.matrix(pred_po_3_df$matfit)

png(here('figures', 'figures_output', 'dlnm_n_hrs_out_3_df_cvd.png'),  
    width = 1000, height = 400)

par(mar = c(5,7,4,2) + 0.5)

persp(y, x, z = t(m), theta = 30, phi = 30, col = "lightblue", 
      xlab = " ", ylab = " ", zlab = " ", 
      ticktype = "detailed")
dev.off()

# linear
x = 0:24
y = 0:6
m = as.matrix(pred_po_linear$matfit)

png(here('figures', 'figures_output', 'dlnm_n_hrs_out_linear_cvd.png'),  
    width = 1000, height = 400)

par(mar = c(5,7,4,2) + 0.5)

persp(y, x, z = t(m), theta = 30, phi = 30, col = "lightblue", 
      xlab = " ", ylab = " ", zlab = " ", 
      ticktype = "detailed")
dev.off()


# Resp outcomes -----------------------------------------------------------

# same for both 
temp_crossbasis_ns <- crossbasis(
  an_dat$max_temp,
  lag = 6,
  argvar = list(fun = "ns", df = 3),
  arglag = list(fun = "ns", df = 3)
)

# 3 df for continuous PO exposure 
power_outage_crossbasis_3_df <- crossbasis(
  an_dat$n_hrs_out_0.01,
  lag = 6,
  argvar = list(fun = "ns", df = 3, knots = c(2, 5)),
  arglag = list(fun = "ns", df = 5)
)

# linear for continuous PO exposure
power_outage_crossbasis_linear <- crossbasis(
  an_dat$n_hrs_out_0.01,
  lag = 6,
  argvar = list(fun = "lin"),
  arglag = list(fun = "ns", df = 5)
)

# models

model_linear_resp <- gnm(
  n_resp ~ 
    power_outage_crossbasis_linear +
    temp_crossbasis_ns +
    precip +
    ns(wind_speed, df = 3) + 
    offset(log(n_benes)),
  family = quasipoisson(),
  eliminate = as.factor(an_dat$stratum),
  data = an_dat
)

model_3_df_resp <- gnm(
  n_resp ~ 
    power_outage_crossbasis_3_df +
    temp_crossbasis_ns +
    precip +
    ns(wind_speed, df = 3) + 
    offset(log(n_benes)),
  family = quasipoisson(),
  eliminate = as.factor(an_dat$stratum),
  data = an_dat
)

# compare models 
anova(model_3_df_resp, model_linear_resp, test ='F')

# not a better fit according to the anova

# get preds 
pred_po_linear_resp <- crosspred(
  basis = power_outage_crossbasis_linear,  
  # the model
  model= model_linear_resp,
  at = 0:24, 
  bylag = 1)

plot(pred_po_linear_resp)



pred_po_3_df_resp <- crosspred(
  basis = power_outage_crossbasis_3_df,  
  model= model_3_df_resp,
  at =0:24, 
  bylag = 1)

plot(pred_po_3_df_resp)

# Plot results and save ---------------------------------------------------

# 3 dfs
x = 0:24
y = 0:6
m = as.matrix(pred_po_3_df_resp$matfit)

png(here('figures', 'figures_output', 'dlnm_n_hrs_out_3_df_resp.png'),  
    width = 1000, height = 400)

par(mar = c(5,7,4,2) + 0.5)

persp(y, x, z = t(m), theta = 30, phi = 30, col = "lightblue", 
      xlab = " ", ylab = " ", zlab = " ", 
      ticktype = "detailed")
dev.off()

# linear
x = 0:24
y = 0:6
m = as.matrix(pred_po_linear_resp$matfit)

png(here('figures', 'figures_output', 'dlnm_n_hrs_out_linear_resp.png'),  
    width = 1000, height = 400)

par(mar = c(5,7,4,2) + 0.5)

persp(y, x, z = t(m), theta = 30, phi = 30, col = "lightblue", 
      xlab = " ", ylab = " ", zlab = " ", 
      ticktype = "detailed")
dev.off()

# ok so interesting - 3 dfs is actually a better fit for respiratory hosp 
