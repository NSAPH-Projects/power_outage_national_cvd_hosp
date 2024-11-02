# Check relationship between precipitation and CVD and respiratory
# hospitalizations and outages to check how to control for precip.

pacman::p_load(here, tidyverse, arrow, mgcv)

an_dat <- read_rds(here('data', 'an_dat_urgent_hosp.RDS'))

# run a model to see if the relationship between precipitation and hosp is 
# non-linear, using penalized spline and controlling for temp
resp_model <-
  gam(
    n_resp ~ s(precip, bs = "ps") +
      s(max_temp, bs = "ps") +
      offset(log(n_benes)),
    family = poisson,
    data = an_dat,
    method = 'REML'
  )

cvd_model <-
  gam(
    n_cvd ~ s(precip, bs = "ps") +
      s(max_temp, bs = "ps") +
      offset(log(n_benes)),
    family = poisson,
    data = an_dat,
    method = "REML"
  )

summary(cvd_model)
summary(resp_model)

o <- an_dat %>% 
  mutate(p_precip = ntile(precip, 10)) %>% 
  group_by(p_precip) %>% 
  summarize(mean_outage = mean(exposed_8_hrs_0.005),
            mean_cvd = mean(n_cvd),
            mean_resp = mean(n_resp))

plot(o$p_precip, o$mean_cvd)
plot(o$p_precip, o$mean_outage)
plot(o$p_precip, o$mean_resp)

# interesting. it seems like a spline is the best way maybe w 4 dfs.
# need to talk to marianthi. 
