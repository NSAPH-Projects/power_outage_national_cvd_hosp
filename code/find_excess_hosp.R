# if it were unconstrained, each lag is association indep of other lags
# so you can sum them (right)
# if they were from separate models, there would be autocorrelation so no sum

# first you'd sum the coefs and then exponentiate, that would give cumulative RR
# if it were unconstrained, the output (summary(m1)) gives you the effect est
# and the standard error

# model estimates covar matrix - so you need the covars var(x) + var(y) + 2cov(x,y)
# so for calculating the CIs, you could extract all this for the seven coefs 


# RR fit mat
# cum RR fit mat (something like that)
# the cum RR mat is the cumulative RR, and you use the standard errors that come
# with it - there is an SE fit
# you extract that from there 


# using the cumulative RR and the CIs for that

# we'll calculate the AF_e (num incidents among exposed group attributable to exposure)
# multiply AF_e by the population and exposure 



# 1.07

(1.07-1)*(1.07)*23000000*0.00037*7







# Calculate the absolute risk increase
# CVD
sum_of_rrs = 2 + 2 + 2.3 + 0.6 -1.2 - 0.8 + 0.09


hosp_rate = 1351 / 10000

daily_hosp_rate = hosp_rate / 365

power_outage_hosp = ((daily_hosp_rate + 0.0499*daily_hosp_rate)*7) + (daily_hosp_rate * (365 - 7))

regular_hosp = daily_hosp_rate * 365

extra_hosp = (power_outage_hosp * 23622770) - (regular_hosp * 23622770)

no_po_hosp = regular_hosp * 23622770

po_hosp = power_outage_hosp * 23622770


# RESP 
sum_of_rrs = 2.5 + 0.17 + 0.9 + 0.4 + 0 - 0.02 - 0.03

hosp_rate = 805 / 10000

daily_hosp_rate = hosp_rate / 365

power_outage_hosp = ((daily_hosp_rate + 0.0392*daily_hosp_rate)*7) + (daily_hosp_rate * (365 - 7))

regular_hosp = daily_hosp_rate * 365

extra_hosp = (power_outage_hosp * 23622770) - (regular_hosp * 23622770)

no_po_hosp = regular_hosp * 23622770

po_hosp = power_outage_hosp * 23622770