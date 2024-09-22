library(tidyverse)
library(brms)
library(janitor)
library(tidybayes)
library(scales)

# function to estimate stream temperatures from air temperature
# from eq. 2 in Mohseni, O., Erickson, T. R., & Stefan, H. G. (1997). A Non-Linear Regression Model for Weekly Stream Temperatures at 585 Gaging Stations in the US.
estimate_streamtemp = function(x){
  numerator = 26.2                           # values are median parameter values from Mohseni et al. Tbl X.
  denominator = 1 + (exp(0.18*(13.3 - x)))
  0.8 + (numerator/denominator)
}

# load data
hydroatlas_temps_precip = read_csv("data/Production_wHydrobasinID_06262024.csv") %>% 
  mutate(tmp_dc_syr = tmp_dc_syr/10) # put temps in dec C instead of 10*dec C

emergence_production = readRDS("data/emergence_production_bioclim.rds") %>% 
  ungroup %>% 
  left_join(hydroatlas_temps_precip) %>% 
  mutate(emerge_1 = mean_emergence_mgdmm2y/max(mean_emergence_mgdmm2y, na.rm = T),
         stream_temp = estimate_streamtemp(tmp_dc_syr),
         stream_temp20 = stream_temp/20) %>%  # reduce range of stream temps to improve model fitting
  mutate(BAS_ID = as.character(BAS_ID))


# fit models --------------------------------------------------------------

fit_gam_temp = readRDS("models/fit_gam_temp.rds")
fit_gam_precip = readRDS("models/fit_gam_precip.rds")
fit_gam_tempprecip = readRDS("models/fit_gam_tempprecip.rds")
fit_gam_intercept = readRDS("models/fit_gam_intercept.rds")
fit_gam_tempaddprecip = readRDS(file = "models/fit_gam_tempaddprecip.rds")

fit_gam_temp = brm(emerge_1 ~ s(stream_temp20) + (1|author_year) + (1|BAS_ID),
              family = Gamma(link = "log"),
              data = emergence_production,
              chains = 4, iter = 2000,
              prior = c(prior(normal(-5, 2), class = "Intercept"),
                        prior(normal(0, 2), class = "b"),
                        prior(exponential(2), class = "sd"),
                        prior(exponential(4), class = "shape"))
)

saveRDS(fit_gam_temp, file = "models/fit_gam_temp.rds")

fit_gam_tempprecip = update(fit_gam_temp,
                            formula = . ~ s(stream_temp20, precip_s) + (1|author_year) + (1|BAS_ID),
                            newdata = emergence_production,
                            iter = 2000, chains = 4)

saveRDS(fit_gam_tempprecip, file = "models/fit_gam_tempprecip.rds")

fit_gam_precip = update(fit_gam_temp,
                            formula = . ~ s(precip_s) + (1|author_year) + (1|BAS_ID),
                            newdata = emergence_production,
                            iter = 2000, chains = 4)

saveRDS(fit_gam_precip, file = "models/fit_gam_precip.rds")

fit_gam_intercept = update(fit_gam_temp,
                        formula = . ~ 1 + (1|author_year) + (1|BAS_ID),
                        newdata = emergence_production,
                        iter = 2000, chains = 4)

saveRDS(fit_gam_intercept, file = "models/fit_gam_intercept.rds")

fit_gam_tempaddprecip = update(fit_gam_temp,
                            formula = . ~ s(stream_temp20) + s(precip_s) + (1|author_year) + (1|BAS_ID),
                            newdata = emergence_production,
                            iter = 2000, chains = 4)

saveRDS(fit_gam_tempaddprecip, file = "models/fit_gam_tempaddprecip.rds")


# check models ------------------------------------------------------------

pp_check(fit_gam_intercept, type = "boxplot") 
pp_check(fit_gam_precip, type = "boxplot")
pp_check(fit_gam_temp, type = "boxplot") 
pp_check(fit_gam_tempprecip, type = "boxplot")
pp_check(fit_gam_tempaddprecip, type = "boxplot")

waic_a = waic(fit_gam_intercept)
waic_b = waic(fit_gam_temp)
waic_c = waic(fit_gam_precip)
waic_d = waic(fit_gam_tempprecip)
waic_e = waic(fit_gam_tempaddprecip)

loo_compare(waic_a,
            waic_b,
            waic_c,
            waic_d,
            waic_e)

newdata = tibble(stream_temp20 = seq(from = 0, to = 1.5, length.out = 40)) %>% 
  expand_grid(precip_s = c(-1, 0, 1)) %>% 
  mutate(author_year = "new")

mod_averages = pp_average(fit_gam_temp, fit_gam_precip, newdata = newdata, allow_new_levels = T)

mod_averages %>% as_tibble() %>% 
  mutate(stream_temp20 = newdata$stream_temp20,
         precip_s = newdata$precip_s) %>% 
  ggplot(aes(x = stream_temp20)) + 
  geom_lineribbon(aes(y = Estimate, ymin = Q2.5, ymax = Q97.5)) +
  facet_wrap(~precip_s)

plot(conditional_effects(fit_gam_precip), points = T)
plot(conditional_effects(fit_gam_temp), points = T)
plot(conditional_effects(fit_gam_tempprecip), points = T)
plot(conditional_effects(fit_gam_tempaddprecip), points = T)

