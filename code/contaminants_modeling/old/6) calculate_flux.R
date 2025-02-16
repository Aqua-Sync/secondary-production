library(tidyverse)
library(brms)
library(tidybayes)
library(bit64)
library(cccapi)

# load data and models
contaminants = readRDS(file = "data/contaminants.rds")
flux_predictions_all = readRDS("posteriors/flux_predictions_all.rds")
cas_names = readRDS(file = "data/cas_names.rds")
modeled_water = readRDS(file = "data/modeled_water.rds")
mod_list = readRDS(file = "models/mod_list.rds")

# This code generates aquatic concentrations of contaminants. # concentrations from Jakob on seafile.rlp.net...then reformat. mean.conc.year is the mean, mean.det.year is detection. 
#1) filter to desired contaminant
sort(unique(cas_names$chemical))
chem = cas_names %>% 
  filter(chemical == "Copper")

water_predictions = modeled_water %>% 
  filter(cas == chem$cas) %>% 
  rename(HYBAS_ID = HYBAS_L12) %>% 
  mutate(chemical = chem$chemical,
         HYBAS_ID = as.numeric(as.character(HYBAS_ID)),
         mean.conc.year_units = "log10_ug_l",
         water_ug_l_raw = 10^(mean.conc.year*mean.det.year),
         log_water_ug_l = log(water_ug_l_raw))

# get parameters of adult_conc ~ a + b*water_conc
unique(sapply(mod_list, function(m) m$data2$chemical)) # show possibilities
mod = Filter(function(m) m$data2$chemical == "Cu", mod_list)[[1]]

int_slope_post = as_draws_df(mod) %>%
  mutate(b_Intercept = b_Intercept + rnorm(nrow(.), 0, 1.18)) %>%
  reframe(int = median(b_Intercept),
          int_lower = quantile(b_Intercept, probs = 0.025),
          int_upper = quantile(b_Intercept, probs = 0.975),
          slope = median(b_x_s),
          slope_lower = quantile(b_x_s, probs = 0.025),
          slope_upper = quantile(b_x_s, probs = 0.975))

# combine
hybas_predictions = flux_predictions_all %>% 
  # slice(1:20) %>%
  right_join(water_predictions) %>% 
  filter(!is.na(log_water_ug_l)) %>%
  mutate(mean_x = mod$data2$mean_x$`scaled:center`,
         sd_x = mod$data2$sd_x$`scaled:scale`,
         max_adult_conc = mod$data2$max_y) %>% 
  mutate(x_s = (log_water_ug_l - mean_x)/sd_x) %>% 
  mutate(int = int_slope_post$int,
         slope = int_slope_post$slope,
         int_lower = int_slope_post$int_lower,
         int_upper = int_slope_post$int_upper,
         slope_lower = int_slope_post$slope_lower,
         slope_upper = int_slope_post$slope_upper) %>% 
  mutate(y_s = exp(int + slope*x_s),
         y_s_lower = exp(int_lower + slope_lower*x_s),
         y_s_upper = exp(int_upper + slope_upper*x_s)) %>% 
  mutate(y = y_s*max_adult_conc,
         y_lower = y_s_lower*max_adult_conc,
         y_upper = y_s_upper*max_adult_conc,
         element = mod$data2$chemical_category,
         bug_conc_units = "ng_mg_dm") %>% 
  mutate(y_mg_kg = y,
         y_mg_kg_lower = y_lower,
         y_mg_kg_upper = y_upper) %>% 
  mutate(chem_flux_mg_year = y_mg_kg*mean,
         chem_flux_mg_year_lower = y_mg_kg_lower*mean,
         chem_flux_mg_year_upper = y_mg_kg_upper*mean)

hybas_predictions %>%
  filter(x_s == min(x_s, na.rm = T)|x_s == max(x_s, na.rm = T)) %>% 
  bind_rows(hybas_predictions %>%
              arrange(x_s) %>% 
              slice_sample(n = 70000)) %>% 
  # sample_n(80000) %>%
  ggplot(aes(x = exp((x_s*mod$data2$sd_x$`scaled:scale`) + 
                       mod$data2$mean_x$`scaled:center`), y = y)) +
  geom_line() +
  geom_ribbon(aes(ymin = y_lower, ymax = y_upper), alpha = 0.2) +
  geom_point(data = mod$data ,
             aes(y = y_s*mod$data2$max_y), color = 'red') +
  labs(y = "Contaminant flux (mg/kgDM/y)",
       x = "Contaminant in water (\u03bcg/l)") +
  scale_y_log10() +
  scale_x_log10() +
  NULL

sum(hybas_predictions$chem_flux_mg_year_lower, na.rm = T)/1e09
sum(hybas_predictions$chem_flux_mg_year, na.rm = T)/1e09
sum(hybas_predictions$chem_flux_mg_year_upper, na.rm = T)/1e09
