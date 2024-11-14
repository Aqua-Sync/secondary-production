library(tidyverse)
library(brms)
library(tidybayes)

# load data
contaminants = readRDS(file = "data/contaminants.rds")
flux_predictions_all = readRDS("posteriors/flux_predictions_all.rds")
selenium_predictions_all = readRDS("data/selenium_jeff.rds") %>% rename(HYBAS_ID = HYBAS_L12) %>% 
  mutate(HYBAS_ID = as.numeric(HYBAS_ID),
         mean.conc.year_units = "log10_ug_l",
         water_se_ug_l_raw = 10^(mean.conc.year*mean.det.year),
         log_water_se_ug_l = log(water_se_ug_l_raw))

# get parameters of adult_conc ~ a + b*water_conc
mod_list = readRDS(file = "models/mod_list.rds")

mod = mod_list[[5]]

mean_x = mod$data2$mean_x$`scaled:center`
sd_x = mod$data2$sd_x$`scaled:scale`
max_adult_conc = mod$data2$max_y

int_slope_post = as_draws_df(mod) %>% 
  reframe(int = median(b_Intercept),
          int_lower = quantile(b_Intercept, probs = 0.025),
          int_upper = quantile(b_Intercept, probs = 0.975),
          slope = median(b_x_s),
          slope_lower = quantile(b_x_s, probs = 0.025),
          slope_upper = quantile(b_x_s, probs = 0.975))

# combine
hybas_predictions = flux_predictions_all %>% 
  # slice(1:10) %>% 
  right_join(selenium_predictions_all) %>% 
  mutate(x_s = (log_water_se_ug_l - mean_x)/sd_x) %>% 
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

se_hybas_to_plot = hybas_predictions %>% select(HYBAS_ID, mean, starts_with("chem_flux_mg_year")) %>% 
  rename(bug_flux_kg_year = mean)

hybas_predictions %>%
  filter(x_s == min(x_s, na.rm = T)|x_s == max(x_s, na.rm = T)) %>% 
  bind_rows(hybas_predictions %>%
              arrange(x_s) %>% 
              slice_sample(n = 70000)) %>% 
  # sample_n(80000) %>%
  ggplot(aes(x = x_s, y = y)) +
  geom_line() +
  geom_ribbon(aes(ymin = y_lower, ymax = y_upper), alpha = 0.2) +
  geom_point(data = mod$data ,
             aes(y = y_s*max_adult_conc,
                 x = x_s), color = 'red') +
  # scale_y_log10() +
  NULL

sum(hybas_predictions$chem_flux_mg_year_lower, na.rm = T)/1e06
sum(hybas_predictions$chem_flux_mg_year, na.rm = T)/1e06
sum(hybas_predictions$chem_flux_mg_year_upper, na.rm = T)/1e06
