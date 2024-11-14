library(tidyverse)

# load data
flux_predictions_all = readRDS("posteriors/flux_predictions_all.rds")
selenium_predictions_all = readRDS("data/selenium_jeff.rds") %>% rename(HYBAS_ID = HYBAS_L12) %>% 
  mutate(HYBAS_ID = as.numeric(HYBAS_ID),
         se_units = "log10_ug_l",
         se_corrected = mean.conc.year*mean.det.year) %>% 
  mutate(water_se_ug_l = exp(se_corrected))

# get parameters
cont_s = readRDS(file = "data/cont_s.rds")
brm_se = readRDS(file = "models/brm_se.rds")
mean_se = cont_s %>% filter(chemical == "se") %>% filter(!is.na(log_water_conc_ugl)) %>% 
  reframe(mean = mean(log_water_conc_ugl)) %>% pull()
sd_se = cont_s %>% filter(chemical == "se") %>% filter(!is.na(log_water_conc_ugl)) %>% 
  reframe(mean = sd(log_water_conc_ugl)) %>% pull()
max_bug_se = cont_s %>% filter(chemical == "se") %>% filter(adult_conc_ng_mg_dm == max(adult_conc_ng_mg_dm)) %>% 
  distinct(adult_conc_ng_mg_dm) %>% pull(adult_conc_ng_mg_dm)

int_slope_post = as_draws_df(brm_se) %>% 
  reframe(int = mean(b_Intercept),
          slope = mean(b_x_s))


# combine
se_hybas_predictions = flux_predictions_all %>% 
  # slice(1:10) %>% 
  left_join(selenium_predictions_all) %>% 
  mutate(x_s = (se_corrected*sd_se) + mean_se) %>% 
  mutate(x_s = case_when(is.na(x_s) ~ 0.00001*mean_se,
                         TRUE ~ x_s)) %>% 
  # select(HYBAS_ID, x_s) %>% 
  mutate(int = int_slope_post$int,
         slope = int_slope_post$slope) %>% 
  mutate(y_s = exp(int + slope*x_s)) %>% 
  mutate(y = y_s*max_bug_se,
         element = "se",
         bug_conc_units = "ng_mg_dm") %>% 
  mutate(y_mg_kg = y) %>% 
  mutate(chem_flux_mg_year = y_mg_kg*mean)

se_hybas_to_plot = se_hybas_predictions %>% select(HYBAS_ID, mean, chem_flux_mg_year) %>% 
  rename(bug_flux_kg_year = mean)

saveRDS(se_hybas_to_plot, file = "data/se_hybas_to_plot.rds")

se_hybas_predictions %>% 
  sample_n(10000) %>% 
  ggplot(aes(x = x_s, y = y)) +
  geom_point() +
  geom_point(data = cont_s %>% filter(chemical == "se"),
             aes(y = adult_conc_ng_mg_dm), color = 'red')

sum(se_hybas_predictions$chem_flux_mg_year)/1e06

cont_s %>% filter(chemical == "se") %>% 
  reframe(mean = median(adult_conc_ng_mg_dm))
