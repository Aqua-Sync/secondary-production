library(tidyverse)
library(tidybayes)

# load bug data and model
emergence_production = readRDS(file = "data/emergence_production_bioclim.rds")
temp_mean_sd = attributes(emergence_production$temp_s)

fit_gam = readRDS("models/fit_gam_intercept.rds")

estimate_streamtemp = function(x){
  numerator = 26.2                           # values are median parameter values from Mohseni et al. Tbl X.
  denominator = 1 + (exp(0.18*(13.3 - x)))
  0.8 + (numerator/denominator)
}

# Get mean temps for each basin
RSSAtavg <- read_csv("data/RSSAtavg.csv") %>%
  pivot_longer(cols = starts_with("mean"), values_to = "temp_celsius") %>% 
  group_by(BAS_ID) %>% 
  mutate(temp_celsius_s = (temp_celsius- temp_mean_sd$`scaled:center`)/temp_mean_sd$`scaled:scale`) 

# no duplicate HYBAS_ID - good.
read_csv("data/RSSA_Basin_HydroshedsAirTemp.csv") %>% 
  group_by(HYBAS_ID) %>% 
  tally() %>% 
  filter(n > 1)

RSSAtavg <- read_csv("data/RSSA_Basin_HydroshedsAirTemp.csv") %>%
  # pivot_longer(cols = starts_with("mean"), values_to = "temp_celsius") %>% 
  group_by(BAS_ID) %>%
  mutate(temp_celsius_s = (tmp_dc_syr - temp_mean_sd$`scaled:center`)/temp_mean_sd$`scaled:scale`) 

# rssa_temps = RSSAtavg %>% group_by(BAS_ID) %>% 
#   reframe(mean_temp_celsius = mean(temp_celsius),
#           mean_temp_celsius_s = mean(temp_celsius_s)) %>% 
#   select(BAS_ID, mean_temp_celsius, mean_temp_celsius_s)

rssa_temps = RSSAtavg %>% 
  select(BAS_ID, HYBAS_ID, tmp_dc_syr) %>% 
  mutate(water_temp = estimate_streamtemp(tmp_dc_syr),
         stream_temp20 = water_temp/20)

# add temps to the original hydrobasin dataset    

# RSSA_Basin_HydroshedsAirTemp <- read_csv("data/RSSA_Basin_HydroshedsAirTemp.csv")

basin_temps = read_csv("data/HydroBasin_RSSA_02192024.csv") %>% 
  mutate(mnRSSA_prop = mnRSSA_pc/100,
         sdRSSA_prop = sdRSSA_pc/100) %>% 
  mutate(water_km2 = BA_km2*mnRSSA_prop,
         water_km2_sd = BA_km2*sdRSSA_prop) %>% 
  right_join(rssa_temps) %>% 
  mutate(water_lower = water_km2 - 1.96*water_km2_sd,
         water_upper = water_km2 + 1.96*water_km2_sd)

# extract posterior predictions for each basin

post_preds = basin_temps %>% 
  distinct(BAS_ID, HYBAS_ID, water_km2, water_km2_sd, stream_temp20) %>%
  sample_n(1000) %>% 
  
  fit_gam$data %>% 
  distinct(BAS_ID) %>% 
  mutate(author_year = "new") %>% # placeholder. This doesn't affect the results 
  add_predicted_draws(fit_gam, re_formula = NULL, allow_new_levels = T, ndraws = 500) %>% 
  mutate(.prediction = .prediction*max(emergence_production$mean_emergence_mgdmm2y, na.rm = T)) %>% 
  mutate(units = "kgkm2dmy")

post_preds %>% 
  ggplot(aes(x = stream_temp20*20, y = .prediction)) + 
  stat_pointinterval(.width = 0.5, linewidth = 0.1, aes(color = water_km2,
                                                      group = HYBAS_ID)) +
  guides(color = "none") +
  # scale_y_log10() +
  NULL


post_summary = post_preds %>% 
  group_by(BAS_ID, stream_temp20, HYBAS_ID, water_km2, water_km2_sd) %>% 
  reframe(mean_emerge = median(.prediction),
          sd_emerge = sd(.prediction)) %>% 
  # left_join(basin_temps %>% distinct(BAS_ID, water_km2, water_km2_sd)) %>% 
  mutate(mean_emerge_perbasin_kgdmy = water_km2*mean_emerge,
         var_emerge = sd_emerge^2,
         var_water = water_km2_sd^2,
         var_water_emerge = var_emerge*var_water,
         sd_emerge_perbasin_kgdmy = sqrt(var_water_emerge)) %>% 
  mutate(temp_celsius = stream_temp20*20)

post_summary %>% 
  ggplot(aes(x = stream_temp20*20, y = mean_emerge_perbasin_kgdmy)) + 
  geom_point(aes(color = water_km2)) +
  geom_linerange(aes(ymin = mean_emerge - sd_emerge,
                     ymax = mean_emerge + sd_emerge)) +
  # scale_y_log10() +
  guides(color = "none") +
  # geom_point(data = fit_gam$data %>% mutate(mean_emerge = emerge_1*max(emergence_production$mean_emergence_mgdmm2y, na.rm = T)),
             # color = "yellow") +
  NULL

plot(conditional_effects(brm_emerge_temp, method = "predict"), points = T)

write_csv(post_summary, file = "data/allen_area_emergence_tempadjusted.csv")

correct_for_zeros = readRDS(file = "data/correct_for_zeros.rds")

post_summary %>%
  mutate(mean = mean_emerge_perbasin_kgdmy + correct_for_zeros,
         upper = mean_emerge_perbasin_kgdmy + sd_emerge_perbasin_kgdmy,
         lower = mean_emerge_perbasin_kgdmy - sd_emerge_perbasin_kgdmy) %>% 
  ggplot(aes(y = reorder(BAS_ID, mean_emerge_perbasin_kgdmy),
             x = mean_emerge_perbasin_kgdmy)) +
  geom_linerange(aes(xmin = lower,
                     xmax = upper),
                 linewidth = 0.01,
                 color = "gray80") + 
  geom_point(color = "black",
             size = 0.01) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_x_log10() +
  labs(y = "BAS_ID (ranked by mean emergence)",
       x = "Annual Emergence (kgDM/y)",
       title = "Annual Emergence from 47,179 drainage basins",
       subtitle = "mean +/- sd")

# calculate total flux
global_flux = post_preds %>% 
  left_join(basin_temps %>% distinct(BAS_ID, water_km2, water_km2_sd, water_lower, water_upper)) %>% 
  mutate(mean_emerge_perbasin_kgdmy = water_km2*.prediction,
         lower_emerge_perbasin_kgdmy = water_lower*.prediction,
         upper_emerge_perbasin_kgdmy = water_upper*.prediction) %>% 
  group_by(.draw) %>% 
  reframe(global_kgdmy = sum(mean_emerge_perbasin_kgdmy, na.rm = T),
          global_MTdmy = global_kgdmy/1000)

saveRDS(global_flux, file = "posteriors/global_flux.rds")


global_flux = readRDS(file = "posteriors/global_flux.rds")

global_flux %>% 
  reframe(median_MTdmy = median(global_kgdmy/1000),
          mean_MTdmy = mean(global_kgdmy/1000),
          sd_MTdmy = sd(global_kgdmy/1000),
          low95_MTdmy = quantile(global_kgdmy/1000, probs = 0.025),
          high95_MTdmy = quantile(global_kgdmy/1000, probs = 0.975)) %>% 
  pivot_longer(everything())


total_kgdmy %>% 
  ggplot(aes(x = kgdmy)) + 
  # geom_histogram(bins = 100) +
  stat_slabinterval() +
  scale_x_log10(limits = c(1e07, 3e09)) +
  labs(x = "Global Emergence (kg/y)") +
  stat_slabinterval(data = global_flux, aes(x = global_kgdmy),
                    fill = "blue", alpha = 0.4)
