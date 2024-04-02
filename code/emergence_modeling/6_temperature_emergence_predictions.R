library(tidyverse)

# load bug data and model
emergence_production = readRDS(file = "data/emergence_production_bioclim.rds")
temp_mean_sd = attributes(emergence_production$temp_s)
brm_emerge_temp = readRDS("models/brm_emerge_temp.rds")


# Get mean temps for each basin
RSSAtavg <- read_csv("data/RSSAtavg.csv") %>%
  pivot_longer(cols = starts_with("mean"), values_to = "temp_celsius") %>% 
  group_by(BAS_ID) %>% 
  mutate(temp_celsius_s = (temp_celsius- temp_mean_sd$`scaled:center`)/temp_mean_sd$`scaled:scale`) 

rssa_temps = RSSAtavg %>% group_by(BAS_ID) %>% 
  reframe(mean_temp_celsius = mean(temp_celsius),
          mean_temp_celsius_s = mean(temp_celsius_s)) %>% 
  select(BAS_ID, mean_temp_celsius, mean_temp_celsius_s)

# add temps to the original hydrobasin dataset    
    
basin_temps =
  allen_area = read_csv("data/HydroBasin_RSSA_02192024.csv") %>% 
  mutate(mnRSSA_prop = mnRSSA_pc/100,
         sdRSSA_prop = sdRSSA_pc/100) %>% 
  mutate(water_km2 = BA_km2*mnRSSA_prop,
         water_km2_sd = BA_km2*sdRSSA_prop) %>% 
  left_join(rssa_temps) %>% 
  mutate(water_lower = water_km2 - 1.96*water_km2_sd,
         water_upper = water_km2 + 1.96*water_km2_sd)

# extract posterior predictions for each basin

post_preds = basin_temps %>% distinct(BAS_ID, mean_temp_celsius_s, mean_temp_celsius) %>% 
  mutate(temp_s = mean_temp_celsius_s) %>% 
  # sample_n(size = 20) %>% 
  mutate(author_year = "new",
         sd_emergence_kg = 1) %>% # placeholder. This doesn't affect the results 
  add_predicted_draws(brm_emerge_temp, re_formula = NULL, allow_new_levels = T, ndraws = 500) %>% 
  mutate(.prediction = .prediction*1e+06) # convert from kgm2dmy to kgkm2dmy


post_summary = post_preds %>% 
  group_by(BAS_ID, temp_s, mean_temp_celsius) %>% 
  reframe(mean_emerge = mean(.prediction),
          sd_emerge = sd(.prediction)) %>% 
  left_join(basin_temps %>% distinct(BAS_ID, water_km2, water_km2_sd)) %>% 
  mutate(mean_emerge_perbasin_kgdmy = water_km2*mean_emerge,
         var_emerge = sd_emerge^2,
         var_water = water_km2_sd^2,
         var_water_emerge = var_emerge*var_water,
         sd_emerge_perbasin_kgdmy = sqrt(var_water_emerge))

write_csv(post_summary, file = "data/allen_area_emergence_tempadjusted.csv")

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
  reframe(global_kgdmy = sum(mean_emerge_perbasin_kgdmy, na.rm = T))


global_flux %>% 
  reframe(median_MTdmy = median(global_kgdmy/1000),
          mean_MTdmy = mean(global_kgdmy/1000),
          sd_MTdmy = sd(global_kgdmy/1000))


total_kgdmy %>% 
  ggplot(aes(x = kgdmy)) + 
  # geom_histogram(bins = 100) +
  stat_slabinterval() +
  scale_x_log10(limits = c(1e07, 3e09)) +
  labs(x = "Global Emergence (kg/y)") +
  stat_slabinterval(data = global_flux, aes(x = global_kgdmy),
                    fill = "blue", alpha = 0.4)
