library(tidyverse)
library(brms)
library(tidybayes)

raw_contaminants = readRDS(file = "data/contaminants.rds") %>%
  group_by(chemical_category) %>% 
  mutate(max_y = max(adult_conc_ng_mg_dm,na.rm = T),
         y_s = adult_conc_ng_mg_dm/max_y,
         mean_water_conc_001 = 0.001*mean(water_conc_ug_l, na.rm = T),
         log_water_conc_ugl_01 = log(water_conc_ug_l + mean_water_conc_001),
         x_s = scale(log_water_conc_ugl_01))

cont_corrections = raw_contaminants %>% 
  group_by(chemical_category) %>% 
  mutate(water_conc_ug_l = water_conc_ug_l + 0.001*mean(water_conc_ug_l, na.rm = T),
         min_water_conc_ugl_05 = 0.5*min(water_conc_ug_l, na.rm = T),
         mean_log_water = mean(log(water_conc_ug_l), na.rm = T),
         sd_log_water = sd(log(water_conc_ug_l), na.rm = T),
         min_x_s05 = (log(min_water_conc_ugl_05) - mean_log_water)/sd_log_water) %>% 
  ungroup %>% 
  distinct(chemical_category, max_y,  min_x_s05)

baseline_list = list()

for(i in 1:length(mod_list)){
  baseline_list[[i]] = cont_corrections %>% 
    filter(chemical_category == mod_list[[i]]$data2$chemical_category) %>% 
    mutate(pub_name = "new",
           y_max = mod_list[[i]]$data2$max_y,
           x_s = min_x_s05) %>% 
    add_epred_draws(mod_list[[i]], allow_new_levels = T) %>% 
    mutate(.epred = .epred*y_max)
  
}

baseline_values = bind_rows(baseline_list) %>% 
  # filter(chemical_category %in% c("Se", "Cu", "Zn")) %>% 
  group_by(chemical_category) %>% 
  median_qi(.epred) %>% 
  rename(tissue_ng_mg = .epred) %>% 
  mutate(tissue_ng_mg = round(tissue_ng_mg, 1),
         .lower = round(.lower, 1),
         .upper = round(.upper, 1)) %>% 
  pivot_longer(cols = c(tissue_ng_mg, .lower, .upper)) %>% 
  mutate(value = case_when(chemical_category %in% c("Se", "Cu", "Zn") ~ value, TRUE ~ 0)) %>% 
  pivot_wider(names_from = name, values_from = value)
