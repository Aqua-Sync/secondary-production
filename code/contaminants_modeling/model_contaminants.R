library(tidyverse)
library(brms)
library(janitor)
library(tidybayes)
library(viridis)
library(scales)

contaminants = read_csv("data/Overviewavailabledata-2024-1-9.csv") %>% 
  clean_names() %>% 
  mutate(adults_ng_mg_dm = parse_number(adults_ng_mg_dm),
         emergence_mg_m2_y = parse_number(emergence_mg_m2_y),
         water_ug_l = parse_number(water_ug_l),
         adults_ng_mg_dm_s = adults_ng_mg_dm/mean(adults_ng_mg_dm, na.rm = T),
         log10_water_ug_l = log10(water_ug_l))

contaminants %>% 
  ggplot(aes(y = reorder(chemical, adults_ng_mg_dm), x = adults_ng_mg_dm)) +
  geom_point() +
  scale_x_log10()

contaminants %>% 
  filter(grepl("Hg", chemical))

contaminants %>% 
  ggplot(aes(x = water_ug_l, adults_ng_mg_dm)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method = lm) 

contaminants %>% 
  group_by(chemical) %>% 
  tally() %>% 
  arrange(-n)

contaminants %>% 
  filter(chemical == "Thiacloprid") %>% 
  ggplot(aes(x = water_ug_l, y = adults_ng_mg_dm)) + 
  geom_point() +
  scale_x_log10() +
  # scale_y_log10() +
  NULL

# Thiacloprid model

# try with log10(water_ug_l) to match Jakob's modeling. Jakob is estimating log10(water_ug_l) for Europe.
brm_thiac = brm(adults_ng_mg_dm ~ water_ug_l,
                family = Gamma(link = "log"),
                data = contaminants %>% filter(chemical == "Thiacloprid"))

saveRDS(brm_thiac, file = "models/brm_thiac.rds")

test = plot(conditional_effects(brm_thiac), points = T)

test$water_ug_l + scale_x_log10() +
  scale_y_log10()

