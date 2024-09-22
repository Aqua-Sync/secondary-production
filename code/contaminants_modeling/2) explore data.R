library(tidyverse)
library(brms)
library(janitor)
library(tidybayes)
library(viridis)
library(scales)

# load data
contaminants = readRDS(file = "data/contaminants.rds") 

# number of records per category
contaminants %>% 
  pivot_longer(cols = c(water_conc_ugl, sediment_conc_ugg, adult_conc_ng_mg_dm,
                        emergence_mgdm_m2_y)) %>% 
  group_by(chemical, name) %>%
  filter(!is.na(value)) %>% 
  tally() %>% 
  arrange(-n) %>% 
  pivot_wider(names_from = name, values_from = n)


# check metals

contaminants %>% 
  ggplot(aes(x = chemical, y = adult_conc_ng_mg_dm)) + 
  geom_jitter() +
  facet_wrap(~chemical_category_broad, scales = "free")

contaminants %>% 
  filter(chemical_category_broad == "metal") %>% 
  filter(!is.na(water_conc_ugl)) %>% 
  filter(!is.na(adult_conc_ng_mg_dm)) %>% 
  group_by(chemical) %>% 
  mutate(water_s = water_conc_ugl/mean(water_conc_ugl),
         adult_s = adult_conc_ng_mg_dm/mean(adult_conc_ng_mg_dm)) %>% 
  ggplot(aes(x = water_s, y = adult_s)) + 
  geom_point(aes(color = chemical)) +
  scale_x_log10() +
  scale_y_log10() +
  NULL


contaminants %>% 
  # filter(chemical_category_broad == "metal") %>% 
  # filter(water_conc_ugl <= 1) %>% 
  ggplot(aes(x = water_conc_ugl, y = adult_conc_ng_mg_dm)) + 
  geom_point(aes(color = chemical)) +
  geom_smooth(method = lm) +
  scale_x_log10() +
  scale_y_log10() +
  guides(color = "none") +
  facet_wrap(~chemical_category_broad, scales = "free") +
  NULL

contaminants %>% 
  # filter(chemical_category_broad == "metal") %>% 
  # filter(water_conc_ugl <= 1) %>% 
  filter(chemical == "Zn") %>% 
  ggplot(aes(x = water_conc_ugl, y = adult_conc_ng_mg_dm)) + 
  geom_point(aes(color = chemical)) +
  geom_smooth(method = lm) +
  scale_x_log10() +
  scale_y_log10() +
  guides(color = "none") +
  facet_wrap(~chemical_category_broad, scales = "free") +
  NULL

contaminants %>% 
  # filter(chemical_category_broad == "metal") %>% 
  # filter(water_conc_ugl <= 1) %>% 
  filter(chemical == "Zn") %>% 
  select(pub_name, pub_number, adult_conc_ng_mg_dm, water_conc_ugl) %>% 
  View()

contaminants %>% 
  # filter(chemical_category_broad == "metal") %>% 
  ggplot(aes(x = sediment_conc_ugg, y = adult_conc_ng_mg_dm)) + 
  geom_point(aes(color = chemical)) +
  geom_smooth(method = lm) +
  scale_x_log10() +
  scale_y_log10() +
  guides(color = "none") +
  facet_wrap(~chemical_category_broad, scales = "free") +
  # facet_wrap(~chemical) +
  NULL

contaminants %>% 
  # filter(chemical_category_broad == "metal") %>% 
  ggplot(aes(x = sediment_conc_ugg, y = water_conc_ugl)) + 
  geom_point(aes(color = chemical)) +
  geom_smooth(method = lm) +
  scale_x_log10() +
  scale_y_log10() +
  guides(color = "none") +
  facet_wrap(~chemical_category_broad, scales = "free") +
  # facet_wrap(~chemical) +
  NULL

contaminants %>% 
  filter(chemical_category_broad == "metal") %>% 
  arrange(adult_conc_ng_mg_dm) %>% 
  ggplot(aes(x = chemical, y = adult_conc_ng_mg_dm, 
             color = chemical)) + 
  geom_jitter() +
  scale_y_log10()
