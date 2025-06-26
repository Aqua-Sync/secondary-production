library(tidyverse)
library(brms)
library(janitor)
library(tidybayes)
library(scales)
theme_set(theme_default())

# Get ranges of aqueous contaminant concentrations globally and compare to the
# range of contaminants in the HYBAS for which we have emergence measured. This 
# provides a glimpse of the amount of mortality-driven contaminant effects that might
# already be accounted for in our estimates of emergence.

cas_names = readRDS("C:/Users/jeff.wesner/OneDrive - The University of South Dakota/USD/Github Projects/secondary-production/data/cas_names.rds")
modeled_water = as_tibble(readRDS(file = "data/modeled_water.rds")) %>% # values have been corrected for minimums with essential elements (i.e., if water concentrations indicate zero Se but still has emergence, then we need to assign a minimum amount to flux b/c flux of Se in tissues can't also be zero)
  left_join(cas_names) %>% 
  mutate(water_ug_l_raw = 10^(mean.conc.year * mean.det.year)) %>% 
  filter(chemical != "Propyzamide")

# load raw emergence
emergence = readRDS(file = 'data/emergence_production_with_vars.rds') %>% 
  left_join(readRDS("data/HYBAS_surface_area_REDIST.rds")) %>% # add area of HYBAS water
  mutate(raw_kg_perhybas = mean_emergence_mgdmm2y*area.redist)  # kg/km2 is the same as mg/m2 so this works to produce kg per hybas

modeled_water_wide = modeled_water %>% 
  filter(HYBAS_ID %in% unique(emergence$HYBAS_ID)) %>% 
  select(water_ug_l_raw, HYBAS_ID, chemical) 

emergence %>% 
  select(HYBAS_ID, raw_kg_perhybas) %>% 
  left_join(modeled_water_wide, relationship = "many-to-many") 

modeled_water_global_empirical = modeled_water %>% 
  select(water_ug_l_raw, HYBAS_ID, chemical) %>% 
  mutate(data = "Global Range") %>% 
  bind_rows(modeled_water_wide %>% 
              mutate(data = "Range at sites with emergence"))

modeled_water_ranges = modeled_water_global_empirical %>% 
  group_by(chemical, data) %>% 
  filter(water_ug_l_raw == max(water_ug_l_raw, na.rm = T) | water_ug_l_raw == min(water_ug_l_raw, na.rm = T)) %>% 
  distinct(water_ug_l_raw, chemical, data) %>% 
  mutate(chem = str_sub(chemical, 1, 10)) %>%
  # filter(chemical == "Zinc") %>% 
  group_by(chemical) %>% 
  mutate(water_01 = water_ug_l_raw/max(water_ug_l_raw, na.rm = T)) 

modeled_water_orders = modeled_water_ranges %>% 
  filter(data == "Range at sites with emergence") %>% 
  group_by(chemical) %>% 
  filter(water_01 == max(water_01)) %>% 
  rename(order = water_01) %>% 
  distinct(chemical, order)

water_range = modeled_water_ranges %>%
  left_join(modeled_water_orders) %>% 
  ggplot(aes(x = reorder(chem, order), y = water_01)) +
  geom_line(position = position_dodge(width = 0.4),
            aes(color = data)) +
  coord_flip() +
  labs(color = "Water Concentration Range",
       x = "Chemical",
       y = "Water concentrations\nscaled to min/max)") +
  scale_color_brewer(type = "qual", palette = 3) +
  theme(legend.title = element_text(size = 9),
        legend.text = element_text(size = 8))

ggsave(water_range, file = "plots/water_range.jpg", width= 6.5, height = 6)
