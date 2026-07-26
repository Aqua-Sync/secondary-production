library(tidyverse)
library(brms)
library(tidybayes)


# Table S2: Global contaminant fluxes -------------------------------------

global_predictions_metals = readRDS(file = "posteriors/global_predictions_metals.rds")

total_metals = bind_rows(global_predictions_metals) %>% 
  group_by(.draw) %>% 
  reframe(global_flux_MT_peryr = sum(global_flux_MT_peryr)) %>% 
  median_qi(global_flux_MT_peryr) %>% 
  mutate(chemical = "Total Metals")

total_essential_nonessential =  bind_rows(global_predictions_metals) %>% 
  mutate(essential = case_when(chemical %in% c("Cu", "Zn", "Se") ~ "Essential", T ~ "Non-essential")) %>% 
  group_by(.draw, essential) %>% 
  reframe(global_flux_MT_peryr = sum(global_flux_MT_peryr)) %>% 
  group_by(essential) %>% 
  median_qi(global_flux_MT_peryr) %>%
  mutate(across(where(is.double), ~ janitor::signif_half_up(.x, 2))) 

total_per_chemical =  bind_rows(global_predictions_metals) %>% 
  mutate(essential = case_when(chemical %in% c("Cu", "Zn", "Se") ~ "Essential", T ~ "Non-essential")) %>% 
  group_by(.draw, chemical) %>% 
  reframe(global_flux_MT_peryr = sum(global_flux_MT_peryr)) %>% 
  group_by(chemical) %>% 
  median_qi(global_flux_MT_peryr) %>%
  mutate(across(where(is.double), ~ janitor::signif_half_up(.x, 2))) 

# Global Annual Metric Tons
global_metal_flux = bind_rows(global_predictions_metals) %>% 
  group_by(chemical) %>% 
  median_qi(global_flux_MT_peryr) %>% 
  arrange(-global_flux_MT_peryr) %>% 
  bind_rows(total_metals) %>%
  mutate(across(where(is.double), ~ janitor::signif_half_up(.x, 2))) 

global_predictions_pest_herb_fungicide = bind_rows(readRDS(file = "posteriors/global_predictions_pest_herb_fungicide.rds"))

# Global Annual Metric Tons
global_pesticide_flux = global_predictions_pest_herb_fungicide %>% 
  group_by(chemical) %>% 
  median_qi(global_flux_MT_peryr) %>% 
  bind_rows(global_predictions_pest_herb_fungicide %>% 
              group_by(.draw) %>% 
              reframe(global_flux_MT_peryr = sum(global_flux_MT_peryr)) %>% 
              median_qi(global_flux_MT_peryr) %>% 
              mutate(chemical = "Total")) %>% 
  mutate(chemical = fct_relevel(chemical, "herbicide", "insecticide", "fungicide")) %>% 
  arrange(chemical) %>%
  mutate(across(where(is.double), ~ janitor::signif_half_up(.x, 1))) 

global_metal_pesticide = bind_rows(global_metal_flux, global_pesticide_flux)

write_csv(global_metal_pesticide, file = "tables/table_s2.csv")
