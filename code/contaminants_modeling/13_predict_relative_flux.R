library(tidyverse)
library(janitor)


hybas_regions_centroids <- readRDS("data/hybas_regions_centroids.rds")

# 1) load posteriors for each contaminant
hybas_predictions_mgperyear_fungicide <- readRDS("posteriors/hybas_predictions_mgperyear_filtered_fungicide.rds") %>% 
  select(HYBAS_ID, starts_with("chem_flux"), element)
hybas_predictions_mgperyear_herbicide <- readRDS("posteriors/hybas_predictions_mgperyear_filtered_herbicide.rds") %>% 
  select(HYBAS_ID, starts_with("chem_flux"), element)
hybas_predictions_mgperyear_insecticide <- readRDS("posteriors/hybas_predictions_mgperyear_filtered_insecticide.rds") %>% 
  select(HYBAS_ID, starts_with("chem_flux"), element)
hybas_predictions_nonessentialmetals <- readRDS("posteriors/hybas_predictions_metals.rds") %>% 
  select(HYBAS_ID, starts_with("chem_flux"), element) %>% 
  filter(element %in% c("Cd", "Pb", "Hg")) # filter to only non-essential metals

hybas_covariates = readRDS("data/hybas_covariates.rds")

# 1) load posteriors for each contaminant and bind them
# Note, we only need to work with dry mass and PUFAs, because C/N/P are linear transformations of dry mass so are redundant for calculating a proportion.
hybas_bind_nutrients = bind_rows(readRDS("posteriors/hybas_predictions_kgPUFA_peryear.rds"),
                                 readRDS("posteriors/hybas_predictions_kgdm_peryear.rds")) %>% 
  left_join(readRDS("data/hybas_regions.rds"))

hybas_bind_contaminants = bind_rows(hybas_predictions_mgperyear_fungicide,
                                    hybas_predictions_mgperyear_herbicide,
                                    hybas_predictions_mgperyear_insecticide,
                                    hybas_predictions_nonessentialmetals) %>% 
  left_join(readRDS("data/hybas_regions.rds") %>% mutate(HYBAS_ID = as.character(HYBAS_ID)))

sum_contaminants = hybas_bind_contaminants %>% 
  group_by(HYBAS_ID) %>% 
  reframe(sum_contaminants = sum(chem_flux_mg_year, na.rm = T))

sum_nonessential = hybas_predictions_nonessentialmetals %>% 
  group_by(HYBAS_ID) %>% 
  reframe(sum_nonessential = sum(chem_flux_mg_year, na.rm = T))

sum_pesticides = bind_rows(hybas_predictions_mgperyear_fungicide,
                           hybas_predictions_mgperyear_herbicide,
                           hybas_predictions_mgperyear_insecticide) %>% 
  group_by(HYBAS_ID) %>% 
  reframe(sum_pesticides = sum(chem_flux_mg_year, na.rm = T))

sum_nutrients = hybas_bind_nutrients %>% 
  group_by(HYBAS_ID) %>% 
  reframe(sum_nutrients = sum(median, na.rm = T))

prop_contaminants = sum_contaminants %>% 
  mutate(prop_contaminants = sum_contaminants/sum(sum_contaminants, na.rm = T)) %>% 
  mutate(HYBAS_ID = as.numeric(HYBAS_ID))

prop_nonessential = sum_nonessential %>% 
  mutate(prop_nonessential = sum_nonessential/sum(sum_nonessential, na.rm = T)) %>% 
  mutate(HYBAS_ID = as.numeric(HYBAS_ID))

prop_nutrients = sum_nutrients %>% 
  mutate(prop_nutrients = sum_nutrients/sum(sum_nutrients, na.rm = T)) %>% 
  mutate(HYBAS_ID = as.numeric(HYBAS_ID))

prop_pesticides = sum_pesticides %>% 
  mutate(prop_pesticides = sum_pesticides/sum(sum_pesticides, na.rm = T)) %>% 
  mutate(HYBAS_ID = as.numeric(HYBAS_ID))

nut_cont_diffs = left_join(prop_nutrients, prop_contaminants) %>% 
  mutate(diff = prop_nutrients - prop_contaminants) %>% 
  left_join(hybas_regions_centroids)

nut_nonessential_diffs = left_join(prop_nutrients, prop_nonessential) %>% 
  mutate(diff = prop_nutrients - prop_nonessential) %>% 
  left_join(hybas_regions_centroids)

nut_pesticide_diffs = left_join(prop_nutrients, prop_pesticides) %>% 
  mutate(diff = prop_nutrients - prop_pesticides) %>% 
  left_join(hybas_regions_centroids)


saveRDS(nut_pesticide_diffs, file = "posteriors/nut_pesticide_diffs.rds")
saveRDS(nut_nonessential_diffs, file = "posteriors/nut_nonessential_diffs.rds")
saveRDS(nut_cont_diffs, file = "posteriors/nut_contaminants_diffs.rds")
