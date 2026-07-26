library(tidyverse)
library(brms)
library(janitor)
library(tidybayes)
library(viridis)
library(scales)
library(readxl)

# 1) load data
contaminants_raw_initial = read_csv("data/AquaSync-Contaminant_transfer-2024-2-21_JMK(main_data_good_names).csv") %>% 
  clean_names() %>% 
  filter(if_any(everything(), ~ !is.na(.))) %>%  # remove empty rows
  ungroup %>% 
  mutate(row_id = row_number()) %>% 
  select(!starts_with("x")) %>% 
  mutate(chemical = str_to_lower(chemical)) 

# 2) convert range measures to the centroid
contaminants_with_range = contaminants_raw_initial %>% 
  filter(sediment_conc_measure_type == "range") %>% 
  separate(sediment_conc, into = c("low", "high"), sep = "-") %>% 
  mutate(low = parse_number(low),
         high = parse_number(high)) %>% 
  mutate(sediment_conc_ugg = as.character((low + high)/2)) %>% 
  select(-low, -high)

# 3) add range fix back to contaminants
contaminants_raw = contaminants_raw_initial %>% 
  filter(!row_id %in% unique(contaminants_with_range$row_id)) %>% 
  bind_rows(contaminants_with_range)

# 4) wrangle
# This code removes characters from concentration columns (e.g., "35.2 (ng/l)" becomes "35.2"). 
# It then puts the units in a new column and harmonizes them to the same units. 
contaminants_unit_harmonized = contaminants_raw %>% 
  mutate(water_conc_num = parse_number(water_conc),
         adult_conc_num = parse_number(adult_conc),
         sediment_conc_num = parse_number(sediment_conc)) %>% 
  mutate(adult_conc_ng_mg_dm = case_when(adult_units == "ug_g" ~ adult_conc_num*1,
                           adult_units == "µg_g" ~ adult_conc_num*1,
                           adult_units == "mg_kg" ~ adult_conc_num*1,
                           adult_units == "mg_g" ~ adult_conc_num*1000,
                           adult_units == "mg_g_dm" ~ adult_conc_num*1000,
                           adult_units == "ppm_dw" ~ adult_conc_num*1,
                           adult_units == "ng_g_dm" ~ adult_conc_num/1000,
                           TRUE ~ adult_conc_num)) %>% 
  mutate(water_conc_ug_l = case_when(water_units == "mgl" ~ water_conc_num*1000,
                                         water_units == "ngl" ~ water_conc_num/1000,
                                         TRUE ~ water_conc_num)) %>% 
  mutate(sediment_conc_ug_g = case_when(sediment_units == "mgg" ~ sediment_conc_num*1000,
                                     sediment_units == "ngg" ~ sediment_conc_num/1000,
                                     TRUE ~ sediment_conc_num))%>% 
  mutate(chemical_category = case_when(chemical_category == "legacy insecticide" ~ "insecticide",
                                       chemical == "propyzamide" ~ "herbicide",
                                       TRUE ~ chemical_category),
         chemical = str_remove(chemical, " \\(sum of isomers\\)"),
         chemical = case_when(chemical == "fluopicolid" ~ "fluopicolide",  # matching names in the data with official cas names
                              chemical == "p,p'-ddd" ~ "p,p′-DDD",
                              chemical == "primicarb" ~ "pirimicarb",
                              TRUE ~ chemical)) %>% 
  mutate(pub_name = case_when(grepl("Silina et al. Emergence ", pub_name) ~ "Silina et al. 2023. Emergence of Amphibious Insects from an Old Beaver Pond",
                              TRUE ~ pub_name))


saveRDS(contaminants_unit_harmonized, file = "data/contaminants.rds")

# describe contaminants data (remove PFAS first)

cont_nopfas = contaminants_unit_harmonized %>% filter(chemical_category != "PFAS")

sort(unique(cont_nopfas$chemical))
length(unique(cont_nopfas$chemical))

sort(unique(cont_nopfas$chemical_category))
length(unique(cont_nopfas$chemical_category))

sort(unique(cont_nopfas$pub_name))
length(unique(cont_nopfas$pub_name))

# PUFA --------------------------------------------------------------------

library(tidyverse)
library(brms)
library(janitor)
library(tidybayes)
library(viridis)
library(scales)
library(readxl)

# 1) load data
pufa_raw_initial = read_csv("data/AquaSync-Contaminant_transfer-2024-2-21_JMK(pufa).csv") %>% 
  clean_names() %>% 
  ungroup %>% 
  mutate(row_id = row_number()) %>% 
  select(!starts_with("x")) %>% 
  mutate(chemical = str_to_lower(chemical)) 

# 2) convert range measures to the centroid
pufa_with_range = pufa_raw_initial %>% 
  filter(sediment_conc_measure_type == "range") %>% 
  separate(sediment_conc, into = c("low", "high"), sep = "-") %>% 
  mutate(low = parse_number(low),
         high = parse_number(high)) %>% 
  mutate(sediment_conc_ugg = as.character((low + high)/2)) %>% 
  select(-low, -high)

# 3) add range fix back to pufa
pufa_raw = pufa_raw_initial %>% 
  filter(!row_id %in% unique(pufa_with_range$row_id)) %>% 
  bind_rows(pufa_with_range)

# 4) wrangle

# load environmental attributes to join with pufa data
source('code/custom_functions/estimate_streamtemp.R') # this converts air temperature to stream temperatures using a published equation

emergence_production_with_vars = readRDS(file = 'data/emergence_production_with_vars.rds')# use the mean and sd of temp from this to standardize the variables and make them harmonize with the standardized global prediction temperatures later

pufa_attributes = read_csv("data/PUFA_Data_ALL_ATTRIBUTES_PROCESSED_V1.csv") %>% 
  distinct() %>% 
  rename(latitude = coordinates,
         longitude = coord) %>% 
  mutate(chemical = str_to_lower(chemical),
         organisms = str_replace_all(organisms, "\uFFFD", " ")) %>% 
  mutate(tmp_dc_syr10 = tmp_dc_syr/10, # put temps in dec C instead of 10*dec C
         pre_cm_syr1000 = pre_mm_syr/1000,
         precip_s = scale(pre_mm_syr),
         stream_temp = estimate_streamtemp(tmp_dc_syr10))

# This code removes characters from concentration columns (e.g., "35.2 (ng/l)" becomes "35.2"). 
# It then puts the units in a new column and harmonizes them to the same units. 

pufa_unit_harmonized = pufa_raw %>% 
  filter(adult_units != "mg dw_m-2_yr-1") %>% # some data were annual fluxes not tissue concentrations, remove here
  mutate(water_conc_num = parse_number(as.character(water_conc)),
         adult_conc_num = parse_number(as.character(adult_conc)),
         sediment_conc_num = parse_number(as.character(sediment_conc)),
         organisms = str_replace_all(organisms, "\uFFFD", " ")) %>% 
  mutate(adult_conc_ng_mg_dm = case_when(adult_units == "ug_g" ~ adult_conc_num*1,
                                         adult_units == "µg_g" ~ adult_conc_num*1,
                                         adult_units == "mg_kg" ~ adult_conc_num*1,
                                         adult_units == "mg_g" ~ adult_conc_num*1000,
                                         adult_units == "mg_g_dm" ~ adult_conc_num*1000,
                                         adult_units == "ppm_dw" ~ adult_conc_num*1,
                                         adult_units == "ng_g_dm" ~ adult_conc_num/1000,
                                         TRUE ~ adult_conc_num)) %>% 
  mutate(pub_name = case_when(grepl("Silina et al. Emergence ", pub_name) ~ "Silina et al. 2023. Emergence of Amphibious Insects from an Old Beaver Pond",
                              TRUE ~ pub_name)) %>% 
  select(-latitude, -longitude) %>% 
  rename(latitude = coordinates,
         longitude = coord) %>% 
  left_join(pufa_attributes, by = c("pub_number", "pub_name", "organisms", "chemical", "latitude", "longitude")) %>% 
  filter(adult_units == "mg_g_dm") %>% # original pufa data units are in mg_g_dm. they were converted to ng_mg_dm to be consistent with other contaminants. But the adult_units here refers to the correct "old" units.
  mutate(max_y = max(adult_conc_ng_mg_dm, na.rm = T),
         y_s = adult_conc_ng_mg_dm/max_y) %>% 
  mutate(taxon = str_replace(organisms, "\xa0", "_"), # remove this character string
         taxon = str_replace(taxon, "\\+", "_"), # remove pluses
         taxon = str_replace(taxon, "\\+", "_"),
         taxon = str_replace_all(taxon, "\\s+", "")) # remove all spaces 

saveRDS(pufa_unit_harmonized, file = "data/pufa_data.rds")


