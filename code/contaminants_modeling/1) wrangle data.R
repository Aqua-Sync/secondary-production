library(tidyverse)
library(brms)
library(janitor)
library(tidybayes)
library(viridis)
library(scales)
library(readxl)

# 1) load data
contaminants_raw = read_excel("data/AquaSync-Contaminant_transfer-2024-6-28.xlsx", 
                          sheet = "main_data_good_names") %>% 
  clean_names() %>% 
  ungroup %>% 
  mutate(row_id = row_number())

# 2) wrangle
# This code removes characters from concentration columns (e.g., "35.2 (ng/l)" becomes "35.2"). 
# It also harmonizes the units 

contaminants_fix_non_numeric_rows = contaminants_raw %>%
  pivot_longer(cols = c(emergence_mgdm_m2_y,
                        water_conc_ugl,
                        adult_conc_ng_mg_dm,
                        sediment_conc_ugg)) %>% 
  mutate(non_numeric_conc = as.numeric(value)) %>%
  # filter(is.na(non_numeric_conc)) %>% 
  # filter(!is.na(value)) %>% 
  separate(value, into = c("value", "characters"), sep = "\\s") %>% 
  mutate(characters = str_remove(characters, "\\)"),
         characters = str_remove(characters, "\\("),
         characters = str_to_lower(characters)) %>% 
  mutate(greater_lesser = case_when(grepl(">", value) ~ ">",
                                    grepl("<", value) ~ "<")) %>% # There are only < signs as of 6/30/2024
  mutate(value = parse_number(value),
         value = case_when(greater_lesser == "<" ~ value/0.5,
                           TRUE ~ value)) %>% 
  mutate(value = case_when(characters == "mg/l" ~ value*1000,
                           characters == "ug/g" ~ value*1,
                           characters == "ng/l" ~ value/1000,
                           characters == "µg/g" ~ value*1,
                           characters == "mg/kg" ~ value*1,
                           characters == "mg/g" ~ value*1000,
                           characters == "ppm/dw" ~ value*1,
                           TRUE ~ value)) %>% 
  mutate(name = paste0(name, "_corrected")) %>%
  pivot_wider(names_from = name, 
              values_from = value,
              id_cols = row_id)

# 3) Append the fixed units using left_join 
contaminants_updated = contaminants_raw %>% 
  mutate(water_conc_ugl = parse_number(water_conc_ugl),
         adult_conc_ng_mg_dm = parse_number(adult_conc_ng_mg_dm),
         emergence_mgdm_m2_y = parse_number(emergence_mgdm_m2_y),
         sediment_conc_ugg = parse_number(sediment_conc_ugg)) %>% 
  left_join(contaminants_fix_non_numeric_rows) %>% 
  mutate(adult_conc_ng_mg_dm_original = adult_conc_ng_mg_dm,
         water_conc_ugl_original = water_conc_ugl,
         sediment_conc_ugg_original = sediment_conc_ugg,
         emergence_mgdm_m2_y_original = emergence_mgdm_m2_y) %>% 
  mutate(water_conc_ugl = case_when(is.na(water_conc_ugl_corrected) ~ water_conc_ugl,
                                    TRUE ~ water_conc_ugl_corrected)) %>% 
  mutate(adult_conc_ng_mg_dm = case_when(is.na(adult_conc_ng_mg_dm_corrected) ~ adult_conc_ng_mg_dm,
                                    TRUE ~ adult_conc_ng_mg_dm_corrected)) %>% 
  mutate(emergence_mgdm_m2_y = case_when(is.na(emergence_mgdm_m2_y_corrected) ~ emergence_mgdm_m2_y,
                                    TRUE ~ emergence_mgdm_m2_y_corrected)) %>% 
  mutate(sediment_conc_ugg = case_when(is.na(sediment_conc_ugg_corrected) ~ sediment_conc_ugg,
                                         TRUE ~ sediment_conc_ugg_corrected))

# 4) create a broad chemical classifier and save
contaminants = contaminants_updated %>% 
  mutate(chemical_original = chemical) %>% 
  mutate(chemical = case_when(grepl("Hg", chemical) ~ "Hg",
                              TRUE ~ chemical)) %>% 
  ungroup %>% 
  mutate(chemical_category_broad = case_when(chemical_category == "Cu" ~ "metal",
                                             chemical_category == "Pb" ~ "metal",
                                             chemical_category == "other metals" ~ "metal",
                                             chemical_category == "thiacloprid" ~ "insecticide",
                                             grepl("Hg", chemical_category) ~ "Hg",
                                             chemical_category %in% c("HOP", "PCB", "PBDE", "PAH") ~ "organics",
                                             TRUE ~ chemical_category)) %>% 
  select(!contains("_corrected")) # removes the temporary columns

saveRDS(contaminants, file = "data/contaminants.rds")

contaminants %>% 
  distinct(chemical_category_broad) %>% 
  print(n = Inf)
