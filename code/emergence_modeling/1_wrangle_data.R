library(tidyverse)
library(readxl)
library(janitor)

# 1) load data that were downloaded from here: https://niva365.sharepoint.com/:x:/r/sites/int_AquaSYNC-SWG002-StefanoLarsenchair/_layouts/15/Doc.aspx?sourcedoc=%7BA504FDE8-388D-4601-8E35-C3D504F74D4D%7D&file=ACSP_Data_ALL_ATTRIBUTES_PROCESSED_V2.1.csv&action=default&mobileredirect=true

# these data are with local-scale predictors
# secondary_prod_raw = read_xlsx("data/ACSP_Data_ALL_ATTRIBUTES_PROCESSED_V2.1.xlsx") %>% 
#   clean_names() %>% 
#   mutate(id = 1:nrow(.)) %>% 
#   select(id, everything()) 

# these data are with basin-wide predictors
secondary_prod_raw = read_excel("data/ACSP_Data_BASIN ATTRIBUTES.xlsx") %>%
  clean_names() %>%
  mutate(id = 1:nrow(.)) %>%
  select(id, everything()) %>% 
  filter(site_id != "Bottger_1975_Kalengo_Stream") %>% 
  filter(site_id != "Jackson_Fisher_1986") # Filtered after consulting via email with Muehlbauer Oct 2025. These were duplicates

# 2) harmonize units
secondary_prod_wrangled = secondary_prod_raw %>%
  mutate(mass_type = case_when(grepl("AFDM", units) ~ "AFDM", # jsw confirmed that these are accurate on 2025-01-17
                               grepl("DW", units) ~ "DM",
                               grepl("dry mass", units) ~ "DM",
                               grepl("DM", units) ~ "DM",
                               grepl("wet", units) ~ "WM",
                               grepl("mg C", units) ~ "C")) %>% 
  mutate(mass_units = str_sub(units, 1, 1),
         mass_units = case_when(mass_units == "g" ~ "g",  # jsw confirmed that these are accurate on 2025-01-17
                                mass_units == "m" ~ "mg")) %>% 
  pivot_longer(cols = c(acsp, aisp, eph_sp, ple_sp, tri_sp, chi_sp, other_sp, emerg), values_to = "raw_value") %>% 
  mutate(value = case_when(mass_units == "g" ~ raw_value*1000, TRUE ~ raw_value),
         perc_ash = 10, # 10 percent from Waters et al. 1977 page 115 Table 1
         perc_ash_correction = 100/(100-perc_ash),
         dm_mg_m2_y = case_when(mass_type == "AFDM" ~ value*perc_ash_correction,
                        mass_type == "DM" ~ value,
                        mass_type == "WM" ~ value*0.2, # wet to dry correction
                        mass_type == "C" ~ (value*2)*perc_ash_correction),   # C to mg correction (from Wesner et al. 2020)
         # afdm_mg_m2_y = case_when(mass_type == "AFDM" ~ value,
         #                  mass_type == "DM" ~ value/perc_ash_correction)
         ) %>% 
  mutate(mass_type = "DM", 
         mass_units = "mg_m2_y")

# 3) clean and pivot back to original form
secondary_prod = secondary_prod_wrangled %>% 
  select(-raw_value, -value, - units, -perc_ash, - perc_ash_correction) %>% 
  pivot_wider(names_from = name, values_from = dm_mg_m2_y) %>% 
  select(id, mass_type, mass_units, acsp, aisp, eph_sp, ple_sp, tri_sp, chi_sp, other_sp, 
         emerg, everything())

# 4) check and save
unique(secondary_prod$mass_type)
unique(secondary_prod$mass_units)
is.na(secondary_prod$mass_type)
is.na(secondary_prod$mass_units)


write_csv(secondary_prod, file = "data/secondary_prod.csv")

# summarize
secondary_prod %>% glimpse() %>% 
  pivot_longer(cols = c(acsp, aisp)) %>% 
  # group_by(name) %>% 
  filter(!is.na(value)) %>% 
  distinct(id) %>% 
  tally()
