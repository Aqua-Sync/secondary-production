library(tidyverse)
library(readxl)
library(janitor)

# 1) load data that were downloaded from here: https://niva365.sharepoint.com/sites/int_AquaSYNC-SWG002-StefanoLarsenchair/Shared%20Documents/Forms/AllItems.aspx?csf=1&web=1&e=w70dX5&cid=d57aa1b9%2D2bab%2D47b4%2D851b%2D4976cade772d&FolderCTID=0x012000C7B36F0724E5ED4AAABF6D2A333A6AF4&id=%2Fsites%2Fint%5FAquaSYNC%2DSWG002%2DStefanoLarsenchair%2FShared%20Documents%2FSWG002%20%2D%20Stefano%20Larsen%20%28chair%29%2FData%2FACSP
secondary_prod_raw = read_excel("data/ACSP_Data.xlsx") %>% 
  clean_names() %>% 
  mutate(id = 1:nrow(.))

# 2) harmonize units
secondary_prod_wrangled = secondary_prod_raw %>%
  mutate(mass_type = case_when(grepl("AFDM", units) ~ "AFDM",
                                     grepl("DW", units) ~ "DM",
                                     grepl("dry mass", units) ~ "DM",
                                     grepl("DM", units) ~ "DM",
                                     grepl("wet", units) ~ "WM")) %>% 
  mutate(mass_units = str_sub(units, 1, 1),
         mass_units = case_when(mass_units == "g" ~ "g", 
                                mass_units == "m" ~ "mg")) %>% 
  pivot_longer(cols = c(acsp, aisp, eph_sp, ple_sp, tri_sp, chi_sp, other_sp), values_to = "raw_value") %>% 
  mutate(value = case_when(mass_units == "g" ~ raw_value*1000, TRUE ~ raw_value),
         perc_ash = 10, # 10 percent from Waters et al. 1977 page 115 Table 1
         perc_ash_correction = 100/(100-perc_ash),
         dm_mg_m2_y = case_when(mass_type == "AFDM" ~ value*perc_ash_correction,
                        mass_type == "DM" ~ value,
                        mass_type == "WM" ~ value*0.2),   # wet to dry correction
         # afdm_mg_m2_y = case_when(mass_type == "AFDM" ~ value,
         #                  mass_type == "DM" ~ value/perc_ash_correction)
         ) %>% 
  mutate(mass_type = "DM", 
         mass_units = "mg_m2_y")

# 3) clean and pivot back to original form
secondary_prod = secondary_prod_wrangled %>% 
  select(-raw_value, -value, - units, -perc_ash, - perc_ash_correction) %>% 
  pivot_wider(names_from = name, values_from = dm_mg_m2_y)

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
  distinct(index) %>% 
  tally()
