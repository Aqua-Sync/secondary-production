library(tidyverse)
library(readxl)
library(janitor)

secondary_prod_raw = read_excel("data/ACSP_Data.xlsx") %>% 
  clean_names() %>% 
  mutate(id = 1:nrow(.))

# distinct units
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

secondary_prod = secondary_prod_wrangled %>% 
  select(-raw_value, -value, - units, -perc_ash, - perc_ash_correction) %>% 
  pivot_wider(names_from = name, values_from = dm_mg_m2_y)

# check
unique(secondary_prod$mass_type)
unique(secondary_prod$mass_units)
is.na(secondary_prod$mass_type)
is.na(secondary_prod$mass_units)

write_csv(secondary_prod, file = "data/secondary_prod.csv")

