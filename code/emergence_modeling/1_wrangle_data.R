library(tidyverse)
library(readxl)
library(janitor)
# wrangle data for fitting models in subsequent scripts
# ~5 seconds

# 1) load data that were downloaded from here: https://niva365.sharepoint.com/:x:/r/sites/int_AquaSYNC-SWG002-StefanoLarsenchair/_layouts/15/Doc.aspx?sourcedoc=%7BA504FDE8-388D-4601-8E35-C3D504F74D4D%7D&file=ACSP_Data_ALL_ATTRIBUTES_PROCESSED_V2.1.csv&action=default&mobileredirect=true

# these data are with local-scale predictors
# secondary_prod_raw = read_xlsx("data/ACSP_Data_ALL_ATTRIBUTES_PROCESSED_V2.1.xlsx") %>% 
#   clean_names() %>% 
#   mutate(id = 1:nrow(.)) %>% 
#   select(id, everything()) 

# these data are with basin-wide predictors
secondary_prod_raw = read_csv("data/ACSP_Data_ALL_ATTRIBUTES_PROCESSED_V3.csv") %>%
  left_join(read_csv('data/ACSP_author_year.csv') %>% distinct()) %>% # adds info on author and year b/c this was removed during the attributes addition from the raw file on the shared drive
  clean_names() %>%
  mutate(id = 1:nrow(.)) %>%
  select(id, everything()) 


# extract just the attributes from HYBAS per observation. For joining later to taxa-specific data, etc.
attributes_by_id = secondary_prod_raw %>% select(-site_id, -index, -lon, -lat, -acsp,-aisp,
                                                 -eph_sp, -ple_sp, -tri_sp, -chi_sp, -other_sp, -emerg, -units, 
                                                 -notes, -stream_width_m, -basin_size_km2, -figure_table,
                                                 -author, -year)

saveRDS(attributes_by_id, file = "data/attributes_by_id.rds")

# 2) harmonize units
secondary_prod_wrangled = secondary_prod_raw %>%
  mutate(mass_type = case_when(grepl("AFDM", units) ~ "AFDM", # jsw confirmed that these are accurate on 2025-01-17
                               grepl("DW", units) ~ "DM",
                               grepl("dry mass", units) ~ "DM",
                               grepl("DM", units) ~ "DM",
                               grepl("wet", units, ignore.case = TRUE) ~ "WM",
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
         emerg, everything()) %>% 
  filter(!is.na(acsp) | !is.na(aisp) |!is.na(emerg)) # keep only if here are values for acsp, aisp, or emergence

# 4) check and save
unique(secondary_prod$mass_type)
unique(secondary_prod$mass_units)
is.na(secondary_prod$mass_type)
is.na(secondary_prod$mass_units)


write_csv(secondary_prod, file = "data/secondary_prod.csv")


# 3) clean and pivot back to original form for taxa (different filter than above)
secondary_prod_taxa = secondary_prod_wrangled %>% 
  select(-raw_value, -value, - units, -perc_ash, - perc_ash_correction) %>% 
  pivot_wider(names_from = name, values_from = dm_mg_m2_y) %>% 
  select(id, mass_type, mass_units, acsp, aisp, eph_sp, ple_sp, tri_sp, chi_sp, other_sp, 
         emerg, everything()) 

write_csv(secondary_prod_taxa, file = "data/secondary_prod_taxa.csv")

# summarize
secondary_prod %>% glimpse() %>% 
  pivot_longer(cols = c(acsp, aisp)) %>% 
  # group_by(name) %>% 
  filter(!is.na(value)) %>% 
  distinct(id) %>% 
  tally()

# plot taxa
secondary_prod %>% 
  pivot_longer(cols = ends_with("_sp")) %>% 
  filter(value > 0) %>% 
  ggplot(aes(x = acsp, y = value)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~name, scales = "free") +
  scale_x_log10() +
  scale_y_log10() +
  NULL

# how many samples had full taxonomic data?
secondary_prod_raw %>% 
  pivot_longer(cols = contains("_sp")) %>% 
  mutate(value = case_when(is.na(value) ~ 0, T ~ 1)) %>% 
  group_by(id) %>% 
  reframe(value = sum(value)) %>% 
  group_by(value) %>% 
  tally() %>% 
  mutate(total = 305,
         prop = n/total)

secondary_prod_raw %>% 
  pivot_longer(cols = contains("_sp")) %>% 
  filter(!is.na(value)) %>% 
  group_by(name) %>% 
  tally()


# set ggplot font ---------------------------------------------------------

library(ggplot2)

theme_set(theme(
      text = element_text(size = 14),
      panel.grid = element_blank()))
