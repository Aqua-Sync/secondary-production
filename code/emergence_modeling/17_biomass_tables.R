library(tidyverse)
library(brms)
library(tidybayes)
theme_set(theme_default())


# Table S1: global emergence per m2 and total -----------------------------

post_mass_nutrients = readRDS("posteriors/post_mass_nutrients.rds")
post_pufa = readRDS(file = "posteriors/post_pufa.rds")

post_pufa_flux =  post_pufa %>% 
  ungroup %>% 
  select(-HYBAS_ID) %>% 
  left_join(post_mass_nutrients %>% ungroup %>% select(.draw, mgDMm2y)) %>% 
  mutate(ngPUFAm2y = mean_ngPUFA_mgDM*(mgDMm2y)) %>% # multiply pufa concentration by mass concentration. 
  mutate(mgPUFAm2y = ngPUFAm2y/1e6) %>% 
  median_qi(mgPUFAm2y, na.rm = T) %>% 
  mutate(chemical = "\u2211mgPUFAm2y") %>% 
  rename(median = mgPUFAm2y)

# combine pufa, mass, nutrients 

post_mass_nutrients_pufa = post_mass_nutrients %>%
  ungroup %>% 
  select(.draw, contains("m2")) %>% 
  pivot_longer(cols = -.draw) %>% 
  group_by(name) %>% 
  median_qi(value, .width = 0.95) %>% 
  rename(chemical = name,
         median = value) %>% 
  bind_rows(post_pufa_flux) %>% 
  arrange(-median) %>%
  mutate(across(where(is.double), ~ janitor::signif_half_up(.x, 2))) %>% 
  mutate(nutrient = case_when(grepl("DM", chemical) ~ "Dry Mass",
                              grepl("mgC", chemical) ~ "Carbon",
                              grepl("mgN", chemical) ~ "Nitrogen",
                              grepl("mgPUFA", chemical) ~ "PUFA",
                              T ~ "Phosphorus"),
         units = "mg/m2/y",
         measure = "Export per surface water area") %>% 
  select(measure, nutrient, units, median, .lower, .upper)

# total emergence
post_mass_nutrients_pufa_global = readRDS(file = "posteriors/post_total_all.rds") %>% 
  group_by(chemical) %>% 
  median_qi(median = flux/1000) %>% 
  mutate(units = "Metric Tons") %>% 
  arrange(-median) %>%
  mutate(across(where(is.double), ~ round(.x, -3))) %>% 
  mutate(nutrient = case_when(grepl("dm", chemical) ~ "Dry Mass",
                              chemical == "C" ~ "Carbon",
                              chemical == "N" ~ "Nitrogen",
                              chemical == "PUFA" ~ "PUFA",
                              T ~ "Phosphorus"),
         units = "t/y",
         measure = "Global export") %>% 
  select(measure, nutrient, units, median, .lower, .upper)

write_csv(bind_rows(post_mass_nutrients_pufa,
                    post_mass_nutrients_pufa_global), file = "tables/table_S1.csv")

post_mass_nutrients_pufa_global = read_csv(file = "tables/post_emergence_global.csv")
post_emergence_perm2 = read_csv(file = "tables/post_emergence_perm2.csv")
post_flux_all_peryear_hybas = readRDS(file = "posteriors/hybas_predictions_mass_nutrients.rds") %>% 
  left_join(readRDS("data/hybas_regions.rds"))

