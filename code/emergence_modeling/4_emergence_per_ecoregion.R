library(tidyverse)
library(brms)
library(janitor)
library(tidybayes)
library(viridis)
library(scales)

# load model
brm_emerge = readRDS(file = "models/brm_emerge.rds")

# get emergence predictions from model

emergence_predictions = tibble(author_year = "new",
       sd_emergence_kg = 1) %>% 
  add_predicted_draws(brm_emerge, re_formula = NULL, allow_new_levels = T) %>% 
  mutate(units = "kgdmm2y")

# summarize emergence predictions
emergence_quantiles = emergence_predictions %>% 
  group_by(units) %>% 
  reframe(q2.5 = quantile(.prediction, 0.025),
          q25 = quantile(.prediction, 0.25),
          q50 = quantile(.prediction, 0.5),
          q75 = quantile(.prediction, 0.75),
          q97.5 = quantile(.prediction, 0.975)) 

# load water areas
witharea = read_excel("data/witharea.xlsx")

witharea_flux = witharea %>% select(-`...1`) %>% 
  rownames_to_column() %>% 
  merge(emergence_quantiles) %>% 
  as_tibble() %>% 
  pivot_longer(cols = starts_with("q")) %>% 
  mutate(kgdmy = area_m*value) %>% 
  select(-value) %>% 
  pivot_wider(names_from = "name", values_from = "kgdmy")

write_csv(witharea_flux, file = "data/witharea_flux.csv")
