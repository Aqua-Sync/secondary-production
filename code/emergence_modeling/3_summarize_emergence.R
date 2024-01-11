library(tidyverse)
library(brms)
library(janitor)
library(tidybayes)
library(viridis)
library(scales)

emergence_production = read_csv(file = "data/emergence_production.csv") %>% 
  separate(site_id, into = c("author", "author2"), extra = "merge") %>% 
  mutate(year = parse_number(author2)) %>% 
  mutate(author_year = paste(author, year, sep = "_"))

brm_emerge = brm(mean_emergence_kgdmm2y|mi(sd_emergence_kg) ~ 1 + (1|author_year),
                 data = emergence_production,
                 family = Gamma(link = "log"),
                 prior = c(prior(normal(-2, 2), class = "Intercept")))
# 
# saveRDS(brm_emerge, file = "models/brm_emerge.rds")

brm_emerge = readRDS(file = "models/brm_emerge.rds")

pp_check(brm_emerge)


# Show estimates
author_sims = emergence_production %>% 
  add_epred_draws(brm_emerge, re_formula = NULL)


author_sims %>% 
  mutate(has_data = case_when(is.na(mean_emergence_kgdmm2y) ~ "no", TRUE ~ "yes")) %>% 
  ggplot(aes(x = author_year, y = .epred, color = has_data)) + 
  stat_pointinterval() +
  scale_y_log10() +
  coord_flip()


# Estimate global fluxes --------------------------------------------------

river_area_global_m2 = 7.73e+11  # From Allen, G. & Pavelsky, T. (2018) Global extent of rivers and streams. Science, (361), 6398.

river_area_global_km2 = 773000
river_area_global_sd = 79000
river_area_global_m2_sims = tibble(river_global_m2 = rnorm(1000, river_area_global_km2, river_area_global_sd)*1e6)


tibble(sd_emergence_kg = 1) %>%  # placeholder
  add_epred_draws(brm_emerge, allow_new_levels = T, re_formula = NA) %>%
  ggplot(aes(x = .epred)) +
  stat_slabinterval() +
  scale_x_log10() +
  labs(x = "Emergence (kg/m2/y)")

#estimate with fixed river area
tibble(sd_emergence_kg = 1) %>%  # placeholder
  add_epred_draws(brm_emerge, allow_new_levels = T, re_formula = NA) %>% 
  ggplot(aes(x = .epred*river_area_global_m2)) + 
  # geom_histogram(bins = 100) +
  stat_slabinterval() +
  scale_x_log10(limits = c(1e07, 3e09)) +
  labs(x = "Global Emergence (kg/y)") 

# estimate with variable river area
tibble(sd_emergence_kg = 1) %>%  # placeholder
  add_epred_draws(brm_emerge, allow_new_levels = T, re_formula = NA) %>% 
  ggplot(aes(x = .epred*rnorm(nrow(.), river_area_global_km2, river_area_global_sd)*1e6)) + 
  # geom_histogram(bins = 100) +
  stat_slabinterval() +
  scale_x_log10(limits = c(1e07, 3e09)) +
  labs(x = "Global Emergence (kg/y)") 

# percent affected by mercury
mercury_affected_m2 = river_area_global_m2*0.1

# concentration of mercury in contaminanted bugs
bug_hg_mgkg = 85

tibble(sd_emergence_kg = 1) %>%  # placeholder
  add_epred_draws(brm_emerge, allow_new_levels = T, re_formula = NA) %>% 
  mutate(global_m2 = rnorm(nrow(.), river_area_global_km2, river_area_global_sd)*1e6,
         mercury_affected_m2 = global_m2*0.1) %>% 
  mutate(b_global_emergence_kgDM_year = .epred*global_m2,
         c_mercury_affected_emergence_kgDM_year = .epred*mercury_affected_m2,
         d_mercury_flux_kgHg_year = (c_mercury_affected_emergence_kgDM_year*rnorm(nrow(.), bug_hg_mgkg, 10)/1e6),
         a_mean_emergence_kgDM_m2_year = .epred) %>% 
  pivot_longer(cols = c(b_global_emergence_kgDM_year, 
                        c_mercury_affected_emergence_kgDM_year,
                        d_mercury_flux_kgHg_year,
                        a_mean_emergence_kgDM_m2_year)) %>% 
  group_by(name) %>% 
  # filter(name == "d_mercury_flux_kgHg_year") %>% 
  # mutate(median = median(value)) %>% 
  # mutate(median = case_when(name == "mean_emergence_kgDM_m2_year" ~ 50, TRUE ~ median)) %>% 
  ggplot(aes(x = name, y = value, fill = name)) + 
  stat_slabinterval(alpha = 0.9) +
  scale_y_log10() +
  scale_fill_viridis_d() +
  labs(y = "Emergence or contaminant flux")


