library(tidyverse)
library(brms)
library(janitor)
library(tidybayes)
library(viridis)
library(scales)

# 1) load raw emergence data
emergence_production = read_csv(file = "data/emergence_production.csv") %>% 
  separate(site_id, into = c("author", "author2"), extra = "merge") %>% 
  mutate(year = parse_number(author2)) %>% 
  mutate(author_year = paste(author, year, sep = "_"))

# 2) fit emergence model
brm_emerge = brm(mean_emergence_kgdmm2y|mi(sd_emergence_kg) ~ 1 + (1|author_year),
                 data = emergence_production,
                 family = Gamma(link = "log"),
                 prior = c(prior(normal(-2, 2), class = "Intercept"),
                           prior(exponential(10), class = "sd"),
                           prior(exponential(10), class = "shape")))
#
saveRDS(brm_emerge, file = "models/brm_emerge.rds")

# brm_emerge = update(readRDS(file = "models/brm_emerge.rds"),
#                             newdata = emergence_production)

brm_emerge = readRDS(file= "models/brm_emerge.rds")

brm_emerge_2 = brm(mean_emergence_kgdmm2y|mi(sd_emergence_kg) ~ 1 + (1|author_year),
                 data = emergence_production,
                 family = Gamma(link = "log"),
                 prior = c(prior(normal(-2, 2), class = "Intercept"),
                           prior(exponential(2), class = "sd"),
                           prior(exponential(2), class = "shape")),
                 chains = 1)

saveRDS(brm_emerge_2, file = "models/temporary/brm_emerge_2.rds")

pp_check(brm_emerge, type = "boxplot")


# Show estimates
author_sims = emergence_production %>% 
  distinct(author_year) %>% 
  mutate(sd_emergence_kg = 1)  %>% 
  add_epred_draws(brm_emerge, re_formula = NULL)

author_sims %>% 
  ggplot(aes(y = author_year, x = .epred)) + 
  stat_pointinterval() +
  scale_x_log10() +
  geom_point(data = emergence_production, aes(x = mean_emergence_kgdmm2y,
                                              alpha = sd_emergence_kg),
             color = "red") +
  NULL

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
total_kgdmy = tibble(sd_emergence_kg = 1) %>%  # placeholder
  add_epred_draws(brm_emerge, allow_new_levels = T, re_formula = NA) %>% 
  mutate(kgdmy = .epred*rnorm(nrow(.), river_area_global_km2, river_area_global_sd)*1e6,
         name = "Total") 

saveRDS(total_kgdmy, file = "data/total_kgdmy_posterior.rds")

total_kgdmy %>% 
  ggplot(aes(x = kgdmy)) + 
  # geom_histogram(bins = 100) +
  stat_slabinterval() +
  scale_x_log10(limits = c(1e07, 3e09)) +
  labs(x = "Global Emergence (kg/y)") 

# percent affected by mercury
mercury_affected_m2 = river_area_global_m2*0.1

# concentration of mercury in contaminated bugs
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


tibble(sd_emergence_kg = 1) %>%  # placeholder
  add_epred_draws(brm_emerge, allow_new_levels = T, re_formula = NA) %>% 
  mutate(global_m2 = rnorm(nrow(.), river_area_global_km2, river_area_global_sd)*1e6,
         mercury_affected_m2 = global_m2*0.1) %>% 
  mutate(b_global_emergence_kgDM_year = .epred*global_m2) %>% 
  



# summarize
# Mean kgDMy flux

total_kgdmy %>% 
  mutate(MTdmy = kgdmy/1000) %>% 
  median_qi(MTdmy)

# Nyffeler estimates that birds consume 400-500 million metric tons of prey annually (in wet mass)
# That would be ~0.2*x in dry mass, so 80 to 100 million metric tons

# Thus, if 100% of emergence was eaten by birds, it would satisfy..

demand = total_kgdmy %>% 
  mutate(MTdmy = kgdmy/1000,
         bird_demand_MTdmy_low = 8e7,
         bird_demand_MTdmy_high = 1e8,
         prop_demand_low = MTdmy/bird_demand_MTdmy_low,
         prop_demand_high = MTdmy/bird_demand_MTdmy_high) %>% 
  pivot_longer(cols = starts_with("prop_"), names_to = "demand_range",
               values_to = "demand_value")


demand %>% 
  group_by(demand_range) %>% 
  median_qi(demand_value)

# ... between 0.4 and 0.5% of global annual bird demand. 

# Earth's land area is ~ 149 million km2 (from Wikipedia)
# Excluding Antarctica leaves 149 million - 14.2 million = 134.8 million km2
# Earth's River area is ~ 773000 km2 (from the Allen paper)
# The proportion of terrestrial earth that is rivers is...
773000/135000000
#...0.5%!
