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
          q97.5 = quantile(.prediction, 0.975),
          mean_emerge = mean(.prediction),
          sd_emerge = sd(.prediction)) 

# load water areas
# witharea = read_excel("data/witharea.xlsx")

# witharea_flux = witharea %>% select(-`...1`) %>% 
#   rownames_to_column() %>% 
#   merge(emergence_quantiles) %>% 
#   as_tibble() %>% 
#   pivot_longer(cols = starts_with("q")) %>% 
#   mutate(kgdmy = area_m*value) %>% 
#   select(-value) %>% 
#   pivot_wider(names_from = "name", values_from = "kgdmy")
# 
# write_csv(witharea_flux, file = "data/witharea_flux.csv")

# load Allen water areas
allen_area = read_csv("data/HydroBasin_RSSA_02192024.csv") %>% 
  mutate(mnRSSA_prop = mnRSSA_pc/100,
         sdRSSA_prop = sdRSSA_pc/100) %>% 
  mutate(water_km2 = BA_km2*mnRSSA_prop,
         water_km2_sd = BA_km2*sdRSSA_prop)

allen_area_emergence_calc = cbind(allen_area, emergence_quantiles) %>% as_tibble() %>% 
  glimpse() %>% 
  mutate(mean_emerge_perbasin_kgdmy = water_km2*mean_emerge,
         var_emerge = sd_emerge^2,
         var_water = water_km2_sd^2,
         var_water_emerge = var_emerge*var_water,
         sd_emerge_perbasin_kgdmy = sqrt(var_water_emerge))

allen_area_emergence = allen_area_emergence_calc %>% 
  select(BAS_ID, BAS_Class, BA_km2, mnRSSA_pc, sdRSSA_pc, 
         mnRSSA_prop, sdRSSA_prop,
         mean_emerge_perbasin_kgdmy,
         sd_emerge_perbasin_kgdmy)

write_csv(allen_area_emergence, file = "data/allen_area_emergence.csv")

library(scales)

correct_for_zeros = allen_area_emergence %>% 
  filter(mean_emerge_perbasin_kgdmy != 0) %>% 
  filter(mean_emerge_perbasin_kgdmy == min(mean_emerge_perbasin_kgdmy)) %>% 
  mutate(min_div_2 = mean_emerge_perbasin_kgdmy/2) %>% 
  pull(min_div_2)

emerge_distribution_plot = allen_area_emergence %>%
  mutate(mean = mean_emerge_perbasin_kgdmy + correct_for_zeros,
         upper = mean_emerge_perbasin_kgdmy + sd_emerge_perbasin_kgdmy,
         lower = mean_emerge_perbasin_kgdmy - sd_emerge_perbasin_kgdmy) %>% 
  ggplot(aes(y = reorder(BAS_ID, mean_emerge_perbasin_kgdmy),
             x = mean_emerge_perbasin_kgdmy)) +
  geom_linerange(aes(xmin = lower,
                      xmax = upper),
                  linewidth = 0.01,
                 color = "gray80") + 
  geom_point(color = "black",
             size = 0.01) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_x_log10() +
  labs(y = "BAS_ID (ranked by mean emergence)",
       x = "Annual Emergence (kgDM/y)",
       title = "Annual Emergence from 47,179 drainage basins",
       subtitle = "mean +/- sd")

emerge_distribution_plot

ggsave(emerge_distribution_plot, file = "plots/emerge_distribution_plot.pdf",
       width = 6.5, height = 6.5)



