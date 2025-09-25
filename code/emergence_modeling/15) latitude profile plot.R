# latitude profile
library(tidyverse)
library(tidybayes)
library(ggridges)
library(tidybayes)
library(brms)
theme_set(theme_default())

d = readRDS("posteriors/post_flux_kgdm_perm2_perhybas.rds") %>% 
  group_by(region_name) %>% 
  mutate(median_region = median(median, na.rm = T)) %>% 
  ungroup
hybas_covariates = readRDS("data/hybas_covariates.rds")


latitude_profile <- d %>%
  left_join(hybas_covariates) %>% 
  group_by(terr_biom) %>% 
  mutate(bin_index = cut_interval(lat, 1000, labels = FALSE)) %>% # create 1000 bins per biome
  group_by(bin_index, terr_biom) %>%
  mutate(lat = min(lat, na.rm = T)) %>% 
  reframe(median = median(median, na.rm = TRUE), lat = first(lat)) # calculate median flux per biome and bin

# plot
latitude_profile_plot = latitude_profile %>% 
  ggplot(aes(y = lat, x = median, color = terr_biom)) + 
  geom_point(shape = ".")  +
  geom_segment(aes(y = lat, yend = lat, x = 0, xend = median))

ggsave(latitude_profile_plot, file = "plots/latitude_profile_plot.jpg", width = 7, height = 8)
