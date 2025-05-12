library(tidyverse)
library(tidybayes)
library(ggmap)
library(ggExtra)

post_flux_all_peryear_hybas = readRDS(file = "posteriors/post_flux_all_peryear_hybas.rds") %>% 
  left_join(readRDS("data/hybas_regions.rds")) %>% 
  left_join(readRDS("data/HYBAS_surface_area_REDIST.rds") %>% mutate(HYBAS_ID = as.numeric(HYBAS_ID))) %>% 
  left_join(readRDS("data/hydrobasin_vars_rssa_short.rds") %>% select(HYBAS_ID, BA_km2))

dm = post_flux_all_peryear_hybas %>%
  filter(grepl("kgdm", units))

map = dm %>% 
  # filter(median > 0) %>%
  sample_n(100000)  %>% 
  ggplot(aes(x = lon, y = lat, color = median)) + 
  geom_point(shape = 20, size = 0.1) + 
  coord_quickmap() +
  scale_color_viridis(trans = "log10") +
  theme(legend.position = "top")

ggMarginal(map, margins = "y", type = "histogram",
           yparams = list(  bins=50),
           size = 10)

