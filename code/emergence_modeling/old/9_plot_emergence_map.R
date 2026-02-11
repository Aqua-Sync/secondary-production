library(tidyverse)
library(tidybayes)
library(ggmap)
library(ggExtra)
library(viridis)

dm2 = readRDS("posteriors/hybas_predictions_kgdm_peryear.rds") %>% 
  left_join(readRDS("data/hybas_regions.rds")) %>% 
  left_join(readRDS("data/HYBAS_surface_area_REDIST.rds") %>% 
              mutate(HYBAS_ID = as.numeric(HYBAS_ID))) %>% 
  left_join(readRDS("data/hydrobasin_vars_rssa_short.rds") %>% 
              select(HYBAS_ID, BA_km2)) 

set.seed(03354)
map = dm2 %>% 
  # filter(median > 0) %>%
  filter(region_name != "Greenland") %>% 
  sample_n(300000)  %>%
  # filter(median > median(median, na.rm = T)) %>% 
  ggplot(aes(x = lon, y = lat, color = log10(median + 0.01))) + 
  geom_point(shape = ".") + 
  coord_quickmap() +
  guides(alpha = "none") +
  labs(caption = "scale_color_distiller(type = div, palette = Spectral)") +
  # scale_color_viridis() +
  scale_color_distiller(type = "div", palette = "Spectral") +
  # scale_color_viridis(trans = "log10", color = ) +
  # theme(legend.position = "top") +
  guides(color = "none") +
  theme_void() +
  NULL

ggsave(map, file = "plots/centroid_map.jpg", width = 6.5, height = 3)

map2 = dm2 %>% 
  # filter(median > 0) %>%
  filter(region_name != "Greenland") %>% 
  # sample_n(3000)  %>%
  # filter(median > median(median, na.rm = T)) %>% 
  ggplot(aes(x = lon, y = lat, color = log10(median + 0.01))) + 
  geom_point(shape = ".") + 
  coord_quickmap() +
  guides(alpha = "none") +
  labs(caption = "scale_color_distiller(type = div)") +
  # scale_color_viridis() +
  scale_color_distiller(type = "div") +
  # scale_color_viridis(trans = "log10", color = ) +
  # theme(legend.position = "top")  +
  guides(color = "none") +
  theme_void() +
  NULL

ggsave(map2, file = "plots/centroid_map2.jpg", width = 6.5, height = 3)


map3 = dm2 %>% 
  # filter(median > 0) %>%
  filter(region_name != "Greenland") %>% 
  # sample_n(3000)  %>%
  # filter(median > median(median, na.rm = T)) %>% 
  ggplot(aes(x = lon, y = lat, color = log10(median + 0.01))) + 
  geom_point(shape = ".") + 
  coord_quickmap() +
  guides(alpha = "none") +
  labs(caption = "scale_color_distiller(type = div, palette = RdGy)") +
  # scale_color_viridis() +
  scale_color_distiller(type = "div", palette = "RdGy") +
  # scale_color_viridis(trans = "log10", color = ) +
  # theme(legend.position = "top")  +
  guides(color = "none") +
  theme_void() +
  NULL

ggsave(map3, file = "plots/centroid_map3.jpg", width = 6.5, height = 3)

map4 = dm2 %>% 
  # filter(median > 0) %>%
  filter(region_name != "Greenland") %>% 
  # sample_n(3000)  %>%
  # filter(median > median(median, na.rm = T)) %>% 
  ggplot(aes(x = lon, y = lat, color = log10(median + 0.01))) + 
  geom_point(shape = ".") + 
  coord_quickmap() +
  guides(alpha = "none") +
  labs(caption = "scale_color_viridis()") +
  scale_color_viridis() +
  # scale_color_distiller(type = "div", palette = "RdGy") +
  # scale_color_viridis(trans = "log10", color = ) +
  # theme(legend.position = "top")  +
  guides(color = "none") +
  theme_void() +
  NULL

ggsave(map4, file = "plots/centroid_map4.jpg", width = 6.5, height = 3)

library(cowplot)
maps_of_a_different_color = plot_grid(map, map2, map3, map4, ncol = 2)

ggsave(maps_of_a_different_color, file = "plots/maps_of_a_different_color.jpg", width = 6.5, height = 6.5)


dm2 %>% 
  # filter(median > 0) %>%
  filter(region_name != "Greenland") %>% 
  filter(lat > -20 & lat < 8) %>% 
  filter(lon < -30) %>% 
  sample_n(10000) %>%
  # filter(median > median(median, na.rm = T)) %>% 
  ggplot(aes(x = lon, y = lat, color = log10(median + 0.01))) + 
  geom_point(shape = ".") + 
  coord_quickmap() +
  guides(alpha = "none") +
  scale_color_viridis() +
  # scale_color_distiller(type = "div", palette = "Spectral") +
  # scale_color_viridis(trans = "log10", color = ) +
  # theme(legend.position = "top") +
  NULL

map_perm2 = dm_perm2 %>% 
  ungroup %>% 
  # filter(median > 0) %>%
  sample_n(100000)  %>% 
  ggplot(aes(x = lon, y = lat, color = median + 1, alpha = log10(median + 1))) + 
  geom_point(shape = ".", size = 0.5) + 
  # coord_quickmap() +
  guides(alpha = "none") +
  scale_color_distiller(palette = "Greens", direction = 1,
                        trans = "log10") +
  # scale_color_viridis(trans = "log10", color = ) +
  # theme(legend.position = "top") +
  NULL

ggMarginal(map, margins = "y", type = "histogram",
           yparams = list(  bins=50),
           size = 10)



# water density -----------------------------------------------------------
dm %>% 
  ungroup %>% 
  # filter(median > 0) %>% 
  filter(lat < 0) %>% 
  filter(lon < -30) %>% 
  # sample_n(10000) %>%
  ggplot(aes(x = lon, y = lat, color = ((area.redist) + 1))) + 
  geom_point(shape = ".") + 
  coord_quickmap() +
  guides(alpha = "none") +
  scale_color_distiller(palette = "Greens", direction = 1,
                        trans = "log10") +
  # theme(legend.position = "top") +
  NULL

# metals ------------------------------------------------------------------

metals = readRDS("posteriors/hybas_predictions_metals.rds") %>% 
  left_join(readRDS("data/hybas_regions.rds") %>%
              mutate(HYBAS_ID = as.character(HYBAS_ID)))

#get layer for the world map
world <- map_data("world")


metals_map = metals %>% 
  group_by(cas) %>% 
  mutate(chem_flux_mg_year_s = chem_flux_mg_year/max(chem_flux_mg_year)) %>% 
  # filter(median > 0) %>%
  sample_n(1000)  %>%
  ggplot() + 
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "grey60") +  #fill is the land color
  coord_quickmap() +
  geom_point(shape = 20, size = 0.1,
             aes(x = lon, y = lat, color = chem_flux_mg_year + 0.01)) + 
  scale_color_viridis(trans = "log10") +
  theme(legend.position = "top") +
  facet_wrap(~element) +
  guides(color = "none") +
  theme_void()

ggsave(metals_map, file = "plots/metals_map.jpg", width = 6.5, height = 2)


water = readRDS("data/HYBAS_surface_area_REDIST.rds") %>% 
  left_join(readRDS("data/hybas_regions.rds"))


water = modeled_water %>% filter(cas == "7782-49-2") %>% 
  # mutate(HYBAS_ID = as.numeric(HYBAS_L12)) %>% 
  left_join(readRDS("data/hybas_regions.rds"))

water %>% 
  # filter(median > 0) %>%
  # filter(area.redist >= 1e-02) %>% 
  # filter(lon >= 0 & lon <= 30) %>% 
  # filter(lat >= 20 & lat <= 30) %>% 
  # filter(area.redist > 0) %>%
  filter(cas == "7440-66-6") %>% 
  sample_n(1000) %>%
  ggplot(aes(x = lon, y = lat, color = area.redist)) + 
  geom_point(shape = ".", size = 0.1) + 
  coord_quickmap() +
  # scale_color_brewer(type = "seq") +
  scale_color_viridis(trans = "log10", option = "B") +
  theme(legend.position = "top") 


# cides -------------------------------------------------------------------


cides = readRDS("posteriors/hybas_predictions_pest_herb_fungicide.rds") %>% 
  left_join(readRDS("data/hybas_regions.rds") %>%
              mutate(HYBAS_ID = as.character(HYBAS_ID)))

cides_map = cides %>% 
  # filter(median > 0) %>%
  sample_n(100000)  %>% 
  ggplot(aes(x = lon, y = lat, color = chem_flux_mg_year)) + 
  geom_point(shape = 20, size = 0.1) + 
  coord_quickmap() +
  scale_color_viridis(trans = "log10") +
  theme(legend.position = "top") +
  facet_wrap(~element) +
  theme_void()

ggsave(cides_map, file = "plots/cides_map.jpg", width = 6.5, height = 2)


cides_metals_map = cides %>% 
  bind_rows(metals) %>% 
  # filter(median > 0) %>%
  sample_n(100000)  %>% 
  ggplot(aes(x = lon, y = lat, color = chem_flux_mg_year)) + 
  geom_point(shape = 20, size = 0.1) + 
  coord_quickmap() +
  scale_color_viridis(trans = "log10") +
  theme(legend.position = "top") +
  facet_wrap(~element) +
  theme_void()

ggsave(cides_metals_map, file = "plots/cides_metals_map.jpg", width = 6.5, height = 2)
