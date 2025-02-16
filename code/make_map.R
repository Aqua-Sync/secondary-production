library(tidyverse)
library(ggmap)
library(janitor)
library(viridis)
library(readxl)

dat_bugs = readRDS(file = 'data/emergence_production_with_vars.rds')
dat_contaminants = read_excel("data/AquaSync-Contaminant_transfer-2024-2-21_JMK.xlsx", 
                              sheet = "main_data_good_names")

#get layer for the world map
world <- map_data("world")


#make a map
( map_bugs <- ggplot() + 
    geom_polygon(data = world, 
                 aes(x = long, y = lat, group = group)) +  #fill is the land color
    coord_quickmap() +
    geom_point(data = dat_bugs, aes(x = lon, y = lat, size = emerge_1, 
                               color = tmp_dc_syr)) + 
    theme_void()  + 
    guides(fill = "none", color = "none", size = "none") +
    scale_color_viridis() +
    NULL)

map_bugs

ggsave(map_bugs, file = "plots/map_bugs.jpg", dpi = 600, width = 6.5, height = 5, units = "in")
