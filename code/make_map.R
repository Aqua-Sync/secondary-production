library(tidyverse)
library(ggmap)
library(janitor)

emergence_production = readRDS(file = "data/emergence_production_bioclim.rds")

#get layer for the world map
world <- map_data("world")

#make a map
( map_flux <- ggplot() + 
    geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "grey60") +  #fill is the land color
    coord_quickmap() +
    geom_point(data = emergence_production, aes(x = lon, 
                                                y = lat,
                                                size = mean_emergence_mgdmm2y/1000),
               shape = 21, fill = "yellow") +
    scale_size_continuous(breaks = c(0.01, .1, 0.3, 1, 3, 10, 30)) +
    theme_void() +
    labs(size = "Emergence\ngDM/m2/y") +
    NULL 
  )

map_flux

#save the map
ggview::ggview(map_flux, width = 6.5, height = 5, units = "in")
ggsave(map_flux, file = "plots/map_flux.jpg", dpi = 600, width = 6.5, height = 5, units = "in")
