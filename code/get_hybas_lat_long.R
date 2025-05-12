library(sf)
library(tidyverse)

hybas_regions = readRDS("data/hybas_regions.rds")

# get lat long of the outlet point of each hybas_id. We could load the full area shapes for each hybas and get the centroid,
# but this might be easier and visually similar.
# hybas_pour = st_read("data/hydrobasins_shapefiles/hybas_pour_lev12_v1.shp") 
# 
# hybas_latlong = hybas_pour %>% 
#   mutate(lon = st_coordinates(geometry)[,1],
#          lat = st_coordinates(geometry)[,2]) %>% 
#   group_by(HYBAS_ID) %>% 
#   reframe(lat = mean(lat),
#           lon = mean(lon))
# 
# 
# hybas_regions = hybas_regions %>% 
#   left_join(hybas_latlong %>% select(HYBAS_ID, lat, lon))
# 
# saveRDS(hybas_regions, file = "data/hybas_regions.rds")


hybas_regions %>%
  sample_n(100000) %>%
  ggplot(aes(x = lon, y = lat)) +
  geom_point(shape = ".")
