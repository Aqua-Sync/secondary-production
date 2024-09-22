library(sf)

allen_sf = read_sf("data/allen_shapefiles/GRWL_summaryStats.shp") %>% # https://zenodo.org/records/1297434
  mutate(length_m = Shape_Leng*1000) %>% 
  mutate(area_m2 = width_med_*length_m) %>% 
  filter(lakeFlag == 0) %>% # keep only rivers
  filter(width_max_ > 0) # something odd here with 221 observations having a median of 0 and a max of -1. Delete.

# Shape_Leng is in kilometers: https://zenodo.org/records/1297434

allen_sf %>% 
  ggplot(aes(x = area_m2)) + 
  geom_histogram() +
  scale_x_log10() +
  NULL

# even in the dataset with only large rivers ~30% are less than 60 meters wide
allen_sf %>% 
  reframe(prop_less_than_60m = sum(width_med_ <= 60)/max(nrow(.)))

max(allen_sf$width_med_)

library(isdbayes)

xmax = max(allen_sf$area_m2)
xmin = 0.32

stream_area_sims = tibble(sim_areas = rparetocounts(n = 10000, lambda = -0.83, xmin = xmin, xmax = xmax)) %>% 
  arrange(-sim_areas) %>% 
  mutate(y = row_number())


stream_area_sims %>% 
  reframe(prop_less_than_60m = sum(sim_areas <= 60)/10000)


stream_area_sims %>% 
  ggplot(aes(x = sim_areas, y = y)) + 
  geom_point() +
  # scale_x_log10() + 
  # scale_y_log10() +
  NULL


library(VGAM)


vgam_stream_area_sims = tibble(sim_areas = rpareto(100000, shape = 0.9, scale = 0.32)) %>% 
  arrange(-sim_areas) %>% 
  mutate(y = row_number())

max(vgam_stream_area_sims$sim_areas)

vgam_stream_area_sims %>% 
  ggplot(aes(x = sim_areas, y = y)) + 
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  NULL
