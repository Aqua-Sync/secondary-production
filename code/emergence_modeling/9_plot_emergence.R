library(tidyverse)
library(tidybayes)

# load data
data_to_predict_list = readRDS("data/data_to_predict.rds") %>% group_by(region) %>% group_split()

# get regions from here: https://developers.google.com/earth-engine/datasets/catalog/WWF_HydroATLAS_v1_Basins_level12#table-schema
regions = tibble(region_name = c("Africa",
                            "Europe",
                            "Siberia",
                            "Asia",
                            "Australia",
                            "South America",
                            "North America",
                            "Arctic (North America)",
                            "Greenland"),
                 region = as.character(c(1, 2, 3, 4, 5, 6, 7, 8, 9)))

hybas_regions = bind_rows(data_to_predict_list) %>% left_join(regions) %>% 
  select(HYBAS_ID, region_name)

saveRDS(hybas_regions, file = "data/hybas_regions.rds")

# load model predictions of each HYBAS_ID
flux_predictions = readRDS(file = "posteriors/flux_predictions_all.rds") %>% 
  mutate(region = as.numeric(str_sub(HYBAS_ID, 1, 1))) %>% left_join(regions)

write_csv(flux_predictions, file = "posteriors/flux_predictions.csv")

# load posterior of global and regional total flux
flux_global = readRDS(file = "posteriors/flux_global.rds") %>% mutate(region_name = "Earth", 
                                                                      global = "Earth")
flux_region = readRDS(file = "posteriors/flux_region.rds") %>% mutate(global = "Regional") %>% left_join(regions)

flux_region_global = bind_rows(flux_region, flux_global)

# plot --------------------------------------------------------------------

flux_hydrobasin_plot = flux_predictions %>% 
  sample_n(120000) %>%
  group_by(region_name) %>% 
  mutate(median_flux = median(median)) %>% 
  arrange(median) %>% 
  mutate(order = row_number()) %>% 
  ggplot(aes(x = order, y = median + 0.1, color = region_name)) +
  geom_linerange(aes(ymin = lower95 + 0.1, 
                     ymax = upper95 + 0.1),
                 alpha = 0.02, linewidth = 0.1) + 
  geom_point(size = 0.01) +
  facet_wrap(~region_name) +
  scale_y_log10() +
  guides(color = "none") +
  scale_color_viridis_d(option = "A", begin = 0, end = 0.7) +
  theme_bw() +
  labs(y = "Aquatic Insect Emergence\n(kg DM/yr per HydroBasin)",
       x = "Hydrobasin (ranked by emergence)",
       caption = "Figure X: Total emergence (kgDM/yr) in 120,000 hydrobasins, randomly sampled from the 1.03 Million HydroAtlas Basins from
       https://developers.google.com/earth-engine/datasets/catalog/WWF_HydroATLAS_v1_Basins_level12#table-schema.
       Each dot is a posterior median +/- the 95% Credible Interval (shading)") +
  NULL

ggsave(flux_hydrobasin_plot, file = "plots/flux_hydrobasin_plot.jpg")


flux_global_plot = flux_region_global %>% 
  filter(region_name != "Greenland") %>%
  group_by(region_name) %>% 
  mutate(median = median(kgyr_global, na.rm = T)) %>% 
  ggplot(aes(x = kgyr_global + 0.1, fill = log2(median),
             y = reorder(region_name, -median))) +
  stat_halfeye(alpha = 0.8) + 
  scale_x_log10() +
  scale_fill_viridis_b(option = "C") +
  labs(y = "",
       x = "Total aquatic insect emergence\n(kg DM/yr)",
       caption = "Figure X. Total annual aquatic insect emergence (kgDM/yr) for each region and for the earth.
       Densities show the posterior distribution, summarized by the median (dot), 66% Credible Intervals (thick line),
       and 95% CrI (thin line). The dashed line for global shows a back-of-the-envelope estimate that combines emergence
       estimates from Gratton and Vander Zanden (2009) with water area from Allen and Pavelsky (2015).") +
  guides(fill = "none") +
  geom_segment(aes(x = 1.54e+09, xend = 1.54e+09,
                   y = 1, yend = 1.8), linetype = "dashed") +
  theme_bw() +
  NULL

ggsave(flux_global_plot, file = "plots/flux_global_plot.jpg", width = 6, height = 8)

flux_global %>% 
  median_qi(kgyr_global)
