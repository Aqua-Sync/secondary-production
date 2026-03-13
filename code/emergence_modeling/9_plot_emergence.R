library(tidyverse)
library(tidybayes)
library(ggridges)
library(tidybayes)
library(brms)
theme_set(theme_default())

make_summary_table <- function(df, center = ".epred", lower = ".lower", upper = ".upper",
                               center_interval = "median_cri", digits = 1) {
  df %>%
    mutate(
      .center_val = round(.data[[center]], digits),
      .lower_val = round(.data[[lower]], digits),
      .upper_val = round(.data[[upper]], digits),
      center_interval = paste0(.center_val, " (", .lower_val, " to ", .upper_val, ")")) %>% 
    select(-.center_val, -.lower_val, -.upper_val)
}

# load data
data_to_predict_list = readRDS("data/data_to_predict.rds") %>% group_by(region) %>% group_split()
hybas_filter <- readRDS("data/hybas_filtered.rds")

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
flux_predictions = readRDS("posteriors/hybas_predictions_kgdm_peryear.rds")

# write_csv(flux_predictions, file = "posteriors/flux_predictions.csv")

# load posterior of global and regional total flux
flux_global = readRDS(file = "posteriors/post_total_all.rds") %>% mutate(region_name = "Earth",
                                                                      global = "Earth") %>% 
  filter(chemical == "dm")
flux_region = readRDS(file = "posteriors/post_total_region_summary.rds") %>%
  bind_rows() %>% 
  mutate(global = "Regional",
         region = as.character(region_name)) %>% 
  left_join(regions) %>% 
  mutate(chemical = "dm")

flux_region_global = bind_rows(flux_region, flux_global)
# flux_region_global = readRDS(file = "posteriors/flux_region_global.rds")
saveRDS(flux_region_global, file = "posteriors/flux_region_global.rds")

# plot flux per m2 --------------------------------------------------------

d = readRDS(file = "posteriors/post_mass_nutrients.rds") %>% 
  filter(region_name != "Greenland")%>% 
  group_by(region_name) %>% 
  mutate(median_region = median(mgDMm2y, na.rm = T)) %>% 
  ungroup %>% 
  group_by(HYBAS_ID, region_name, median_region) %>%
  median_qi(mgDMm2y)

d_summary = d %>% group_by(region_name, median_region) %>% 
  median_qi(mgDMm2y) %>% 
  make_summary_table(center = "mgDMm2y", digits = 0)

write_csv(d_summary, file = "tables/region_perm2.csv")

plot_region_perm2 = d %>% 
  ggplot(aes(x = mgDMm2y, y = reorder(region_name, -median_region))) + 
  stat_density_ridges(aes(fill = median_region), 
                      quantile_lines = T, 
                      quantiles = 2,
                      color = "white") +
  # stat_halfeye(aes(fill = median_region)) +
  scale_x_continuous(limits = c(NA, 5000), 
                     labels = scales::comma) +
  scale_fill_viridis_c(option = "plasma") +
  guides(fill = "none",
         color = "none") +
  labs(y = "",
       x = expression("Annual Emergence Production (mg m"^-2*" yr"^-1*" dry mass)")) +
  # geom_text(data = d_summary, aes(label = center_interval),
  #           nudge_y = 0.2, 
  #           x = 4000,
  #           size = 3,
  #           family = "serif") +
  NULL

ggsave(plot_region_perm2, file = "plots/plot_region_perm2.jpg",
       width = 6.5, height = 6.5, dpi = 400)

# plot flux per m2 per biome ---------------

post_flux_kgdm_perm2_perhybas = readRDS("posteriors/post_flux_kgdm_perm2_perhybas.rds")

d_biome = post_flux_kgdm_perm2_perhybas %>% 
  ungroup %>% 
  left_join(readRDS("data/hybas_covariates.rds")) %>%
  filter(HYBAS_ID %in% hybas_filter) %>% 
  group_by(terr_biom) %>%
  mutate(median_region = median(median, na.rm = T))   %>%
  filter(terr_biom != "NA") %>% 
  mutate(mgDMm2y = median)

d_biome_summary = d_biome %>% 
  group_by(terr_biom, median_region) %>% 
  median_qi(mgDMm2y) %>% 
  make_summary_table(center = "mgDMm2y", digits = 0)

write_csv(d_biome_summary, file = "tables/biome_perm2.csv")

plot_biome_perm2 = d_biome %>% 
  filter(terr_biom != "NA") %>% 
  ggplot(aes(x = mgDMm2y, y = reorder(terr_biom, -median_region))) + 
  stat_density_ridges(aes(fill = as.factor(round(median_region, -2)))) +
  # scale_fill_brewer(type = "div", palette = "RdYlBu", 
  #                   direction = -1) +
  # scale_fill_brewer() +
  scale_fill_grey(start = 0.8, end = 0.2) +
  guides(fill = "none",
         color = "none") +
  labs(y = "",
       x = expression("Annual Emergence Production (mg m"^-2*" yr"^-1*" dry mass)")) +
  xlim(NA, 5000) +
  NULL

ggsave(plot_biome_perm2, file = "plots/plot_biome_perm2.jpg",
       width = 6.5, height = 6.5, dpi = 400)

d_biome %>% 
  group_by(terr_biom) %>% 
  tally()


# compare to salmon via Brandt et al -------------------------------------------------

library(tidyverse)
library(tidybayes)
library(janitor)
library(ggrepel)
library(scales)
library(viridis)

brandt_fig1_data = read_csv("C:/Users/jeff.wesner/OneDrive - The University of South Dakota/USD/Github Projects/salmon_nutrients_contaminants/plots/fig_1c_data.csv") %>% 
  clean_names() %>% 
  # add_row(species = "Insects",          # don't include. These are scaled to earth, but the Huang paper mentions that East China is "16x's" higher flux than Britain. So we'd need to account for spatial variation for these global fluxes to make any sense.
  #         ecosystem = "Global Bioflows",
  #         mechanism = "Aerial Bioflows",
  #         n_flux_annualkg = 7.05e+8,
  #         p_flux_annualkg = 70500000,
  #         source = "Huang et al. 2024 PNAS (our estimate)")%>% 
  add_row(species = "Insects",
          ecosystem = "East China Bioflows",
          mechanism = "Aerial Bioflows",
          n_flux_annualkg = 1500000,
          p_flux_annualkg = 150000,
          source = "Huang et al. 2024 PNAS") %>% 
  add_row(species = "Anadromous Fish",
          ecosystem = "Global",
          mechanism = "Seasonal Migration",
          n_flux_annualkg = 5.6e6*9.09, # Doughty has data for P only, so N is converted using the 0.11:1 P:N ration calculated from Brandt's salmon paper (b/c 1/0.11 = 9.09)
          p_flux_annualkg = 5.6e6,
          source = "Doughty et al. 2016 PNAS")

post_total_all = readRDS(file = "posteriors/post_total_all.rds")

aquasync_np = post_total_all %>% 
  group_by(units, chemical) %>% 
  median_qi(flux) %>% 
  filter(chemical %in% c("N", "P")) %>% 
  select(chemical, flux) %>% 
  pivot_wider(names_from = chemical, values_from = flux) %>% 
  rename(n_flux_annualkg = N,
         p_flux_annualkg = P) %>% 
  mutate(source = "AquaSync",
         species = "Aquatic insects",
         ecosystem = "Global Rivers",
         mechanism = "emergence")

plot_compare_to_salmon = brandt_fig1_data %>% glimpse() %>% 
  bind_rows(aquasync_np) %>% 
  mutate(species_ecosystem = paste0(species, "\n(", ecosystem, ")")) %>% 
  ggplot(aes(x = n_flux_annualkg,
             y = p_flux_annualkg)) +
  geom_point(shape = 21, aes(fill = log10(n_flux_annualkg),
                             # color = log(n_flux_annualkg),
                             size = log10(n_flux_annualkg))) +
  labs(x = expression("N flux (kg yr"^-1*")"),
       y = expression("P flux (kg yr"^-1*")")) +
  scale_x_log10(labels = comma) +
  scale_y_log10(labels = comma) +
  guides(size = "none",
         color = "none",
         fill = "none") +
  geom_text_repel(aes(label = species_ecosystem), size = 2) +
  scale_fill_viridis() +
  scale_color_viridis() +
  brms::theme_default() +
  NULL

ggsave(plot_compare_to_salmon, file = "plots/plot_compare_to_salmon.jpg", 
       width = 6.5, height = 6.5)


plot_compare_to_salmon_linearscale = brandt_fig1_data %>% 
  bind_rows(aquasync_np) %>% 
  mutate(species_ecosystem = paste0(species, "\n(", ecosystem, ")")) %>% 
  ggplot(aes(x = n_flux_annualkg,
             y = p_flux_annualkg)) +
  geom_text_repel(aes(label = species_ecosystem), size = 2, max.overlaps = 100, alpha = 0.5) +
  geom_point(shape = 21, aes(fill = log10(n_flux_annualkg),
                             # color = log(n_flux_annualkg),
                             size = log10(n_flux_annualkg))) +
  labs(x = expression("N flux (kg yr"^-1*")"),
       y = expression("P flux (kg yr"^-1*")")) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  guides(size = "none",
         color = "none",
         fill = "none")  +
  scale_fill_viridis() +
  scale_color_viridis() +
  brms::theme_default() +
  NULL

ggsave(plot_compare_to_salmon_linearscale, 
       file = "plots/plot_compare_to_salmon_linearscale.jpg", 
       width = 6.5, height = 6.5)
