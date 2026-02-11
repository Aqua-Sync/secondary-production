library(tidyverse)
library(brms)
library(tidybayes)
library(ggridges)
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

# load area of water in each hybas (km2)
hybas_area = readRDS("data/HYBAS_surface_area_REDIST.rds") # redistributed surface areas from Jakob. 

keep_names = names(readRDS(file = "posteriors/hybas_predictions_metals.rds")) # columns to retain in both metals and cides


hybas_contaminant_predictions = readRDS(file = "posteriors/hybas_predictions_metals.rds") %>% 
  bind_rows(readRDS(file = "posteriors/hybas_predictions_pest_herb_fungicide_filtered.rds") %>% 
              select(any_of(keep_names))) %>% 
  mutate(HYBAS_ID = as.numeric(HYBAS_ID)) %>% 
  left_join(hybas_area) %>% 
  left_join(readRDS("data/hybas_covariates.rds") %>% select(HYBAS_ID, terr_biom)) %>% 
  left_join(readRDS("data/hybas_regions.rds")) 


# biomes --------------------------------------------------------------

d_region_contaminants = hybas_contaminant_predictions %>% 
  group_by(terr_biom, element) %>% 
  # slice_sample(prop = 0.05) %>% # sample 1% of hybas in each region
  mutate(across(starts_with("chem"), ~ . / area.redist)) %>% # convert measures to per m2 by dividing by water area
  group_by(terr_biom, element) %>% 
  mutate(median_region = case_when(grepl("cide", element) ~ mean(chem_flux_mg_year, na.rm = T),
                                   TRUE ~ median(chem_flux_mg_year, na.rm = T))) %>%  # add median to sort by
  filter(!is.na(chem_flux_mg_year)) %>% 
  group_by(element) %>% 
  mutate(relative = median_region/max(median_region)) # add relative flux scale for colors


cont_plots = function(data = d_region_contaminants, elements, rows = 3,
                      pred = chem_flux_mg_year, prob_cutoff = 0.99,
                      region = terr_biom){
  d = data %>% 
    filter(element %in% c(elements))
  
  d %>% 
    ggplot(aes(x = {{pred}} , y = reorder({{region}}, -median_region))) + 
    stat_density_ridges(aes(fill = relative), 
                        quantile_lines = T, 
                        quantiles = 2,
                        color = "white") +
    # scale_x_log10(labels = scales::comma, limits = c(1, NA)) +
    scale_x_continuous(labels = scales::comma, expand = c(0, 0)) +
    coord_cartesian(xlim = c(0, quantile(d$chem_flux_mg_year, probs = prob_cutoff))) +
    scale_fill_viridis_c(option = "plasma", begin = 0.2, end = 0.8) +
    guides(fill = "none",
           color = "none") +
    labs(y = "",
         x = expression("Annual flux (mg/m"^2*"/y)")) +
    # geom_text(data = d_region_summary_contaminants, aes(label = center_interval),
    #           nudge_y = 0.2,
    #           x = 4000,
    #           size = 3,
    #           family = "serif") +
    NULL 
}

d_list_biome = d_region_contaminants %>% mutate(element = as.factor(element),
                                          element = fct_relevel(element, "Se", "Zn", "Cu", "Cd", "Pb", "Hg", "fungicide", "herbicide", "insecticide")) %>% 
  group_by(element) %>% group_split()

plots_biome = NULL

for(i in 1:length(d_list_biome)){
  plots_biome[[i]] = cont_plots(d_list_biome[[i]],
                          elements = unique(d_list_biome[[i]]$element)) + 
    labs(subtitle = paste(letters[i], ")", unique(d_list_biome[[i]]$element)))
}


library(patchwork)

essential_biome = (plots_biome[[1]] + theme(axis.title.x = element_blank(),
                                            title = element_text(size = 9)) +
               labs(title = "Essential Metalloids"))/
  (plots_biome[[2]] + theme(axis.title.x = element_blank()))/
  plots_biome[[3]]

essential_biome

nonessential_biome = (plots_biome[[4]] + theme(axis.title.x = element_blank(),
                                   axis.text.y = element_blank(),
                                   title = element_text(size = 9)) +
                  labs(title = "Non-essential Metalloids"))/
  (plots_biome[[5]] + theme(axis.title.x = element_blank(),
                      axis.text.y = element_blank()))/
  (plots_biome[[6]] + theme(axis.text.y = element_blank()))

nonessential_biome

plots_cides_biome = NULL

for(i in 7:9){
  plots_cides_biome[[i]] = cont_plots(d_list_biome[[i]],
                                elements = unique(d_list_biome[[i]]$element),
                                prob_cutoff = 0.9, 
  ) + 
    labs(subtitle = paste(letters[i], ")", unique(d_list_biome[[i]]$element)))
}

cides_biome = (plots_cides_biome[[7]] + theme(axis.title.x = element_blank(),
                                  axis.text.y = element_blank(),
                                  title = element_text(size = 9)) +
           labs(title = "Fungicides/Herbicides/Insecticides"))/
  (plots_cides_biome[[8]] + theme(axis.title.x = element_blank(),
                            axis.text.y = element_blank()))/
  (plots_cides_biome[[9]] + theme(axis.text.y = element_blank()))

cides_biome



library(cowplot)
plot_contaminants_perm2_biomes = plot_grid(essential_biome, nonessential_biome, cides_biome, ncol = 3,
          rel_widths = c(1.2, 0.6, 0.6))

ggsave(plot_contaminants_perm2_biomes, file = "plots/plot_contaminants_perm2_biomes.jpg", 
       width = 12, height = 9)
