library(tidyverse)
library(brms)
library(janitor)
library(tidybayes)
library(viridis)
library(scales)
library(ggridges)
theme_set(theme_default())


# Figure S8: Water contaminant ranges -------------------------------------
# Get ranges of aqueous contaminant concentrations globally and compare to the
# range of contaminants in the HYBAS for which we have emergence measured. This 
# provides a glimpse of the amount of mortality-driven contaminant effects that might
# already be accounted for in our estimates of emergence.

cas_names = readRDS("C:/Users/jeff.wesner/OneDrive - The University of South Dakota/USD/Github Projects/secondary-production/data/cas_names.rds") %>% 
  mutate(chemical = case_when(chemical == "1,2,4,5,6,7,8,8-Octachloro-2,3,3a,4,7,7a-hexahydro-4,7-methano-1<em>H</em>-indene" ~ "Chlordane", 
                              T ~ chemical)) 
modeled_water = as_tibble(readRDS(file = "data/modeled_water.rds")) %>% # values have been corrected for minimums with essential elements (i.e., if water concentrations indicate zero Se but still has emergence, then we need to assign a minimum amount to flux b/c flux of Se in tissues can't also be zero)
  left_join(cas_names) %>% 
  mutate(water_ug_l_raw = 10^(mean.conc.year * mean.det.year)) %>% 
  filter(chemical != "Propyzamide")

# load raw emergence
emergence = readRDS(file = 'data/emergence_production_with_vars.rds') %>% 
  mutate(HYBAS_ID = as.character(HYBAS_ID)) %>% 
  left_join(readRDS("data/HYBAS_surface_area_REDIST.rds") %>% 
              mutate(HYBAS_ID = as.character(HYBAS_ID))) %>% # add area of HYBAS water
  mutate(raw_kg_perhybas = mean_emergence_mgdmm2y*area.redist)  # kg/km2 is the same as mg/m2 so this works to produce kg per hybas

modeled_water_wide = modeled_water %>% 
  filter(HYBAS_ID %in% unique(emergence$HYBAS_ID)) %>% 
  select(water_ug_l_raw, HYBAS_ID, chemical) 

modeled_water_global_empirical = modeled_water %>% 
  select(water_ug_l_raw, HYBAS_ID, chemical) %>% 
  mutate(data = "Hydrobasins with empirical emergence data") %>% 
  sample_n(1e6) %>% 
  bind_rows(modeled_water_wide %>% 
              mutate(data = "Hydrobasins globally")) %>% 
  group_by(chemical) %>% 
  mutate(median = median(water_ug_l_raw)) %>% 
  mutate(chem = str_sub(chemical, 1, 25)) %>% 
  mutate(data = fct_relevel(data, "Hydrobasins with empirical emergence data"))

water_comparisons_densities = modeled_water_global_empirical %>% 
  ggplot(aes(x = water_ug_l_raw, fill = data, y = reorder(chem, -median),
             alpha = data)) +
  geom_density_ridges() +
  scale_x_log10() +
  scale_alpha_manual(values = c(1, 0.6)) +
  scale_fill_manual(values = c("#6060F9", "#FB6262")) +
  labs(y = "",
       x = expression("Water Concentration (\u00b5g L"^-1*")"),
       fill = "") +
  guides(alpha = "none") +
  theme(legend.position = c(0.745, 1),
        legend.background = element_rect(fill="white",
                                         linewidth=1, linetype="solid", 
                                         color ="white"),
        legend.text = element_text(size = 8),
        text = element_text(family = "sans"))

ggsave(water_comparisons_densities, 
       file = "plots/Figure_S8.jpg", 
       dpi = 400, width = 6.5, height = 7)

# Figure S11: contaminant regressions -------------------------------------


mod_list = readRDS(file = "models/mod_list.rds")

posts_list = list()
for(i in 1:length(mod_list)){
  posts_list[[i]] = tibble(x_s = seq(min(mod_list[[i]]$data$x_s),
                                     max(mod_list[[i]]$data$x_s),
                                     length.out = 30)) %>%
    mutate(pub_name = "new",
           mean_x = mod_list[[i]]$data2$mean_x$`scaled:center`,
           sd_x = mod_list[[i]]$data2$sd_x$`scaled:scale`,
           max_y = mod_list[[i]]$data2$max_y,
           chemical_category = mod_list[[i]]$data2$chemical_category) %>%
    add_epred_draws(mod_list[[i]], allow_new_levels = T, re_formula = NULL)
}

saveRDS(posts_list, file = "posteriors/posts_list.rds")
posts_list = readRDS(file = "posteriors/posts_list.rds")
# 
chem_regression_posts = bind_rows(posts_list) %>%
  mutate(chemical_category = str_to_sentence(chemical_category))

saveRDS(chem_regression_posts, file = "posteriors/chem_regression_posts.rds")

chem_regression_posts = readRDS(file = "posteriors/chem_regression_posts.rds")

raw_contaminants = readRDS(file = "data/contaminants.rds") %>%
  group_by(chemical_category) %>% 
  mutate(max_y = max(adult_conc_ng_mg_dm,na.rm = T),
         y_s = adult_conc_ng_mg_dm/max_y,
         log_water_conc_ugl_01 = log(water_conc_ug_l + 0.001*mean(water_conc_ug_l, na.rm = T)),
         x_s = scale(log_water_conc_ugl_01)) %>% 
  filter(chemical_category %in% unique(bind_rows(posts_list)$chemical_category)) %>% 
  mutate(chemical_category = str_to_sentence(chemical_category)) %>% 
  filter(chemical_category != "Pharmaceuticals") 


chem_concentrations_posts = chem_regression_posts %>% 
  filter(chemical_category != "Pharmaceuticals") %>% 
  ggplot(aes(x = x_s, y = .epred*max_y)) + 
  stat_lineribbon(alpha = 0.4, .width = 0.95) + 
  facet_wrap(~chemical_category, scales = "free") +
  # scale_y_log10() +
  labs(y = expression("Tissue Concentration (ng mg"^-1*" dry mass)"),
       x = expression("Water Concentration (\u00b5g L"^-1*" z-score)")) +
  guides(fill = "none") +
  geom_point(data = raw_contaminants, aes(y = adult_conc_ng_mg_dm), shape = 1, size = 0.5) +
  theme(axis.text = element_text(size = 7),
        strip.text = element_text(size = 7),
        text = element_text(family = "sans"),
        plot.subtitle = element_text(face = "bold")) 

ggsave(chem_concentrations_posts, file = "plots/Figure_S11.jpg",
       width = 6.5, height = 6.5)


