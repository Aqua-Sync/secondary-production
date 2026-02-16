library(tidyverse)
library(tidybayes)
library(ggridges)
library(ggrepel)
library(brms)
theme_set(brms::theme_default())

# Use the fitted parameters from regression models to predict emergence at unmeasured sites

# 1) load data and models -------------------------------------------------
# load data
emergence_production_with_vars = readRDS(file = 'data/emergence_production_with_vars.rds')

hydrobasin_vars_rssa_short = readRDS(file = "data/hydrobasin_vars_rssa_short.rds")

# load area of water in each hybas (km2)
hybas_area = readRDS("data/HYBAS_surface_area_REDIST.rds") # redistributed surface areas from Jakob. 

# countries to retain (from email with Stefano Larsen on 9/23/2025 315AM)
countries_to_retain = tibble(country = c("Tanzania",
                                         "China",
                                         "England",
                                         "Morocco",
                                         "Algeria"),
                             gad_id_smj = c(225, 48, 239, 152, 4),
                             comparisons = c("Wildebeest & Hippos",
                                             "Flying Terrestrial Insects",
                                             "Flying Terrestrial Insects",
                                             "Locusts",
                                             "Locusts"),
                             country_region = c("Tanzania",
                             "China",
                             "England",
                             "North Africa",
                             "North Africa"))

data_to_predict = readRDS("data/data_to_predict.rds") %>% 
  filter(SUB_AREA > 0) %>% 
  left_join(hydrobasin_vars_rssa_short %>% select(HYBAS_ID, gad_id_smj)) %>% 
  filter(gad_id_smj %in% unique(countries_to_retain$gad_id_smj)) %>% 
  left_join(countries_to_retain) %>% 
  left_join(hybas_area)

data_to_predict_list = data_to_predict %>% 
  group_by(country_region) %>% group_split() # basin-level predictor variables by continent

hybas_regions <- readRDS("data/hybas_regions.rds")
post_pufa = readRDS(file = "posteriors/post_pufa.rds")

# load models
updated_gams = readRDS("models/updated_gams.rds")

# get max emergence to unstandardize
max_emergence = max(emergence_production_with_vars$mean_emergence_mgdmm2y, na.rm = T)



# 2) sample posterior -----------------------------------------------------
# dry mass

# estimates median and CrI's of emergence per hybas in standardized units of mgdmm2y/max(mgdmm2y)
post_summary_bycountry = vector("list", length(data_to_predict_list))
# 
for(i in seq_along(data_to_predict_list)) {
  post_summary_bycountry[[i]] = data_to_predict_list[[i]] %>%
    # slice(1:5) %>%
    select(HYBAS_ID, precip_s, stream_temp_s, area.redist, gad_id_smj, country, country_region) %>%
    mutate(author_year = "new") %>%
    add_epred_draws(updated_gams[[3]], allow_new_levels = TRUE, re_formula = NULL, ndraws = 100) %>%
    mutate(kgdmkm2y = .epred*max_emergence,
           kgdmhybasyr = kgdmkm2y*area.redist) %>%
    group_by(country_region, .draw) %>%
    reframe(.epred = sum(kgdmhybasyr))
}

saveRDS(bind_rows(post_summary_bycountry), file = "posteriors/post_summary_bycountry.rds")

post_summary_bycountry = readRDS(file = "posteriors/post_summary_bycountry.rds")

post_np = bind_rows(post_summary_bycountry) %>% 
  mutate(kgN_peryear = .epred/6.3,
         kgP_peryear = .epred/124) %>% 
  select(-.epred) %>% 
  pivot_longer(cols = starts_with("kg"))

literature_comparisons = read_csv("data/literature_comparisons.csv") %>% 
  pivot_longer(cols = starts_with("kg"))

median = post_np %>% 
  group_by(name, country_region) %>% 
  reframe(median_qi(value))
  
compare_flux_per_country = post_np %>% 
  ggplot(aes(x = value, fill = name)) + 
  geom_density() +
  scale_x_log10() +
  geom_vline(data = median, aes(xintercept = y), color = "white") +
  facet_grid(country_region~name) +
  theme(strip.text = element_text(hjust = 0),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank()) +
  guides(fill = "none") +
  geom_text_repel(data = literature_comparisons %>% filter(country_region != "North America"), aes(label = organism, y = 0),
                   size = 2.5,
                  nudge_x = -2,
                  nudge_y = 0.5) +
  labs(x = "Kilograms per year") 

ggsave(compare_flux_per_country, file = "plots/compare_flux_per_country.jpg", width = 6.5, height = 9)
write_csv(median, file = "tables/median_flux_per_country.csv")
