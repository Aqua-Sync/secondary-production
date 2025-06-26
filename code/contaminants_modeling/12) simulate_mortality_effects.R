library(tidyverse)
library(brms)
library(tidybayes)
library(bit64)
library(cccapi)

# load data and models
# raw contaminants data
contaminants = readRDS(file = "data/contaminants.rds") %>% 
  mutate(chemical = case_when(chemical_category == "Se" ~ "Selenium",
                              chemical_category == "Zn" ~ "Zinc",
                              chemical_category == "Hg" ~ "Mercury",
                              chemical_category == "Pb" ~ "Lead",
                              chemical_category == "Cu" ~ "Copper",
                              chemical_category == "Cd" ~ "Cadmium"
  ))

# load dry mass emergence predictions
flux_predictions_all = readRDS(file = "posteriors/hybas_predictions_mass_nutrients.rds") %>%
  filter(grepl("kgdm", units)) %>% 
  left_join(readRDS("data/hybas_regions.rds")) %>% 
  left_join(readRDS("data/HYBAS_surface_area_REDIST.rds") %>% 
              mutate(HYBAS_ID = as.numeric(HYBAS_ID))) %>% 
  left_join(readRDS("data/hydrobasin_vars_rssa_short.rds") %>% 
              select(HYBAS_ID, BA_km2)) 

# contaminant cas numbers and names
cas_names = readRDS(file = "data/cas_names.rds") %>% 
  mutate(chemical_category = case_when(chemical == "Selenium" ~ "Se",
                                       chemical == "Zinc" ~ "Zn",
                                       chemical == "Mercury" ~ "Hg",
                                       chemical == "Lead" ~ "Pb",
                                       chemical == "Copper" ~ "Cu",
                                       chemical == "Cadmium" ~ "Cd"
  ))
# Wolfram predictions of contaminants. Generated in wrangle_modeled_water.R. It uses water concentrations from Jakob Wolfram on seafile.rlp.net...then reformats.
modeled_water = readRDS(file = "data/modeled_water.rds") # values have been corrected for minimums with essential elements (i.e., if water concentrations indicate zero Se but still has emergence, then we need to assign a minimum amount to flux b/c flux of Se in tissues can't also be zero)

cas_zinc = cas_names %>% filter(chemical == "Zinc")
modeled_zinc = modeled_water %>% filter(cas == unique(cas_zinc$cas))


quantile(10^modeled_zinc$max.conc.year)

flux_predictions_zinc = flux_predictions_all %>% 
  left_join(modeled_zinc) %>% 
  mutate(raw_ug_l_max = 10^(max.conc.year * mean.det.year),
         raw_ug_l_mean = 10^(mean.conc.year * mean.det.year)) %>% 
  pivot_longer(cols = starts_with("raw_ug"))

zinc_distribution = flux_predictions_zinc %>% 
  ggplot(aes(x = value + 1)) + 
  geom_histogram(bins = 100, aes(color = name)) +
  # facet_wrap(~name) +
  geom_vline(aes(xintercept = 120), linetype = "dashed") +
  annotate(geom = "text", x = 370, y = 750000,
           label = "EPA Aquatic Life Criterion",
           size = 3) +
  theme_default() +
  labs(y = "Number of sub-basins", 
       x = "ug/l Zinc",
       color = "",
       subtitle = "Distribution of max and mean aqueous Zinc concentrations across 1,034,083 sub-basins") +
  theme(text = element_text(size = 10))

ggsave(zinc_distribution, file = "plots/zinc_distribution.jpg", 
       width = 6.5, height = 6.5)


quantile(flux_predictions_zinc$mean.conc.year, na.rm = T)

a = flux_predictions_zinc %>% 
  filter(raw_ug_l >= 100) %>%
  # filter(mean.conc.year >= 1.219) %>% 
  mutate(mean_adjusted = 0.03*mean,
         mortality_effect = "yes")

b = flux_predictions_zinc %>% 
  anti_join(a %>% select(HYBAS_ID)) %>% 
  mutate(mean_adjusted = mean,
         mortality_effect = "no")

ab = bind_rows(a, b)

sum(ab$mean)/1e9
sum(ab$mean_adjusted)/1e9


left_join(flux_predictions_zinc %>% 
            mutate(mortality_effect = "no"),
          a %>% select(HYBAS_ID, mean_adjusted)) %>% 
  mutate(raw_ug_l = 10^mean.conc.year) %>% 
  sample_n(10000) %>% 
  ggplot(aes(x = raw_ug_l, y = mean, color = "not adjusted")) +
  geom_point() +
  geom_point(aes(y = mean_adjusted, color = "adjusted")) +
  scale_y_log10() +
  scale_x_log10() +
  NULL


sum(flux_predictions_all$mean)/1e9
sum(ab$mean_adjusted)/1e9


ab %>% 
  filter(mean > 0) %>% 
  # sample_n(100000) %>% 
  ggplot(aes(x = mortality_effect, y = mean + 1)) + 
  geom_violin(aes(group = mortality_effect)) +
  scale_y_log10()

# Cumulative proportion of flux -------------------------------------------

hist(flux_predictions_all$mean)

cumulative_flux = flux_predictions_all %>% 
  select(HYBAS_ID, mean) %>% 
  arrange(-mean) %>% 
  mutate(cumsum = cumsum(mean),
         cumpercent = cumsum/sum(mean)) 

eighty_percent = cumulative_flux %>% 
  filter(cumpercent <= 0.8)

nrow(eighty_percent)/nrow(flux_predictions_all)

cumulative_flux %>% 
  mutate(id = 1:nrow(.)) %>% 
  sample_n(1000) %>% 
  ggplot(aes(x = id, y = cumpercent)) +
  geom_line()



# use flux measures directly from contaminants meta-analysis --------------

contaminants %>% 
  filter(!is.na(emergence_mgdm_m2_y)) %>% 
  separate(emergence_mgdm_m2_y, into = c("mgdm_m2_y", "delete1", "delete2"), sep = " ") %>% 
  mutate(mgdm_m2_y = parse_number(mgdm_m2_y)) %>% 
  mutate(log_water_conc_ugl_01 = log(water_conc_ug_l + 0.001*mean(water_conc_ug_l, na.rm = T)),
         x_s = scale(log_water_conc_ugl_01)) %>% 
  ggplot(aes(x = x_s, y = mgdm_m2_y)) +
  geom_point()
  
cas_keep = cas_names %>% filter(chemical %in% c("Zinc", "Lead", "Cadmium"))
modeled_zn = modeled_water %>% filter(cas == cas_keep %>% filter(chemical == "Zinc") %>% pull(cas)) %>% 
  mutate(zn = mean.conc.year)
modeled_pb = modeled_water %>% filter(cas == cas_keep %>% filter(chemical == "Lead") %>% pull(cas)) %>% 
  mutate(pb = mean.conc.year)
modeled_cd = modeled_water %>% filter(cas == cas_keep %>% filter(chemical == "Cadmium") %>% pull(cas)) %>% 
  mutate(cd = mean.conc.year)


modeled_znpbcd = left_join(modeled_zn, modeled_pb %>% select(HYBAS_ID, pb)) %>% 
  left_join(modeled_cd %>% select(HYBAS_ID, cd))

theme_set(brms::theme_default())

pa = modeled_znpbcd %>% 
  sample_n(10000) %>%
  ggplot(aes(x = zn, y = pb)) + 
  geom_point(shape = ".") +
  labs(x = "log10 Zn ug/l",
       y = "log10 Pb ug/l") +
  ylim(-3, 3) +
  geom_smooth(method = lm) +
  xlim(-3, 3)

pb = modeled_znpbcd %>% 
  sample_n(10000) %>%
  ggplot(aes(x = zn, y = cd)) + 
  geom_point(shape = ".") +
  labs(x = "log10 Zn ug/l",
       y = "log10 Cd ug/l") +
  geom_smooth(method = lm) +
  ylim(-3, 3) +
  xlim(-3, 3)

pc = modeled_znpbcd %>% 
  sample_n(10000) %>%
  ggplot(aes(x = cd, y = pb)) + 
  geom_point(shape = ".") +
  labs(x = "log10 Cd ug/l",
       y = "log10 Pb ug/l") +
  geom_smooth(method = lm) +
  ylim(-3, 3) +
  xlim(-3, 3)

library(patchwork)

metal_pairs = pa + pb + pc
ggsave(metal_pairs, file = "plots/metal_pairs.jpg", width = 6.5, height = 3)


flux_with_metals = flux_predictions_all %>% left_join(modeled_znpbcd %>% select(HYBAS_ID, zn, pb, cd))

metal_regress1 = flux_with_metals %>% 
  sample_n(30000) %>% 
  filter(zn > -2) %>% 
  ggplot(aes(x = zn, y = mean + 1)) + 
  geom_point(shape = ".") +
  geom_smooth(method = lm) +
  scale_y_log10() +
  labs(y = "Emergence biomass per year")

metal_regress2 = flux_with_metals %>% 
  sample_n(30000) %>% 
  ggplot(aes(x = pb, y = mean + 1)) + 
  geom_point(shape = ".") +
  geom_smooth(method = lm) +
  scale_y_log10() +
  labs(y = "Emergence biomass per year")

metal_regress3 = flux_with_metals %>% 
  sample_n(30000) %>% 
  ggplot(aes(x = cd, y = mean + 1)) + 
  geom_point(shape = ".") +
  geom_smooth(method = lm) +
  scale_y_log10() +
  labs(y = "Emergence biomass per year")

metal_regress = metal_regress1 + metal_regress2 + metal_regress3
ggsave(metal_regress, file = "plots/metal_regress.jpg", width = 8.5, height = 3)
