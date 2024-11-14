library(tidyverse)
library(brms)
library(janitor)
library(tidybayes)
library(viridis)
library(scales)

# load data
contaminants = readRDS(file = "data/contaminants.rds")

# number of records per category
contaminants_summary = contaminants %>% 
  pivot_longer(cols = c(water_conc_ug_l, sediment_conc_ug_g, adult_conc_ng_mg_dm)) %>% 
  group_by(chemical, name, chemical_category) %>%
  filter(!is.na(value)) %>% 
  tally() %>% 
  arrange(-n) %>% 
  pivot_wider(names_from = name, values_from = n) 

write_csv(contaminants_summary, file = "data/contaminants_summary.csv")


contaminants %>% 
  pivot_longer(cols = c(water_conc_ug_l, sediment_conc_ug_g, adult_conc_ng_mg_dm)) %>% 
  group_by(chemical_category, name) %>%
  filter(!is.na(value)) %>% 
  tally() %>% 
  arrange(-n) %>% 
  pivot_wider(names_from = name, values_from = n)

chemical_analytes = contaminants %>% 
  group_by(chemical_category, chemical) %>% 
  tally() %>% 
  arrange(-n)

write_csv(chemical_groups, file = "data/chemical_groups.csv")
write_csv(chemical_analytes, file = "data/chemical_analytes.csv")


# plot

all_contaminants_plot = contaminants %>%  
  ggplot(aes(x = water_conc_ug_l + 0.01, 
             y = adult_conc_ng_mg_dm + 0.01, 
             color = chemical_category)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(aes(color = chemical_category), method = lm) +
  facet_wrap(~chemical_category, scales = "free") +
  guides(color = "none")

ggsave(all_contaminants_plot, file = "plots/all_contaminants_plot.jpg", 
       width = 9, height = 8)


contaminants %>%  
  ggplot(aes(x = water_conc_ug_l + 0.01, 
             y = adult_conc_ng_mg_dm + 0.01, 
             color = chemical_category)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(aes(color = chemical_category, group = chemical), method = lm) +
  facet_wrap(~chemical_category, scales = "free") +
  guides(color = "none")


contaminants %>% 
  filter(chemical_category == "pharmaceuticals") %>% 
  # filter(chemical != "Caffeine") %>%
  ggplot(aes(x = water_conc_ug_l , y = adult_conc_ng_mg_dm  )) + 
  geom_point(aes(color = chemical)) +
  scale_x_log10() +
  # scale_y_log10() +
  NULL
