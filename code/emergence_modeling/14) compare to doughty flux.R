library(tidyverse)
library(tidybayes)
library(janitor)
library(ggrepel)
library(scales)
library(viridis)
library(brms)
library(ggridges)
theme_set(theme_default())

post_total_all = readRDS(file = "posteriors/post_total_all.rds")

aquasync_p = post_total_all %>% 
  group_by(units, chemical) %>% 
  median_qi(flux) %>% 
  filter(chemical %in% c("P")) %>% 
  select(chemical, flux) %>% 
  pivot_wider(names_from = chemical, values_from = flux) %>% 
  rename(p_flux_annualkg = P) %>% 
  mutate(source = "AquaSync",
         species = "Aquatic insects",
         ecosystem = "Global Rivers",
         mechanism = "emergence")

compare_to_doughty = post_total_all %>% 
  filter(chemical == "P") %>% 
  ggplot(aes(x = flux/1000)) +
  # stat_density_ridges(quantile_lines = TRUE, quantiles = 2, aes(y = 0)) +
  stat_halfeye() +
  # geom_vline(xintercept = c(5.6e6,     # P of Anadromous Fish global from Doughty et al. 2016 PNAS
                            # 6.3e6)) + # P of Sea birds to land via guano global from Doughty et al. 2016 PNAS
  geom_segment(aes(x= 5.6e6/1000,
                   xend = 5.6e6/1000,
                   y = 0,
                   yend = 0.88),
               linetype = "dashed") +
  geom_segment(aes(x= 6.3e6/1000,
                   xend = 6.3e6/1000,
                   y = 0,
                   yend = 0.88),
               linetype = "dotted") +
  geom_segment(aes(x = median(post_total_all %>% filter(chemical == "P") %>% pull(flux))/1000,
                   xend = median(post_total_all %>% filter(chemical == "P") %>% pull(flux))/1000,
                   y = 0,
                   yend = 0.88)) +
  scale_x_log10(labels = comma,
                breaks = c(2500, 5000, 10000, 20000)) +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank()) +
  labs(x = "Global P export per year (t)")

ggsave(compare_to_doughty, file = "plots/compare_to_doughty.jpg", width = 6, height = 6)  


# per continent -----------------------------------------------------------

doughty_estimates = read_csv("data/doughty_estimates.csv") %>% pivot_longer(cols = c(-source, -organism, -units)) %>% 
  rename(region_name = name)
hybas_regions = readRDS("data/hybas_regions.rds")
hybas_predictions_kgP_peryear = readRDS("posteriors/hybas_predictions_kgP_peryear.rds") %>% 
  left_join(hybas_regions)

doughty_regions = hybas_predictions_kgP_peryear %>% distinct(region_name) %>% 
  left_join(doughty_estimates) %>% 
  filter(!is.na(value))

hybas_predictions_kgP_peryear %>% 
  group_by(region_name) %>% 
  reframe(flux = sum(mean)) %>% 
  filter(region_name %in% unique(doughty_regions$region_name)) %>% 
  ggplot(aes(x = flux/1000, y = region_name)) +
  geom_point(color = "red3", shape = 21, size = 3) +
  # scale_x_log10() +
  geom_point(data = doughty_regions, aes(x = value/1000, color = organism, shape = organism)) +
  NULL
