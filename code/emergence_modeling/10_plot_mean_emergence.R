library(tidyverse)
library(brms)
library(tidybayes)
theme_set(theme_default())

emergence_production_with_vars = readRDS(file = 'data/emergence_production_with_vars.rds')
updated_gams = readRDS("models/updated_gams.rds")
max_emergence <- max(emergence_production_with_vars$mean_emergence_mgdmm2y, na.rm = T)

mod_dat = updated_gams[[1]]$data

# plot emergence conditional
tibble(precip_s = seq(min(mod_dat$precip_s),
                      max(mod_dat$precip_s),
                      length.out = 30)) %>% 
  add_epred_draws(updated_gams[[1]], re_formula = NA) %>% 
  ggplot(aes(x = precip_s, y = .epred*max_emergence)) +
  stat_lineribbon(.width = 0.95, alpha = 0.6) +
  geom_point(data = mod_dat, aes(y = emerge_1*max_emergence),
             size = 0.5) +
  guides(fill = "none") +
  labs(y = "Annual Emergence Production (mgDMm2y)",
       x = "Annual Precipitation (z-score)")


# Gratton measured flux between 0.4 and 3.1 gCm2y with mean of ~1gCm2y
low_gratton = 1000*(0.4*2)/0.9
high_gratton = 1000*(3.1*2)/0.9
mean_gratton = 1000*(1.052*2)/0.9
# get mean emergence production
post_mgdmm2y = mod_dat %>% 
  distinct(precip_s) %>% 
  add_epred_draws(updated_gams[[1]], re_formula = NA) %>% 
  mutate(.epred = .epred*max_emergence) %>% 
  group_by(.draw) %>% 
  reframe(mean_mgDMm2y = mean(.epred))

post_mgdmm2y %>% median_qi(mean_mgDMm2y)

post_mgdmm2y  %>% 
  ggplot(aes(x = mean_mgDMm2y)) +
  # stat_histinterval() +
  # stat_dotsinterval() +
  stat_halfeye() +
  # stat_dist_dots() +
  geom_vline(xintercept = c(mean_gratton, low_gratton, high_gratton)) +
  geom_point(data = emergence_production_with_vars, 
             aes(x = mean_emergence_mgdmm2y), shape = "|",
             y = 0) +
  scale_x_log10() +
  NULL
