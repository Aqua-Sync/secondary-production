library(tidyverse)
library(brms)
library(tidybayes)
theme_set(theme_default())

emergence_production_with_vars = readRDS(file = 'data/emergence_production_with_vars.rds')
updated_gams = readRDS("models/updated_gams.rds")
max_emergence <- max(emergence_production_with_vars$mean_emergence_mgdmm2y, na.rm = T)

mod_dat = updated_gams[[1]]$data

# plot emergence conditional
precip_emergence_plot = tibble(precip_s = seq(min(mod_dat$precip_s),
                      max(mod_dat$precip_s),
                      length.out = 30)) %>% 
  mutate(HYBAS_ID = "new") %>% 
  add_epred_draws(updated_gams[[1]], re_formula = NULL, allow_new_levels = T) %>% 
  ggplot(aes(x = precip_s, y = .epred*max_emergence)) +
  stat_lineribbon(alpha = 0.4) +
  geom_point(data = mod_dat, aes(y = emerge_1*max_emergence),
             size = 0.5) +
  # guides(fill = "none") +
  labs(y = "Annual Emergence Production (mgDMm2y)",
       x = "Annual Precipitation (z-score)",
       subtitle = "Our nonlinear model - Precipitation")

temp_dat = updated_gams[[2]]$data
temp_emergence_plot = tibble(stream_temp_s = seq(min(temp_dat$stream_temp_s),
                                              max(temp_dat$stream_temp_s),
                                              length.out = 30)) %>% 
  mutate(HYBAS_ID = "new") %>% 
  add_epred_draws(updated_gams[[2]], re_formula = NULL, allow_new_levels = T) %>% 
  ggplot(aes(x = stream_temp_s, y = .epred*max_emergence)) +
  stat_lineribbon(alpha = 0.4) +
  geom_point(data = temp_dat, aes(y = emerge_1*max_emergence),
             size = 0.5) +
  # guides(fill = "none") +
  labs(y = "Annual Emergence Production (mgDMm2y)",
       x = "Mean Annual Temperature (z-score)",
       subtitle = "b) Our nonlinear model - Temperature")


library(patchwork)


patrick_plot = ggplot(data = temp_dat, aes(x = stream_temp_s)) +
  geom_point(data = temp_dat, aes(y = emerge_1*max_emergence),
             size = 0.5) +
  geom_smooth(method = lm, aes(y = emerge_1*max_emergence)) +
  # guides(fill = "none") +
  labs(y = "Annual Emergence Production (mgDMm2y)",
       x = "Mean Annual Temperature (z-score)",
       subtitle = "Simple linear regression, similar to Patrick et al.")

emergence_three_plots = (precip_emergence_plot + labs(y = ""))/temp_emergence_plot/(patrick_plot + labs(y = ""))

ggsave(emergence_three_plots, file = "plots/emergence_three_plots.jpg", width = 6, height = 9)


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
  ggplot(aes(x = mean_mgDMm2y/1000)) +
  # stat_histinterval() +
  # stat_dotsinterval() +
  stat_halfeye() +
  # stat_dist_dots() +
  geom_point(data = emergence_production_with_vars, 
             aes(x = mean_emergence_mgdmm2y/1000), shape = 20,
             y = 0, alpha = 0.3) +
  geom_vline(xintercept = c(mean_gratton/1000, low_gratton/1000, high_gratton/1000)) +
  # scale_x_log10() +
  NULL
