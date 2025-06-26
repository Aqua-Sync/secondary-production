library(tidyverse)
library(brms)
library(janitor)
library(tidybayes)
library(scales)

theme_set(theme_default())

# load prefit models
updated_gams = readRDS("models/updated_gams.rds") # stores all of the individual models below

pp_data_list = list()

for(i in 1:length(updated_gams)){
pp_data_list[[i]] = pp_check(updated_gams[[i]])$data %>% 
  mutate(formula = as.character(updated_gams[[i]]$formula)[1],
         model_number = i)
}

pp_data = bind_rows(pp_data_list)

emergence_model_checks = pp_data %>% 
  ggplot(aes(x = value, group = rep_id, color = is_y_label)) + 
  geom_density(aes(alpha = is_y_label)) +
  # stat_slab(fill = NA, aes(alpha = is_y_label), linewidth = 0.2) +
  facet_wrap(~model_number) +
  scale_x_log10(labels = scales::comma) +
  scale_alpha_manual(values = c(1, 0.2)) +
  scale_color_manual(values = c("black", "dodgerblue"),
                     labels = c(expression(italic(y)), 
                                expression(italic(y)[rep]))) +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank()) +
  labs(x = "Insect Emergence (mgDM/m2) [scaled to maximum]") +
  guides(alpha = "none")

ggsave(emergence_model_checks, file = "plots/emergence_model_checks.jpg",
       width = 6.5, height = 6, dpi = 500)



# model list --------------------------------------------------------------

model_list = list()

for(i in 1:length(updated_gams)){
  model_list[[i]] = tibble(model_number = i,
                           formula = as.character(updated_gams[[i]]$formula)[1])
}

model_table = bind_rows(model_list)

write_csv(model_table, file = "tables/model_table.csv")

# plot models -------------------------------------------------------------
theme_set(theme_default())

raw_dat = readRDS(file = 'data/emergence_production_with_vars.rds')
max_emergence <- max(raw_dat$mean_emergence_mgdmm2y, na.rm = T)

mod_dat = updated_gams[[1]]$data %>% as_tibble()
mod = updated_gams[[1]]

mod1_posts = tibble(precip_s = seq(min(mod_dat$precip_s),
                      max(mod_dat$precip_s),
                      length.out = 30)) %>% 
  mutate(HYBAS_ID = "new",
         pre_mm_syr = (precip_s*attributes(raw_dat$precip_s)$`scaled:scale`) + attributes(raw_dat$precip_s)$`scaled:center`) %>% 
  add_epred_draws(mod, allow_new_levels = T, re_formula = NULL) %>% 
  mutate(.epred = .epred*max_emergence)

plot_emergence_precip = mod1_posts %>% 
  ggplot(aes(x = pre_mm_syr, y = .epred/1000)) +
  stat_lineribbon(alpha = 0.3, color = "white") +
  labs(fill = "Credible Interval",
       y = bquote("Emergence (gDM" %.% m^-2%.% y^-1 ~")"),
       x = bquote("Precipitation (mm" %.% basin^-1%.% y^-1~")")) +
  geom_point(data = raw_dat, aes(y = mean_emergence_mgdmm2y/1000),
             shape = 1) +
  theme(legend.position = c(0.8, 0.8),
        text = element_text(size = 16))

ggsave(plot_emergence_precip, file = "plots/plot_emergence_precip.jpg", width = 6.5, height = 6.5,
       dpi = 400)

plot_emergence_precip_nodots = mod1_posts %>% 
  ggplot(aes(x = pre_mm_syr, y = .epred/1000)) +
  stat_lineribbon(alpha = 0.3, color = "white") +
  labs(fill = "Credible Interval",
       y = bquote("Emergence (gDM" %.% m^-2%.% y^-1 ~")"),
       x = bquote("Precipitation (mm" %.% basin^-1%.% y^-1~")")) +
  # geom_point(data = raw_dat, aes(y = mean_emergence_mgdmm2y/1000),
             # shape = 1) +
  ylim(NA, max(raw_dat$mean_emergence_mgdmm2y/1000)) +
  theme(legend.position = c(0.8, 0.8),
        text = element_text(size = 16))

ggsave(plot_emergence_precip_nodots, file = "plots/plot_emergence_precip_nodots.jpg", width = 6.5, height = 6.5,
       dpi = 400)
