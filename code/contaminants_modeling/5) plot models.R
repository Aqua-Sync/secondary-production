library(tidyverse)
library(brms)
library(janitor)
library(tidybayes)
library(viridis)
library(scales)
theme_set(theme_default())

# mod_list = readRDS(file = "models/mod_list.rds")
# 
# posts_list = list()
# for(i in 1:length(mod_list)){
#   posts_list[[i]] = tibble(x_s = seq(min(mod_list[[i]]$data$x_s),
#                                      max(mod_list[[i]]$data$x_s),
#                                      length.out = 30)) %>%
#     mutate(pub_name = "new",
#            mean_x = mod_list[[i]]$data2$mean_x$`scaled:center`,
#            sd_x = mod_list[[i]]$data2$sd_x$`scaled:scale`,
#            max_y = mod_list[[i]]$data2$max_y,
#            chemical_category = mod_list[[i]]$data2$chemical_category) %>%
#     add_epred_draws(mod_list[[i]], allow_new_levels = T, re_formula = NULL)
# }
# 
# 
# chem_regression_posts = bind_rows(posts_list) %>%
#   mutate(chemical_category = str_to_sentence(chemical_category))
# 
# saveRDS(chem_regression_posts, file = "posteriors/chem_regression_posts.rds")
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
       x = expression("Water Concentration (ug L"^-1*" z-score)")) +
  guides(fill = "none") +
  geom_point(data = raw_contaminants, aes(y = adult_conc_ng_mg_dm), shape = 1, size = 0.5) +
  theme(axis.text = element_text(size = 7),
        strip.text = element_text(size = 7)) 

ggsave(chem_concentrations_posts, file = "plots/chem_concentrations_post.jpg",
       width = 6.5, height = 6.5)
