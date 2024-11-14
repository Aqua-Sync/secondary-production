library(tidyverse)
library(brms)
library(janitor)
library(tidybayes)
library(scales)

theme_set(theme_default())

# load prefit models
updated_gams = readRDS("models/updated_gams.rds") # stores all of the individual models below

updated_gams[[10]]$formula

pp_data_list = list()

for(i in 1:length(updated_gams)){
pp_data_list[[i]] = pp_check(updated_gams[[i]])$data %>% 
  mutate(formula = as.character(updated_gams[[i]]$formula)[1],
         model_number = i)
}

pp_data = bind_rows(pp_data_list)

pp_data %>% 
  ggplot(aes(x = value, group = rep_id, color = is_y_label)) + 
  # geom_density(aes(alpha = is_y_label)) +
  stat_slab(fill = NA, aes(alpha = is_y_label), linewidth = 0.2) +
  facet_wrap(~model_number) +
  scale_x_log10() +
  scale_alpha_manual(values = c(1, 0.2)) +
  scale_color_manual(values = c("black", "dodgerblue"),
                     labels = c(expression(italic(y)), expression(italic(y)[rep]))) +
  theme(legend.title = element_blank()) +
  guides(alpha = "none") 
