library(tidyverse)
library(brms)
library(janitor)
library(tidybayes)
library(viridis)
library(scales)

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


raw_dat_list = list()

for(i in 1:length(mod_list)){
  raw_dat_list[[i]] = mod_list[[i]]$data %>% 
    mutate(chemical_category = mod_list[[i]]$data2$chemical_category)
}
raw_dat = bind_rows(raw_dat_list)

bind_rows(posts_list) %>% 
  ggplot(aes(x = x_s, y = .epred)) + 
  stat_lineribbon(alpha = 0.4, .width = 0.95) + 
  facet_wrap(~chemical_category, scales = "free") +
  # scale_y_log10() +
  guides(fill = "none") +
  geom_point(data = raw_dat, aes(y = y_s), shape = 1, size = 0.5)
