library(tidyverse)
library(brms)
library(tidybayes)
library(patchwork)

mod_list = readRDS(file = "models/mod_list.rds")

pp_check_list = lapply(mod_list, pp_check)

pp_check_plots = list()
for(i in 1:length(pp_check_list)){
  pp_check_plots[[i]] = pp_check_list[[i]] + scale_x_log10() +
    labs(subtitle = paste0(letters[i], ") ", 
                           str_to_sentence(mod_list[[i]]$data2$chemical_category))) +
    theme(legend.position = c(0.2, 0.9),
          text = element_text(size = 8),
          legend.text = element_text(size = 8))
}

pp_check_contaminants = pp_check_plots[[1]] + pp_check_plots[[2]] + 
  pp_check_plots[[3]] + pp_check_plots[[4]] + 
    pp_check_plots[[5]] + pp_check_plots[[6]] + 
    pp_check_plots[[7]] + pp_check_plots[[8]]+ 
  pp_check_plots[[9]] + pp_check_plots[[10]]


ggsave(pp_check_contaminants, file = "plots/pp_check_contaminants.jpg",
       width = 6.5, height = 5)


# pufa models -------------------------------------------------------------
emergence_production_with_vars = readRDS("data/emergence_production_with_vars.rds")
pufa_data_short = readRDS("data/pufa_data.rds") %>% 
  filter(chemical == "epa + dha") %>%
  filter(!is.na(stream_temp)) %>% 
  mutate(HYBAS_ID = as.character(HYBAS_ID)) %>% 
  mutate(stream_temp_s = (stream_temp - attributes(emergence_production_with_vars$stream_temp_s)[[2]])/attributes(emergence_production_with_vars$stream_temp_s)[[3]],
         log10_stream_temp = log10(stream_temp),
         log10_stream_temp_s = scale(log10_stream_temp))

pufa_mod_taxon_epadha = readRDS("models/pufa_mod_taxon_epadha.rds")

pp_check(pufa_mod_taxon_epadha)

max_y = unique(pufa_data_short$max_y)

post_pufa_concentration = pufa_mod_taxon_epadha$data %>%
  distinct(order, pub_name, HYBAS_ID) %>% 
  add_epred_draws(pufa_mod_taxon_epadha, re_formula = NULL) %>% 
  group_by(order, .draw) %>% 
  reframe(.epred = mean(.epred)) %>% 
  mutate(adult_conc_mgpergram_dm = (.epred*max_y)/1000)

post_pufa_concentration %>% 
  ggplot(aes(y = order, x = adult_conc_mgpergram_dm)) +
  stat_halfeye() +
  geom_point(data = pufa_data_short, aes(x = adult_conc_ng_mg_dm/1000))

pufa_taxa_table = post_pufa_concentration %>% 
  group_by(order) %>% 
  median_qi(adult_conc_mgpergram_dm)

write_csv(pufa_taxa_table, file = "tables/pufa_taxa_table.csv")
