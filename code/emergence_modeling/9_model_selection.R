library(tidyverse)
library(brms)
library(janitor)
library(tidybayes)
library(scales)


# make model table --------------------------------------------------------

updated_gams = readRDS(file = "models/updated_gams.rds")  

model_formulas_list = list()

for(i in 1:length(updated_gams)){
  model_formulas_list[[i]] = tibble(formula = deparse(updated_gams[[i]]$formula$formula[[3]])) %>% 
    mutate(formula = stringr::str_c(formula, collapse = " ")) %>% 
    pull(formula) %>% 
    str_squish()
  
  model_formulas_list[[i]] = model_formulas_list[[i]][1] # remove duplicates
}

model_list = bind_rows(as_tibble(unlist(model_formulas_list)))
write_csv(model_list, file = "tables/model_list.csv")


# compare models  ---------------------
# get_mod_names = function(model){as.character(model$formula$formula[[3]][2])}

mod_names = read_csv("tables/model_list.csv") %>% pull(value)

ic_gams = lapply(updated_gams, FUN = brms::loo) 

names(ic_gams) = mod_names

elpd_diffs = loo_compare(ic_gams) %>% 
  as_tibble() %>% 
  mutate(lower = elpd_diff - 2*se_diff,
         upper = elpd_diff + 2*se_diff) %>% 
  mutate(models = mod_names)

write_csv(elpd_diffs, file = "tables/model_selection.csv")

elpd_diffs = read_csv("tables/model_selection.csv")

model_comparison = elpd_diffs  %>% 
  ggplot(aes(x = reorder(models, elpd_diff),
             y = elpd_diff,
             ymin = elpd_diff - se_diff, 
             ymax = elpd_diff + se_diff)) +
  geom_pointrange() +
  geom_hline(yintercept = 0) +
  coord_flip()

ggsave(model_comparison, file = "plots/model_comparison.jpg", width = 8, height = 8)


# check divergences ---------------------

divergence_list = NULL

for(i in 1:length(updated_gams)){
  np = nuts_params(updated_gams[[i]])
  divergence_list[[i]] = tibble(divergences = sum(subset(np, Parameter == "divergent__")$Value),
                                model = mod_names[[i]])
}

bind_rows(divergence_list)

# check rhats -------------------------

rhat_list = NULL

for(i in 1:length(updated_gams)){
  rhat_list[[i]] = brms::rhat(updated_gams[[i]]) %>% as.list() %>% 
    as_tibble() %>% pivot_longer(cols = everything()) %>% 
    mutate(model = mod_names[i])
}

bind_rows(rhat_list) %>% 
  ggplot(aes(y = model, x = value)) + 
  geom_point() +
  geom_vline(xintercept = c(1.01, 1.1))

bind_rows(rhat_list) %>% 
  # filter(!grepl("r_", name)) %>%
  # filter(!grepl("s_", name)) %>%
  # filter(!grepl("z_", name)) %>%
  filter(!grepl("lp", name)) %>% 
  mutate(parameter = str_sub(name, 1, 2)) %>% 
  ggplot(aes(y = model, x = value, color = parameter)) + 
  geom_point() +
  geom_vline(xintercept = c(1.01, 1.1))

# bind_rows(rhat_list) %>% 
#   filter(!grepl("r_", name)) %>% 
#   filter(!grepl("s_", name)) %>% 
#   filter(!grepl("z_", name)) %>% 
#   filter(!grepl("lp", name)) %>% View()

# plot models ------------------
gams_with_effects = updated_gams[-4] # removes intercept_only model

get_cond_plots = function(model){plot(conditional_effects(model), points = T, plot = F, ask = F)}

cond_plots = lapply(gams_with_effects, FUN = get_cond_plots)

library(cowplot)

cond_plots_hydrobasin = plot_grid(cond_plots[[1]]$precip_s + scale_y_log10(),
                                  cond_plots[[2]]$stream_temp_s + scale_y_log10(),
                                  cond_plots[[3]]$`precip_s:stream_temp_s`+ scale_y_log10() + guides(fill = "none", color = "none"),
                                  cond_plots[[4]]$precip_s + scale_y_log10(),
                                  cond_plots[[4]]$stream_temp_s + scale_y_log10(),
                                  cond_plots[[5]]$hft_ix_s93_s + scale_y_log10() ,
                                  cond_plots[[6]]$hft_ix_u93_s + scale_y_log10(),
                                  cond_plots[[7]]$hft_ix_s09_s + scale_y_log10(),
                                  cond_plots[[8]]$hft_ix_u09_s + scale_y_log10(),
                                  cond_plots[[9]]$ele_mt_sav_s + scale_y_log10(),
                                  cond_plots[[10]]$logdis_m3_pyr_s + scale_y_log10(),
                                  cond_plots[[11]]$for_pc_sse_s + scale_y_log10(),
                                  cond_plots[[12]]$crp_pc_sse_s + scale_y_log10(),
                                  cond_plots[[13]]$stream_temp_s + scale_y_log10(),
                                  cond_plots[[13]]$crp_pc_sse_s + scale_y_log10(),
                                  cond_plots[[14]]$stream_temp_s + scale_y_log10(),
                                  cond_plots[[14]]$crp_pc_sse_s + scale_y_log10(),
                                  cond_plots[[14]]$for_pc_sse_s + scale_y_log10())

ggsave(cond_plots_hydrobasin, file = "plots/cond_plots_hydrobasin.jpg", 
       width = 10, height = 10, dpi = 400)
