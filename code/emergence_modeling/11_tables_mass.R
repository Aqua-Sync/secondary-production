library(tidyverse)
library(brms)
library(tidybayes)
theme_set(theme_default())

post_mass_nutrients_pufa_global = read_csv(file = "tables/post_emergence_global.csv")
post_emergence_perm2 = read_csv(file = "tables/post_emergence_perm2.csv")
post_flux_all_peryear_hybas = readRDS(file = "posteriors/post_flux_all_peryear_hybas.rds") 

unique(post_flux_all_peryear_hybas$units)
write_csv(post_flux_all_peryear_hybas %>% rename(chemical = units), file = "posteriors/post_DM_CNP_PUFA_peryear_hybas.csv")
write_csv(post_emergence_perm2, file = "posteriors/post_DM_CNP_PUFA_perm2.csv")
write_csv(post_mass_nutrients_pufa_global, file = "posteriors/post_DM_CNP_PUFA_peryear_global.csv")



flux_per_region = post_flux_all_peryear_hybas %>% 
  group_by(region_name, units) %>% 
  add_tally() %>% 
  group_by(region_name, n, units) %>% 
  reframe(median_kgdm_peryear = median(median),
          mean_kgdm_peryear = mean(median)) %>% 
  arrange(units, median_kgdm_peryear) %>% 
  rename(n_basins = n)


write_csv(flux_per_region, file = "tables/flux_per_region.csv")


# table of models ---------------------------------------------------------

updated_gams = readRDS("models/updated_gams.rds")

test2 = paste(updated_gams[[1]]$formula$formula[2:3])

test2
bind_rows(test, test2)
formulas_list <- lapply(updated_gams, function(x) paste(as.character(x$formula$formula), collapse = " "))
bind_rows(formulas_list)



sample_perm2 = function(data_to_predict){
  data_to_predict %>% 
    distinct(precip_s, stream_temp_s, HYBAS_ID, region) %>% 
    group_by(region) %>% 
    slice_sample(prop = 0.01) %>% 
    add_epred_draws(mod, 
                    re_formula = NULL, 
                    allow_new_levels = T,
                    ndraws = 500) %>% 
    ungroup %>%
    mutate(.epred = .epred*max_emergence) %>% 
    median_qi(.epred)
}

test = lapply(1:10, function(i) sample_perm2(data_to_predict)) 

saveRDS(test, file = "posteriors/sample_perm2_n.rds")

bind_rows(test) %>% 
  rownames_to_column() %>%
  mutate(rowname = parse_number(rowname)) %>% 
  ggplot(aes(x = rowname, y = .epred, ymin = .upper ,ymax = .lower)) +
  geom_pointrange() +
  scale_y_log10()
