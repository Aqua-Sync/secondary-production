# Code will not run. This was a playground to explore the poor predictions of the precip only model. the end result shows that using 
# temp and precip probably predicts emergence better outside of the training data.


# get total mass per region
bind_rows(post_total_region_summary) %>% 
  filter(region_name != "Greenland") %>%
  ggplot(aes(x = sum_kgdmyr + 1, y = region_name)) + 
  # geom_density(scale = 1)+
  ggridges::geom_density_ridges() +
  scale_x_log10()

hybas_area %>% 
  left_join(hybas_regions) %>% 
  group_by(region_name) %>% 
  reframe(total_water = sum(area.redist))

bind_rows(data_to_predict_list) %>% 
  left_join(hybas_regions) %>% 
  group_by(region_name) %>% 
  median_qi(pre_mm_syr)

bind_rows(data_to_predict_list) %>% 
  left_join(hybas_regions) %>% 
  ggplot(aes(x = pre_mm_syr, y = region_name)) + 
  ggridges::geom_density_ridges()
  
bind_rows(data_to_predict_list) %>% 
  select(HYBAS_ID, water_km2, old_water_km2) %>% 
  left_join(bind_rows(hybas_area)) %>% 
  left_join(hybas_regions) %>% 
  sample_n(100000) %>% 
  ggplot(aes(x = water_km2, y = area.redist),
         shape = ".") +
  geom_point() +
  scale_x_log10() + 
  scale_y_log10() +
  geom_abline() +
  facet_wrap(~region_name)

bind_rows(data_to_predict_list) %>% 
  select(HYBAS_ID, water_km2) %>% 
  left_join(bind_rows(hybas_area)) %>% 
  left_join(hybas_regions) %>% 
  pivot_longer(cols = c(-HYBAS_ID, -region_name)) %>% 
  group_by(region_name, name) %>% 
  reframe(sum_water = sum(value))

sa_flux = bind_rows(data_to_predict_list) %>% 
  select(HYBAS_ID, water_km2, pre_mm_syr, precip_s) %>% 
  left_join(bind_rows(hybas_area)) %>% 
  left_join(hybas_regions) %>% 
  filter(region_name == "South America") %>% 
  sample_n(1000) %>%  
  mutate(precip_s = -1.2712) %>% 
  add_epred_draws(updated_gams[[1]], re_formula = NULL, allow_new_levels = T, ndraws = 500)

sib_flux = bind_rows(data_to_predict_list) %>% 
  select(HYBAS_ID, water_km2, pre_mm_syr, precip_s) %>% 
  left_join(bind_rows(hybas_area)) %>% 
  left_join(hybas_regions) %>% 
  filter(region_name == "Siberia") %>% 
  sample_n(1000) %>%
  add_epred_draws(updated_gams[[1]], re_formula = NULL, allow_new_levels = T, ndraws = 500)

median_qi(sa_flux$.epred*max_emergence)
median_qi(sib_flux$.epred*max_emergence)
median_qi(sa_flux$pre_mm_syr)
median_qi(sib_flux$pre_mm_syr)
median_qi(sa_flux$precip_s)
median_qi(sib_flux$precip_s)

plot(conditional_effects(updated_gams[[1]]), points = T)

updated_gams[[1]]$data %>% 
  left_join(hybas_regions) %>% 
  ggplot(aes(x = region_name, y = emerge_1)) + 
  geom_point() +
  geom_hline(yintercept = exp(-5))


dat_with_region = updated_gams[[1]]$data %>% 
  left_join(hybas_regions) 

test = update(updated_gams[[1]], newdata = dat_with_region, formula = . ~ s(precip_s) + (1 | HYBAS_ID) + (1|region_name))



134180*2281
163936*1483



data_to_predict_list[[6]] %>%
  # slice(1:100) %>%
  select(HYBAS_ID, precip_s) %>%
  mutate(author_year = "new") %>%
  left_join(hybas_area) %>%
  left_join(hybas_regions) %>%
  add_epred_draws(updated_gams[[1]], allow_new_levels = TRUE, re_formula = NULL, ndraws = 500) %>%
  mutate(.epred = .epred*max_emergence)%>%
  mutate(kgdmhybasyr = (.epred*(area.redist*1e6))/1e6) %>% # convert water area to m2. Multiply by mg/m2. It yields mg/hybas. Then divide by 1e6 to get kg/hybas
  group_by(.draw, region_name) %>%
  reframe(sum_kgdmyr = sum(kgdmhybasyr))



data_to_predict_list[[6]] %>%
  # slice(1:100) %>%
  select(HYBAS_ID, precip_s) %>%
  mutate(author_year = "new") %>%
  left_join(hybas_area) %>%
  left_join(hybas_regions)


max(updated_gams[[1]]$data$precip_s)
max(data_to_predict_list[[6]]$precip_s)
max(data_to_predict_list[[3]]$precip_s)

precip_sa = data_to_predict_list[[6]] %>%
  # slice(1:100) %>%
  select(HYBAS_ID, precip_s) %>%
  mutate(author_year = "new") %>%
  left_join(hybas_area) %>%
  left_join(hybas_regions)

precip_sib = data_to_predict_list[[3]] %>%
  # slice(1:100) %>%
  select(HYBAS_ID, precip_s) %>%
  mutate(author_year = "new") %>%
  left_join(hybas_area) %>%
  left_join(hybas_regions)

precip_raw = updated_gams[[1]]$data %>% 
  mutate(region_name = "raw data")

bind_rows(precip_sa, precip_sib, precip_raw) %>% 
  ggplot(aes(x = region_name, y = precip_s)) + 
  geom_jitter(width = 0.2, shape = ".")


temp_sa = data_to_predict_list[[6]] %>% 
  select(HYBAS_ID, precip_s, tmp_dc_syr) %>% 
  mutate(stream_temp = tmp_dc_syr/10,
         stream_temp_s = (stream_temp - mean_temp)/sd_temp) %>%
  mutate(author_year = "new") %>%
  left_join(hybas_area) %>%
  left_join(hybas_regions)

temp_sib = data_to_predict_list[[3]] %>% 
  select(HYBAS_ID, precip_s, tmp_dc_syr) %>% 
  mutate(stream_temp = tmp_dc_syr/10,
         stream_temp_s = (stream_temp - mean_temp)/sd_temp) %>%
  mutate(author_year = "new") %>%
  left_join(hybas_area) %>%
  left_join(hybas_regions)

temp_raw = updated_gams[[3]]$data %>% 
  select(HYBAS_ID, precip_s, stream_temp_s) %>% 
  mutate(author_year = "new",
         region_name = "raw_data") %>%
  left_join(hybas_area) %>%
  left_join(hybas_regions)

bind_rows(temp_sa, temp_sib, temp_raw) %>% 
  ggplot(aes(x = region_name, y = stream_temp_s)) + 
  geom_jitter(width = 0.2, shape = ".")


region_preds = bind_rows(data_to_predict_list)  %>% 
  select(HYBAS_ID, precip_s, tmp_dc_syr) %>% 
  mutate(stream_temp = tmp_dc_syr/10,
         stream_temp_s = (stream_temp - mean_temp)/sd_temp) %>%
  mutate(author_year = "new") %>%
  left_join(hybas_area) %>%
  left_join(hybas_regions) %>% 
  select(precip_s, stream_temp_s, region_name) %>% 
  bind_rows(temp_raw)

raw_not = region_preds %>% ungroup %>% distinct(region_name) %>% 
  mutate(raw_not = case_when(region_name == "raw_data" ~ "Raw Data",
                             TRUE ~ "Modeled Data"))

region_preds %>% 
  left_join(raw_not) %>% 
  group_by(region_name) %>% 
  sample_n(3000, replace = T) %>% 
  ggplot(aes(x = reorder(region_name, stream_temp_s), 
             y = stream_temp_s, 
             color = raw_not)) +
  geom_jitter(width = 0.2, shape = ".")

region_preds %>% 
  left_join(raw_not) %>% 
  group_by(region_name) %>% 
  # sample_n(3000, replace = T) %>% 
  ggplot(aes(x = reorder(region_name, stream_temp_s), 
             y = precip_s, 
             color = raw_not)) +
  geom_jitter(width = 0.2, shape = ".")




newdat = tibble(precip_s = seq(min(data_to_predict_list[[3]]$precip_s),
                      16, length.out = 50)) 
  
newdat %>% 
  add_epred_draws(updated_gams[[1]], allow_new_levels = T, re_formula = NULL) %>% 
  ggplot(aes(x = precip_s, y = .epred)) + 
  stat_lineribbon() +
  scale_y_log10()

mean_logpre = attributes(emergence_production_with_vars$log10_precip_s)$`scaled:center`
sd_logpre = attributes(emergence_production_with_vars$log10_precip_s)$`scaled:scale`

sa_preds = data_to_predict_list[[6]] %>% 
  select(HYBAS_ID, precip_s, pre_mm_syr, tmp_dc_syr) %>% 
  mutate(stream_temp = tmp_dc_syr/10,
         stream_temp_s = (stream_temp - mean_temp)/sd_temp) %>% 
  mutate(log10_precip = log10(pre_mm_syr + 0.00001),
         log10_precip_s = (log10_precip - mean_logpre)/sd_logpre) %>%
  select(HYBAS_ID, precip_s, stream_temp_s, log10_precip_s) %>%
  mutate(author_year = "new") %>%
  left_join(hybas_area) %>%
  left_join(hybas_regions) %>%
  add_epred_draws(fit_gam_log10precip, allow_new_levels = TRUE, re_formula = NULL, ndraws = 50) %>%
  mutate(.epred = .epred*max_emergence) %>%
  mutate(kgdmhybasyr = (.epred*(area.redist*1e6))/1e6) 


sa_summary = sa_preds %>% 
  filter(.draw <= 50) %>% 
  group_by(precip_s, HYBAS_ID, area.redist) %>% 
  reframe(.epred = median(.epred))


sa_summary %>% 
  ggplot(aes(x = precip_s, y = .epred)) + 
  geom_point(shape = ".")


sib_preds = data_to_predict_list[[3]] %>% 
  select(HYBAS_ID, precip_s, pre_mm_syr, tmp_dc_syr) %>% 
  mutate(stream_temp = tmp_dc_syr/10,
         stream_temp_s = (stream_temp - mean_temp)/sd_temp) %>% 
  mutate(log10_precip = log10(pre_mm_syr + 0.00001),
         log10_precip_s = (log10_precip - mean_logpre)/sd_logpre) %>%
  select(HYBAS_ID, precip_s, stream_temp_s, log10_precip_s) %>%
  mutate(author_year = "new") %>%
  left_join(hybas_area) %>%
  left_join(hybas_regions) %>%
  add_epred_draws(fit_gam_log10precip, allow_new_levels = TRUE, re_formula = NULL, ndraws = 50) %>%
  mutate(.epred = .epred*max_emergence)%>%
  mutate(kgdmhybasyr = (.epred*(area.redist*1e6))/1e6) 

sib_summary = sib_preds %>% 
  filter(.draw <= 50) %>% 
  group_by(precip_s, HYBAS_ID, area.redist) %>% 
  reframe(.epred = median(.epred))

sib_summary %>% 
  ggplot(aes(x = precip_s, y = .epred)) + 
  geom_point(shape = ".") +
  xlim(NA, 18)

bind_rows(sib_summary %>% mutate(region_name = "a) Siberia"),
          sa_summary %>% mutate(region_name = "b) South America")) %>% 
  ggplot(aes(x = log10_precip_s, y = .epred, color = region_name)) + 
  geom_point(shape = ".") +
  geom_point(data = updated_gams[[1]]$data, aes(y = emerge_1*max_emergence), 
             color = "black",
             shape = 1)


sum(sib_summary$.epred*sib_summary$area.redist)
sum(sa_summary$.epred*sa_summary$area.redist)

median(sib_summary$.epred)
median(sa_summary$.epred)



precip_preds = data_to_predict_list[[6]] %>%
  # slice(1:100) %>%
  select(HYBAS_ID, precip_s) %>%
  mutate(author_year = "new") %>%
  left_join(hybas_area) %>%
  left_join(hybas_regions) %>% 
  filter(precip_s >= 10) %>% 
  mutate(model = "precip_s") %>% 
  add_epred_draws(updated_gams[[1]], allow_new_levels = T)

mean_temp = attributes(emergence_production_with_vars$stream_temp_s)$`scaled:center`
sd_temp = attributes(emergence_production_with_vars$stream_temp_s)$`scaled:scale`

temp_preds = data_to_predict_list[[6]] %>% 
  select(HYBAS_ID, precip_s, tmp_dc_syr) %>% 
  mutate(stream_temp = tmp_dc_syr/10,
         stream_temp_s = (stream_temp - mean_temp)/sd_temp) %>% 
  filter(precip_s >= 10) %>% 
  mutate(model = "stream_temp_s") %>% 
  add_epred_draws(updated_gams[[2]], allow_new_levels = T)

precip_temp_preds = data_to_predict_list[[6]] %>% 
  select(HYBAS_ID, precip_s, tmp_dc_syr) %>% 
  mutate(stream_temp = tmp_dc_syr/10,
         stream_temp_s = (stream_temp - mean_temp)/sd_temp) %>% 
  filter(precip_s >= 10) %>% 
  mutate(model = "precip*stream_temp") %>% 
  add_epred_draws(updated_gams[[3]], allow_new_levels = T)

bind_rows(precip_preds, temp_preds, precip_temp_preds) %>% 
  group_by(precip_s, model) %>% 
  reframe(.epred = median(.epred)) %>% 
  ggplot(aes(x = precip_s, y = .epred, color = model)) +
  geom_point(shape = ".")

pp_check(updated_gams[[1]]) + scale_x_log10()

updated_gams[[3]]

conditional_effects(updated_gams[[3]])





fit_gam_log10precip = update(updated_gams[[1]], formula = . ~ s(log10_precip_s) + (1 | HYBAS_ID), newdata = emergence_production_with_vars)



plot(conditional_effects(fit_gam_log10precip), points = T)


pp_check(fit_gam_log10precip) + scale_x_log10







data_to_predict = readRDS("data/data_to_predict.rds") %>% 
  mutate(stream_temp = tmp_dc_syr/10,
         stream_temp_s = (stream_temp - mean_temp)/sd_temp) 


