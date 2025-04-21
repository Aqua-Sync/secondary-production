get_contaminant_preds = function(model){
  
  chem = cas_names %>% 
    filter(chemical_category == model$data2$chemical_category)
  
  water_predictions = modeled_water %>% 
    filter(cas == chem$cas) %>% 
    mutate(chemical = chem$chemical,
           HYBAS_ID = bit64::as.character.integer64(HYBAS_L12),
           mean.conc.year_units = "log10_ug_l")
  
  # get parameters of adult_conc ~ a + b*water_conc
  mod = Filter(function(m) m$data2$chemical == model$data2$chemical_category, mod_list)[[1]]
  
  int_slope_post = as_draws_df(mod) %>%
    mutate(b_Intercept = b_Intercept + rnorm(nrow(.), 0, 1.18)) %>%
    reframe(int = median(b_Intercept),
            int_lower95 = quantile(b_Intercept, probs = 0.025),
            int_upper95 = quantile(b_Intercept, probs = 0.975),
            int_lower50 = quantile(b_Intercept, probs = 0.25),
            int_upper50 = quantile(b_Intercept, probs = 0.75),
            slope = median(b_x_s),
            slope_lower95 = quantile(b_x_s, probs = 0.025),
            slope_upper95 = quantile(b_x_s, probs = 0.975),
            slope_lower50 = quantile(b_x_s, probs = 0.25),
            slope_upper50 = quantile(b_x_s, probs = 0.75))
  
  # combine
 flux_predictions_all %>% 
    # slice(1:20) %>%
    left_join(water_predictions) %>% 
   mutate(
     cas = fct_na_value_to_level(cas, as.character(first(na.omit(cas))))) %>% 
    # filter(!is.na(log_water_ug_l)) %>%
    mutate(water_ug_l_raw = 10^(mean.conc.year*mean.det.year),
           log_water_ug_l = log(water_ug_l_raw),
           mean_x = mod$data2$mean_x$`scaled:center`,
           sd_x = mod$data2$sd_x$`scaled:scale`,
           max_adult_conc = mod$data2$max_y) %>% 
    mutate(x_s = (log_water_ug_l - mean_x)/sd_x) %>% 
    mutate(int = int_slope_post$int,
           slope = int_slope_post$slope,
           int_lower95 = int_slope_post$int_lower95,
           int_upper95 = int_slope_post$int_upper95,
           slope_lower95 = int_slope_post$slope_lower95,
           slope_upper95 = int_slope_post$slope_upper95,
           int_lower50 = int_slope_post$int_lower50,
           int_upper50 = int_slope_post$int_upper50,
           slope_lower50 = int_slope_post$slope_lower50,
           slope_upper50 = int_slope_post$slope_upper50) %>% 
    mutate(y_s = exp(int + slope*x_s),
           y_s_lower95 = exp(int_lower95 + slope_lower95*x_s),
           y_s_upper95 = exp(int_upper95 + slope_upper95*x_s),
           y_s_lower50 = exp(int_lower50 + slope_lower50*x_s),
           y_s_upper50 = exp(int_upper50 + slope_upper50*x_s)) %>% 
    mutate(y_mg_kg = y_s*max_adult_conc,
           y_mg_kg_lower95 = y_s_lower95*max_adult_conc,
           y_mg_kg_upper95 = y_s_upper95*max_adult_conc,
           y_mg_kg_lower50 = y_s_lower50*max_adult_conc,
           y_mg_kg_upper50 = y_s_upper50*max_adult_conc,
           element = mod$data2$chemical_category,
           bug_conc_units = "ng_mg_dm") %>% 
    mutate(chem_flux_mg_year = y_mg_kg*median,
           chem_flux_mg_year_lower95 = y_mg_kg_lower95*median,
           chem_flux_mg_year_upper95 = y_mg_kg_upper95*median,
           chem_flux_mg_year_lower50 = y_mg_kg_lower50*median,
           chem_flux_mg_year_upper50 = y_mg_kg_upper50*median) %>%
   mutate_at(vars(starts_with("chem_flux")), ~replace(., is.na(.), 0)) %>% 
   left_join(cas_names %>% select(cas, chemical)) %>% 
   mutate(
     chemical = fct_na_value_to_level(chemical, as.character(first(na.omit(chemical))))) 
}



