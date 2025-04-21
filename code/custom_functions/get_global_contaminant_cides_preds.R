sample_gamma = function(mean, sd, n = 1){
  alpha = (mean / sd)^2
  beta = mean / (sd^2)
  rgamma(n, shape = alpha, rate = beta)
}

sample_gamma = Vectorize(sample_gamma)

get_global_contaminant_cides_preds <- function(mod, num_iterations = 300) {
  # Filter chemicals based on the model's chemical category

  # Prepare water predictions
  water_predictions <- modeled_water %>% 
    filter(chemical_category == mod$data2$chemical_category) %>% 
    mutate(chemical = unique(mod$data2$chemical_category),
           HYBAS_ID = bit64::as.character.integer64(HYBAS_L12),
           mean.conc.year.units = "log10_ug_l")
  
  # Pre-calculate values that are constant for all rows
  mean_x <- mod$data2$mean_x$`scaled:center`
  sd_x <- mod$data2$sd_x$`scaled:scale`
  max_adult_conc <- mod$data2$max_y
  
  post_samples = as_draws_df(mod)
  
  # Initialize list to store y_mg_kg results
  y_mg_kg <- vector("list", num_iterations)
  
  # Calculate y_mg_kg for each iteration
  for (i in 1:num_iterations) {
    y_mg_kg[[i]] <- water_predictions %>% 
      mutate(
        water_ug_l_raw = 10^(mean.conc.year * mean.det.year),
        x_s = (log(water_ug_l_raw) - mean_x) / sd_x,
        y_mg_kg = max_adult_conc * exp(post_samples$b_Intercept[i] + post_samples$b_x_s[i] * x_s)
      ) %>% 
      select(HYBAS_L12, y_mg_kg)
  }
  
  # Prepare kg_flux data
  kg_flux <- flux_predictions_all %>% 
    select(HYBAS_L12, mean, sd) %>% 
    ungroup() %>% 
    mutate(kg_flux = sample_gamma(n = 1, mean = mean, sd = sd))
  
  # Initialize list to store sum_mg results
  sum_mg <- vector("list", length(y_mg_kg))
  
  # Calculate sum_mg for each y_mg_kg
  for (i in 1:length(y_mg_kg)) {
    sum_mg[[i]] <- y_mg_kg[[i]] %>% 
      left_join(kg_flux %>% filter(mean > 0), by = "HYBAS_L12") %>% 
      filter(!is.na(mean)) %>% 
      mutate(chem_flux_mg_year = kg_flux * y_mg_kg) %>% 
      ungroup() %>% 
      summarise(sum = sum(chem_flux_mg_year, na.rm = TRUE), .draw = i)
  }
  
  # Combine results and calculate global flux
  result <- bind_rows(sum_mg) %>% 
    mutate(global_flux_MT_peryr = sum / 1e9) %>% 
    select(-sum) %>% 
    mutate(chemical = mod$data2$chemical_category)
  
  return(result)
}
