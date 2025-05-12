library(tidyverse)

hybas_predictions = readRDS(file = "posteriors/hybas_predictions.rds")


hybas_predictions[[1]] %>% 
  select(HYBAS_ID, element, chem_flux_mg_year) %>% 
  rename(!!unique(hybas_predictions[[1]]$element) := chem_flux_mg_year) %>% 
  select(-element) %>% 
  left_join(hybas_predictions[[2]] %>% 
              select(HYBAS_ID, element, chem_flux_mg_year) %>% 
              rename(!!unique(hybas_predictions[[2]]$element) := chem_flux_mg_year) %>% 
              select(-element))

results_list <- list()

# Loop through each element in the list (assuming there are 6 elements)
for (i in 1:6) {
  # Extract the unique element name
  unique_element <- unique(hybas_predictions[[i]]$element)
  
  # Check if there is exactly one unique element
  if (length(unique_element) == 1) {
    # Process the current data frame
    processed_df <- hybas_predictions[[i]] %>%
      select(HYBAS_ID, element, water_ug_l_raw) %>%
      rename(!!unique_element := water_ug_l_raw) %>%
      select(-element)
    
    # Store the processed data frame in the results list
    results_list[[i]] <- processed_df
  } else {
    stop(paste("There are multiple unique elements in hybas_predictions[[", i, "]]."))
  }
}

# Combine all processed data frames using left_join
final_result <- Reduce(function(x, y) {
  left_join(x, y, by = "HYBAS_ID")
}, results_list)


final_result %>% 
  sample_n(10000) %>%
  select(-HYBAS_ID) %>% 
  pairs(log = "xy")


