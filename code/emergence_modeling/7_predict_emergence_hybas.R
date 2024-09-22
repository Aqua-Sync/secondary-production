library(tidyverse)
library(tidybayes)

# load data
data_to_predict_list = readRDS("data/data_to_predict.rds") %>% group_by(region) %>% group_split()
# load models
updated_gams = readRDS("models/updated_gams.rds")

max_emergence <- 26254.98

# Loop through each object in data_to_predict_list
for (i in 1:length(data_to_predict_list)) {
  tryCatch({
    # Generate predictions for each object
    hybas_predictions = data_to_predict_list[[i]] %>%
      select(HYBAS_ID, precip_s, water_km2, pre_mm_syr) %>%
      mutate(author_year = "new") %>%
      add_epred_draws(updated_gams[[1]], allow_new_levels = TRUE, re_formula = NULL, ndraws = 1000) %>%
      mutate(mgdmm2y = .epred * max_emergence,
             kgdmm2y = mgdmm2y / 1e6,
             kgdmkm2y = kgdmm2y * 1e6) %>%
      mutate(kgdmhybasyr = kgdmkm2y * water_km2) %>%
      ungroup() %>%
      select(HYBAS_ID, .draw, kgdmhybasyr)
    
    # Save the hybas_predictions with the index appended to the filename
    saveRDS(hybas_predictions, file = paste0("posteriors/hybas_predictionstest", i, ".rds"))
    
    # Summarize the predictions
    flux_predictions = hybas_predictions %>%
      group_by(HYBAS_ID) %>%
      reframe(mean = mean(kgdmhybasyr),
              sd = sd(kgdmhybasyr),
              median = median(kgdmhybasyr),
              lower95 = quantile(kgdmhybasyr, probs = 0.025),
              upper95 = quantile(kgdmhybasyr, probs = 0.975),
              lower75 = quantile(kgdmhybasyr, probs = 0.125),
              upper75 = quantile(kgdmhybasyr, probs = 0.875))
    
    # Save the flux_predictions with the index appended to the filename
    saveRDS(flux_predictions, file = paste0("posteriors/flux_predictionstest", i, ".rds"))
    
  }, error = function(e) {
    # Print an error message and continue to the next item
    message(paste("Error in processing item", i, ":", e$message))
  })
}
