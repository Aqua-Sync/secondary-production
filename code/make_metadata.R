library(tidyverse)
library(janitor)

# 1) read in tables and clear attributes
data_to_predict = as_tibble(
  lapply(readRDS("data/data_to_predict.rds"), function(x) {
    attributes(x) <- NULL
    x
  }),
  check.names = FALSE) %>% 
  select(-gad_id_smj, -BAS_ID, -BA_km2, -mnRSSA_pc, -sdRSSA_pc)

emerge =  as_tibble(
  lapply(readRDS("data/emergence_production_with_vars.rds"), function(x) {
    attributes(x) <- NULL
    x
  }),
  check.names = FALSE) %>% 
  select(mean_emergence_mgdmm2y, mean_emergence, sd_emergence,
         emerge_1, 
         ele_mt_sav_s,
         for_pc_sse_s,
         hft_ix_s93_s,
         hft_ix_u93_s,
         hft_ix_s09_s,
         hft_ix_u09_s,
         crp_pc_sse_s,
         stream_temp_s,
         logdis_m3_pyr_s,
         precip_s)

cont = as_tibble(
  lapply(readRDS("data/contaminants.rds"), function(x) {
    attributes(x) <- NULL
    x
  }),
  check.names = FALSE) %>% 
  select(chemical_category, pub_name, adult_conc_ng_mg_dm, water_conc_ug_l)

water = as_tibble(
  lapply(readRDS("data/modeled_water.rds"), function(x) {
    attributes(x) <- NULL
    x
  }),
  check.names = FALSE)


write_csv(data_to_predict, file = "data/data_release/data_to_predict.csv")
write_csv(emerge, file = "data/data_release/emergence_production_with_vars.csv")
write_csv(cont, file = "data/data_release/contaminants.csv")
write_csv(water, file = "data/data_release/modeled_water.csv")

dat_list = list(emerge ,
                cont ,
                water,
                data_to_predict)

name_list = c("emergence_production_with_vars", "contaminants", "modeled_water", "data_to_predict")

summary_list = list()

for(i in 1:length(dat_list)){
  summary_list[[i]] = tibble(
    table = name_list[[i]],
    column = names(dat_list[[i]]),
    class = map_chr(dat_list[[i]],
                    ~ paste(class(.x), collapse = ", ")),
    summary = map_chr(dat_list[[i]], \(x) {
      
      if (is.numeric(x)) {
        paste0(
          round(min(x, na.rm = TRUE), 1),
          " to ",
          round(max(x, na.rm = TRUE), 1)
        )
        
      } else if (is.factor(x) || is.character(x)) {
        paste(unique(na.omit(x)), collapse = "; ")
        
      } else {
        NA_character_
      }
    })
  )
}

data_metadata = bind_rows(summary_list)
write_csv(data_metadata, file = "data/data_metadata.csv")
