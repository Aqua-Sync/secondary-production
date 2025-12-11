library(tidyverse)
library(brms)
library(tidybayes)
library(patchwork)

mod_list = readRDS(file = "models/mod_list.rds")
pufa_mod = readRDS(file = "models/pufa_mod.rds")

contaminants_data_list = list()

for(i in 1:length(mod_list)){
  contaminants_data_list[[i]] = as_tibble(mod_list[[i]]$data) %>% 
    mutate(model = mod_list[[i]]$data2$chemical_category)
}


data_for_contaminant_models = bind_rows(contaminants_data_list)
attributes(data_for_contaminant_models$x_s) <- NULL # remove attributes so we can save a csv.

unique_citations_for_contaminant_models = bind_rows(contaminants_data_list, pufa_mod$data) %>% ungroup %>% distinct(pub_name) 

write_csv(data_for_contaminant_models, file = "data/data_for_contaminant_models.csv")

write_csv(unique_citations_for_contaminant_models, file = "data/unique_citations_for_contaminant_models.csv")


bind_rows(contaminants_data_list) %>% 
  distinct(pub_name) %>% View()

unique(pufa_mod$data$pub_name)
