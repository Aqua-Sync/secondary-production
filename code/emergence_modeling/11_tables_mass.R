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


