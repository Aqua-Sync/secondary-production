library(tidyverse)
library(brms)
library(tidybayes)
theme_set(theme_default())

post_mass_nutrients_pufa_global = read_csv(file = "tables/post_emergence_global.csv")
post_emergence_perm2 = read_csv(file = "tables/post_emergence_perm2.csv")
