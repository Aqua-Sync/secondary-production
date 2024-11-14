library(tidyverse)
library(brms)
library(tidybayes)
library(patchwork)

mod_list = readRDS(file = "models/mod_list.rds")

pp_check_list = lapply(mod_list, pp_check)

pp_check_plots = list()
for(i in 1:length(pp_check_list)){
  pp_check_plots[[i]] = pp_check_list[[i]] + scale_x_log10() +
    labs(subtitle = paste0(letters[i], ") ", 
                           str_to_sentence(mod_list[[i]]$data2$chemical_category))) +
    theme(legend.position = c(0.2, 0.9),
          text = element_text(size = 8),
          legend.text = element_text(size = 8))
}

pp_check_contaminants = pp_check_plots[[1]] + pp_check_plots[[2]] + 
  pp_check_plots[[3]] + pp_check_plots[[4]] + 
    pp_check_plots[[5]] + pp_check_plots[[6]] + 
    pp_check_plots[[7]] + pp_check_plots[[8]]+ 
  pp_check_plots[[9]] + pp_check_plots[[10]]


ggsave(pp_check_contaminants, file = "plots/pp_check_contaminants.jpg",
       width = 6.5, height = 5)
