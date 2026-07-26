library(tidyverse)

n_sims = 100

# emergence is modeled as emerge/mean(emerge)
# Hence values should average ~ 1 and values >> 1 should be rare with most values 
tibble(int = rnorm(n_sims, 0, 1),
       shape = rexp(n_sims, 4),
       mu = exp(int),
       offset = rlnorm(n_sims, log(4), 0.5),
       # offset = 0,
       scale = (mu + offset)/shape) %>% 
  mutate(ypred = rgamma(nrow(.), shape = shape, scale = scale)) %>% 
  ggplot(aes(x = ypred)) + 
  geom_histogram() +
  # scale_x_log10() +
  geom_vline(xintercept = 1)
