library(tidyverse)

n_sims = 100

# emergence is modeled as emerge/max(emerge)
# Hence values greater than 1 should be rare with most values 
# less than 0.5 or so (i.e., less than half of the maximum).
tibble(int = rnorm(n_sims, -2, 1),
       shape = rexp(n_sims, 1),
       mu = exp(int),
       offset = rexp(n_sims, 4),
       # offset = 0,
       scale = (mu + offset)/shape) %>% 
  mutate(ypred = rgamma(nrow(.), shape = shape, scale = scale)) %>% 
  ggplot(aes(x = ypred)) + 
  geom_histogram() +
  # scale_x_log10() +
  geom_vline(xintercept = c(0.0001, 1))
