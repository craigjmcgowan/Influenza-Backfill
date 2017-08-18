library(tidyverse)

# Calculate difference between initial and final value ------------------------

temp <- past_ILI[[1]] %>%
  spread(indicator, wili) %>%
  mutate(diff = final - initial)
  
