library(tidyverse)

# Calculate difference between initial and final value ------------------------

diff <- map(past_ILI, function(x) {
  x %>%
    spread(indicator, wili) %>%
    mutate(diff = final - initial) %>%
    filter(!(week < 40 & week > 18))})

all_seasons <- bind_rows(diff, .id = "season")


# Calculate average differences for different locations
avg_backfill <- all_seasons %>%
  group_by(region) %>%
  summarize(overall_avg = mean(abs(diff), na.rm = TRUE))

# Look at how many changes are positive or negative
pos_backfill <- all_seasons %>%
  filter(diff > 0) %>%
  group_by(region) %>%
  summarize(pos_avg = mean(abs(diff), na.rm = TRUE))

neg_backfill <- all_seasons %>%
  filter(diff < 0) %>%
  group_by(region) %>%
  summarize(neg_avg = mean(abs(diff), na.rm = TRUE))

percent_pos_neg <- all_seasons %>%
  mutate(pos = ifelse(diff > 0, 1, 0),
         pos = ifelse(is.na(diff), NA, pos)) %>%
  group_by(region) %>%
  summarize(percent_pos_diff = mean(pos, na.rm = TRUE))

# Combine and save

backfill <- full_join(avg_backfill, pos_backfill, by = "region") %>%
  full_join(neg_backfill, by = "region") %>%
  full_join(percent_pos_neg, by = "region")


save(backfill, file = "Average_backfill.Rdata")

