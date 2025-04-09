# Load necessary libraries
library(tidyverse)
library(haven)

# Read the Stata file
data <- read_dta("data/All AVEs.dta")

# Count zeros and ones per trading pair
counts <- data %>%
  mutate(
    temp1 = if_else(ntm == 0, 1, 0),
    temp2 = if_else(ntm == 1, 1, 0)
  ) %>%
  group_by(country, partner) %>%
  summarise(
    zeros = sum(temp1),
    ones = sum(temp2),
    tot_obs = zeros + ones,
    shareones = ones / tot_obs,
    .groups = "drop"
  )

# Keep only rows where ntm == 1
data_ntm1 <- data %>%
  filter(ntm == 1)

# Collapse to median ave by trading pair
medians <- data_ntm1 %>%
  group_by(country, partner) %>%
  summarise(
    ave = median(ave, na.rm = TRUE),
    .groups = "drop"
  )

# Merge shareones into the medians
result <- medians %>%
  left_join(counts %>% select(country, partner, shareones), by = c("country", "partner")) %>%
  mutate(weighted_aves = ave * shareones)

