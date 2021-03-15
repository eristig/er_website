### ------------------
## Results C
### ------------------

# ------------------
# Part I - Set up
# ------------------

library(tidyverse)
library(here)
library(janitor)
library(dplyr)
library(lubridate)
library(tidyr)
library(plyr)


lobster_stats_df <- lobster_tidy_data %>%
  mutate(mpa = if_else(lobster_tidy_data$site %in% c("IVEE", "NAPL"), "mpa", "non-mpa"))

lobster_stats_df <- lobster_stats_df %>% 
  uncount(count)


ggplot(lobster_stats_df, aes(sample = size_mm)) +
  geom_qq()

# ------------------------
# Lobster size, 2012 and 2018
# Summary statistics and analysis
# -------------------


mpa_2012 <- lobster_stats_df %>% 
  filter(year == "2012") %>% 
  filter(mpa == "mpa")

mpa_2018 <- lobster_stats_df %>% 
  filter(year == "2018") %>% 
  filter(mpa == "mpa")

# non-MPA
non_2012 <- lobster_stats_df %>% 
  filter(year == "2012") %>% 
  filter(mpa == "non-mpa")

non_2018 <- lobster_stats_df %>% 
  filter(year == "2018") %>% 
  filter(mpa == "non-mpa")

# -------------------

# Sample size (MPAs):
n_mpa_12 <- sum(!is.na(mpa_2012$mpa))
n_mpa_18 <- sum(!is.na(mpa_2018$mpa))

# Sample size (non-MPAs):
n_non_12 <- sum(!is.na(non_2012$mpa))
n_non_18 <- sum(!is.na(non_2018$mpa))

# -------------------

# Means (2012, MPA and Non):
mean_mpa_12 <- mean(mpa_2012$size_mm, na.rm = TRUE)
mean_non_12 <- mean(non_2012$size_mm, na.rm = TRUE)

# Means (2018, MPA and Non):
mean_mpa_18 <- mean(mpa_2018$size_mm, na.rm = TRUE)
mean_non_18 <- mean(non_2018$size_mm, na.rm = TRUE)

# -------------------

# Medians (2012, MPA and Non):
median_mpa_12 <- median(mpa_2012$size_mm, na.rm = TRUE)
median_non_12 <- median(non_2012$size_mm, na.rm = TRUE)

# Medians (2018, MPA and Non):
median_mpa_18 <- median(mpa_2018$size_mm, na.rm = TRUE)
median_non_18 <- median(non_2018$size_mm, na.rm = TRUE)

# -------------------

# Standard deviations (2012, MPA and Non):
sd_mpa_12 <- sd(mpa_2012$size_mm, na.rm = TRUE)
sd_non_12 <- sd(non_2012$size_mm, na.rm = TRUE)

# Standard deviations (2018, MPA and Non):
sd_mpa_18 <- sd(mpa_2018$size_mm, na.rm = TRUE)
sd_non_18 <- sd(non_2018$size_mm, na.rm = TRUE)

