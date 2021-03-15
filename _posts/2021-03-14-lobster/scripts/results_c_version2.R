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
library(DT)
library(kableExtra)
library(flextable)

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

# ------------------
# Analysis
# ------------------

#### Are observations paired or unpaired? 

# 1.
# **Null hypothesis:** The mean lobster size is the same in MPA and non-MPA sites for 2012 observations.

# **Alternative hypothesis:** The mean lobster size is NOT the same in MPA and non-MPA sites for 2012 observations.

# 2.
# **Null hypothesis:** The mean lobster size is the same in MPA and non-MPA sites for 2018 observations.

# **Alternative hypothesis:**The mean lobster size is NOT the same in MPA and non-MPA sites for 2018 observations.

These observations are reported for each *reef*. Does it make sense to compare the non-MPA lobster size and MPA lobster size observations across different reefs? 
  
  lobster size -- mpa vs non-mpa
  
  **No.** It makes sense to recognize that when we compare lobster size, we should be comparing the immediate and post-flushing lead concentration differences at each house (e.g., each observation in the 'immediate' sample is associated with one and only one observation in the '2 min flushing' sample). 

# When comparing MPA and non-MPA sites, our observations were grouped. We are not comparing transects at each reef, because lobsters move. The transects are a relative measurement, and don't tell us anything about lobster distribution across reefs.

# We're not comparing individual lobsters over time. So, our t-test will be unpaired.

size_test_2012 <- t.test(mpa_2012$size_mm, non_2012$size_mm, paired = FALSE)

size_test_2018 <- t.test(mpa_2018$size_mm, non_2018$size_mm, paired = FALSE)

# -----------------
# Effect size
# -----------------

# 2012 samples
d_2012 <- effsize::cohen.d(non_2012$size_mm, mpa_2012$size_mm, na.rm = TRUE)
d_2012

# We have a moderate effect size for 2012 (d = 0.6).

# 2018 samples
d_2018 <- effsize::cohen.d(non_2018$size_mm, mpa_2018$size_mm, na.rm = TRUE)
d_2018

# We have a small effect size for 2018 (d = 0.3).

# THIS MEANS:
# The size differences between the lobsters was statistically significant in 2012 and 2018 (p < 0.05). But, in 2012, the difference between mean lobster size at MPA and non-MPA reefs was larger (-7.5 mm) than in 2018 (4.0 mm). 

# We also looked at the confidence intervals.

difference_2012 <- mean_mpa_12 - mean_non_12

difference_2018 <- mean_mpa_18 - mean_non_18


# ---------------
# Mean size at MPA 2012 - 2018

size_test_mpa <- t.test(mpa_2012$size_mm, mpa_2018$size_mm, paired = FALSE)
size_test_mpa

# Mean size at non-MPA 2012 - 2018

size_test_non <- t.test(non_2012$size_mm, non_2018$size_mm, paired = FALSE)
size_test_non

# p = .18 ( p > 0.05)

# MPA samples
d_mpa <- effsize::cohen.d(mpa_2012$size_mm, mpa_2018$size_mm, na.rm = TRUE)
d_mpa

# We have a large effect size for MPA lobster size in 2012 vs. 2018 (d > 0.8).

# non-MPA samples
d_non <- effsize::cohen.d(non_2012$size_mm, non_2018$size_mm, na.rm = TRUE)
d_non

# We have a negligible effect size for non-MPA lobster size between 2012 and 2018 (d < 0.2).

# ------------------
# 

table_lobster <- lobster_stats_df %>% 
  filter(year %in% c(2012,2018))


lobster_kable <- table_lobster %>% 
  group_by(mpa, year) %>% 
  summarize(mean = mean(size_mm, na.rm = TRUE)) %>% 
  select(year, mpa, site, size_mm, mean)


,
            sd = sd(size_mm, na.rm = TRUE),
            sample_size = n())
  
  
  mutate(
    group_by(year, mpa),
    mean_size = mean(size_mm, na.rm = TRUE))
  
