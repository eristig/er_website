### ------------------
## Results B
## Lobster size, 2012 and 2018
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


lobster_tidy_data <- read_csv(here::here("clean_data", "lobster_clean.csv"))

# ------------------
# Part II - Data wrangling and exploration
# ------------------

# This is not quite in tidy format. SOme rows store multiple observations. I need to separate those, so that every row contains a distinct observation. I'll also take this opportunity to narrow down to my variables of interest.

lobster_tidier_data <- lobster_tidy_data %>%
  filter(year == "2012"|year == "2018") %>% 
  select(year, site, size_mm, count) %>% 
  uncount(count)

# Okay. Now, I want to find the average lobster size for each site by year.

size_averages <- lobster_tidier_data %>% 
  group_by(site, year) %>% 
  summarize(avg_size = mean(size_mm))

# ------------------
# Part III - Visualization
# ------------------  

# At first, I struggled greatly. This is what I tried:

lobster_2012_df <- lobster_tidier_data  %>% 
  filter(year == "2012")

lobster_2018_df <- lobster_tidier_data  %>% 
  filter(year == "2018")

ggplot() +
  geom_boxplot(data = lobster_2012_df,
               stat = "boxplot",
               aes(x = site, y = size_mm),
               fill = "#c37131",
               position_nudge(x = 0, y = 0)) +
  geom_boxplot(data = lobster_2018_df,
               stat = "boxplot",
               aes(x = site, y = size_mm),
               fill = "#206a78",
               position_nudge(x = 2, y = 0)) +
  facet_wrap(~site)

# This does not look great. Turns out you can use factor() within the aes(fill = ) argument. NOW we're getting places

lobster_plot_b <- 
  ggplot() +
  geom_boxplot(data = lobster_tidier_data,
               aes(x = site,
                   y = size_mm,
                   fill = factor(year)),
               stat = "boxplot",
               position= "dodge")

lobster_plot_b

# Now, I'll distinguish my groups more nicely. I'm going to add a geom_rect() argument to set the MPA sites apart from the non-MPA sites.

lobster_plot_b

ggplot() +
  theme(panel.grid.major  = element_line(color = "transparent"),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "#978B7D", fill = NA)
        ) +
  geom_boxplot(
    data = lobster_tidier_data,
               aes(x = site,
                   y = size_mm,
                   fill = factor(year)),
               stat = "boxplot",
               position= "dodge",
               outlier.colour = "#978B7D",
               color = "#978B7D") +
  scale_fill_manual(
    values = c("#6EC5B8", "#DC5C05")) +
  scale_x_discrete(
    limits = c("IVEE", "NAPL", "MOHK", "CARP", "AQUE"))
  
# Ok, now to add a title and improve the axis labels and legend. 



  