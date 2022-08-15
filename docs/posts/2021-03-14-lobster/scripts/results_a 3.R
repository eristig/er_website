# Results A
# Visually explore changes in annual lobster abundance (counts) by site
# After grouping observations to find lobster counts for each year, create a finalized data visualization (no modeling/stats needed) showing changes in annual lobster abundance at the five sites over time. You should decide how to best present the data. Make your data visualization correct, clear, responsible, and professional. Details matter (e.g. is it easier to read a legend or label lines directly? How can I designate between MPA and non-MPA sites? And many more decisions!). Add a figure caption below the graph.


#_____________________________
# First I created the clean data file

library(tidyverse)
library(janitor)
library(here)
library(lubridate)
library(ggplot2)



lobster_clean <- read_csv("lobster_abundance_sbc_lter.csv", na = "-99999") %>% 
    clean_names() %>% 
    mutate(date = ymd(date)) %>% 
  mutate(day = day(date)) %>% 
  select(-date)
    
    write_csv(lobster_clean, "lobster_clean.csv")
    
#____________________________
# Going to create a summary table that lists total lobsters per year per site   

lobster_year <- lobster_clean %>% 
  # Create new column for total lobsters (bodies & antennas)
  mutate(total_lobsters = count + num_ao) %>% 
  # Create summary table
  group_by(year, site) %>% 
  summarize(total_lobsters = sum(total_lobsters))

#____________________________
# Graph it!

ggplot(data = lobster_year, aes(x = year, y = total_lobsters)) +
         geom_line(aes(color = site)) +
  labs(x = "Year",
       y = "Lobster Count",
       title = "Lobster Abundance (2012-2018)") +
  theme_light() +
  scale_x_continuous(limits = c(2012, 2018)) +
  scale_fill_discrete(name = "Reef", labels = c("Arroyo Quemado", "Carpinteria", "Isla Vista", "Mohawk", "Naples"))
