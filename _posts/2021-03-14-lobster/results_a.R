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
library(directlabels)



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

# Rename sites within data set
lobster_year$site[lobster_year$site == "AQUE"] <- "Arroyo Quemado"
lobster_year$site[lobster_year$site == "CARP"] <- "Carpinteria"
lobster_year$site[lobster_year$site == "NAPL"] <- "Naples (MPA)"
lobster_year$site[lobster_year$site == "IVEE"] <- "Isla Vista (MPA)"
lobster_year$site[lobster_year$site == "MOHK"] <- "Mohawk"

#____________________________
# Graph it!

ggplot(data = lobster_year, aes(x = year, y = total_lobsters)) +
         geom_line(aes(color = site), show.legend = FALSE) +
  labs(x = "Year",
       y = "Lobster Count",
       title = "Lobster Abundance, Santa Barbara Reefs (2012-2018)",
       caption = "Figure Caption") +
  theme_light() +
  scale_x_continuous(lim = c(2012, 2019.5), expand = c(0, 0)) +
  scale_y_continuous(lim = c(0, 1000), expand = c(0, 0)) +
  # Direct labels to label the lines instead of a legend
  geom_dl(aes(label = site), method = list("last.points", cex = .65, hjust = -.17)) +
  # Change colors of the lines 
  scale_color_manual(values = c("lightcoral", "gold3", "steelblue1", "mediumorchid", "darkturquoise"))
