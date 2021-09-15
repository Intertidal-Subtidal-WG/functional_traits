# Plotting Functional Traits by Abundance through Time
# EKB; WG second session (Sept 2021)

# PACKAGES and DATA 
library(tidyverse)

count_data <- read_csv("data/fielddata/ct_clean.csv")
cover_data <- read_csv("data/fielddata/pc_clean.csv")

# SUMMARIZE

# average any replicates and remove rows where no data was collected
count <-  count_data %>% 
  select(Year, Transect, Level, Replicate, Data_taken, Organism, Count) %>% 
  filter(Data_taken != "no") %>%
  group_by(Year, Transect, Level, Replicate, Organism) %>% 
  summarise(Count = mean(Count)) %>% 
  ungroup() %>% 
  select(-Replicate)

# get average count per plot for each transect (by year)
count_avg_by_transect <- count %>%
  group_by(Year, Transect, Organism) %>%
  summarise(Abund = sum(Count)) %>% 
  left_join(., count %>% 
              group_by(Year, Transect) %>% 
              summarise(n_level = n_distinct(Level))) %>% 
  mutate(Avg_abund_per_level = Abund / n_level)

# get average count per plot/transect per year
count_avg_by_year <- count %>% 
  group_by(Year, Organism) %>% 
  summarise(Abund = sum(Count)) %>% 
  left_join(., count %>% group_by(Year) %>% 
              summarise(n_transect = n_distinct(Transect))) %>% 
  mutate(Avg_abund_per_transect = Abund / n_transect)
  


