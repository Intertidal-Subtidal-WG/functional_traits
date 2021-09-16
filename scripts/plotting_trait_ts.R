# Plotting Functional Traits by Abundance through Time
# EKB; WG second session (Sept 2021)

# PACKAGES and DATA #===========================================================
library(tidyverse)

# count and cover data from the intertidal
count_data <- read_csv("data/fielddata/ct_clean.csv")
cover_data <- read_csv("data/fielddata/pc_clean.csv")

# transects and quadrants
transects <- read_csv("data/fielddata/transect_info.csv") %>% 
  rename(Transect = INTERTIDAL_TRANSECT, Site = SITE, KEEN_transect = TRANSECT) %>% 
  distinct()

# cleaned traits data (not imputed)
algae <- read_csv("data/clean/algae_clean.csv")
animal <- read_csv("data/clean/animal_clean.csv")

# SUMMARIZE #===================================================================

# Count Data #

# average any replicates and remove rows where no data was collected
# add transect info
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
  
# Cover Date #

# average any replicates and remove rows where no data was collected
cover <-  cover_data %>% 
  select(Year, Transect, Level, Replicate, Data_taken, Organism, Percent_cover) %>% 
  filter(Data_taken != "no") %>%
  group_by(Year, Transect, Level, Replicate, Organism) %>% 
  summarise(Cover = mean(Percent_cover)) %>% 
  ungroup() %>% 
  select(-Replicate)

# get average cover per plot for each transect (by year)
cover_avg_by_transect <- cover %>%
  group_by(Year, Transect, Organism) %>%
  summarise(Abund = sum(Cover)) %>% 
  left_join(., cover %>% 
              group_by(Year, Transect) %>% 
              summarise(n_level = n_distinct(Level))) %>% 
  mutate(Avg_abund_per_level = Abund / n_level)

# get average cover per plot/transect per year
cover_avg_by_year <- cover %>% 
  group_by(Year, Organism) %>% 
  summarise(Abund = sum(Cover)) %>% 
  left_join(., cover %>% group_by(Year) %>% 
              summarise(n_transect = n_distinct(Transect))) %>% 
  mutate(Avg_abund_per_transect = Abund / n_transect)

# MERGE DATASETS #==============================================================

# Traits #
# join animal and algae together

# add diet and trophic level to algae
algae <- algae %>% 
  mutate(dietary_pref_c = "autotroph",
         trophic_level = "primary producer")

all_sp_traits <- bind_rows(algae, animal)

# Combine Traits with Abundance Data #

count_transect_all <- count_avg_by_transect %>% 
  inner_join(., all_sp_traits, by = c("Organism" = "species"))

count_year_all <- count_avg_by_year %>% 
  inner_join(., all_sp_traits, by = c("Organism" = "species"))

cover_transect_all <- cover_avg_by_transect %>% 
  inner_join(., all_sp_traits, by = c("Organism" = "species"))

cover_year_all <- cover_avg_by_year %>% 
  inner_join(., all_sp_traits, by = c("Organism" = "species"))

# Transects #
# add quadrant info to cover and count data

count_avg_by_transect <- count_avg_by_transect %>% 
  left_join(., )


# PLOTTING #====================================================================

# COUNT DATA BY TRANSECT

# morphology
morph <- count_transect_all %>% 
  group_by(Year, morphology1) %>%
  summarise(Sum = sum(Avg_abund_per_level))
  
ggplot(morph, aes(x = Year, y = Sum, color = morphology1)) +
  geom_point() +
  geom_line()
  
# trophic level
troph <- count_transect_all %>% 
  group_by(Year, trophic_level) %>%
  summarise(Sum = sum(Avg_abund_per_level))

ggplot(troph, aes(x = Year, y = Sum, color = trophic_level)) +
  geom_point() +
  geom_line()
