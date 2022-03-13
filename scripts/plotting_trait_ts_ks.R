# Plotting Functional Traits by Abundance through Time
# EKB; WG second session (Sept 2021)

# PACKAGES and DATA #===========================================================
library(tidyverse)
library(patchwork)

# count and cover data from the intertidal
count_data <- read_csv("data/fielddata/ct_clean.csv")
cover_data <- read_csv("data/fielddata/pc_clean.csv")

# transects and position (exposed vs. sheltered)
transects <- read_csv("data/fielddata/transect_info.csv") %>% 
  select(Transect, Position)

# cleaned traits data (not imputed)
## these were imputed versions actually
#algae <- read_csv("data/clean/algae_clean.csv")
#animal <- read_csv("data/clean/animal_clean.csv")

algae <- read_csv("data/algae_clean.csv")
animal <- read_csv("data/animal_clean.csv")

# SUMMARIZE #===================================================================

# Count Data #

# average any replicates and remove rows where no data was collected
# add transect info
count <-  count_data %>% 
  select(Year, Transect, Level, Replicate, Data_taken, Organism, Count) %>% 
  # remove rows where data was not collected
  filter(Data_taken != "no") %>%
  # remove parenthetical from organism names
  mutate(Organism = stringr::str_replace(Organism, " \\s*\\([^\\)]+\\)", "")) %>% 
  group_by(Year, Transect, Level, Replicate, Organism) %>% 
  # avg any replicates so each Year/Transect/Level/Org combo has one value
  summarise(Count = mean(Count)) %>% 
  ungroup() %>% 
  select(-Replicate) %>% 
  # join in transect position data
  left_join(., transects)

# get average count per level for each transect (by year)
count_avg_by_transect <- count %>%
  # sum abundance along each transect per year
  group_by(Year, Transect, Organism, Position) %>%
  summarise(Abund = sum(Count)) %>% 
  # add a column with the number of levels in that transect that year
  left_join(., count %>% 
              group_by(Year, Transect) %>% 
              summarise(n_level = n_distinct(Level))) %>% 
  # calculate avg # of individuals per level for each transect
  mutate(Avg_abund_per_level = Abund / n_level)

# get average count per transect per year and relative abundance
count_avg_by_year <- count %>% 
  group_by(Year, Organism, Position) %>% 
  # sum abundance per island side (position) per year
  summarise(Abund = sum(Count)) %>%
  # add a column with the number of transects on that side of the island that year
  left_join(., count %>% group_by(Year, Position) %>% 
              summarise(n_transect = n_distinct(Transect))) %>% 
  # calculate avg # of ind per transect for each side of the island
  mutate(Avg_abund_per_transect = Abund / n_transect) %>% 
  # RELATIVE ABUNDANCE #
  # add a column with the total # ind per side of the island in each year
  left_join(., count %>% group_by(Year, Position) %>% 
              summarize(Total_abund = sum(Count))) %>% 
  # calculate relative abund per side of island for each species (per year)
  mutate(Rel_abund_by_side = Abund / Total_abund)
  
# Cover Data #
# see comments above in count data for explanations of the code

# average any replicates and remove rows where no data was collected
cover <-  cover_data %>% 
  select(Year, Transect, Level, Replicate, Data_taken, Organism, Percent_cover) %>% 
  filter(Data_taken != "no") %>%
  group_by(Year, Transect, Level, Replicate, Organism) %>% 
  summarise(Cover = mean(Percent_cover)) %>% 
  ungroup() %>% 
  select(-Replicate) %>% 
  left_join(., transects)

# get average cover per plot for each transect (by year)
cover_avg_by_transect <- cover %>%
  group_by(Year, Transect, Organism, Position) %>%
  summarise(Abund = sum(Cover)) %>% 
  left_join(., cover %>% 
              group_by(Year, Transect) %>% 
              summarise(n_level = n_distinct(Level))) %>% 
  mutate(Avg_abund_per_level = Abund / n_level)

# get average cover per plot/transect per year
cover_avg_by_year <- cover %>% 
  group_by(Year, Organism, Position) %>% 
  summarise(Abund = sum(Cover)) %>% 
  left_join(., cover %>% group_by(Year, Position) %>% 
              summarise(n_transect = n_distinct(Transect))) %>% 
  mutate(Avg_abund_per_transect = Abund / n_transect) %>% 
  # calculate relative abundance
  left_join(., cover %>% group_by(Year, Position) %>% 
              summarize(Total_abund = sum(Cover))) %>% 
  mutate(Rel_abund_by_side = Abund / Total_abund)
#write_csv(cover_avg_by_year, "data/clean/relative_abund_cover.csv")

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

# PLOTTING #====================================================================

# RELATIVE ABUNDANCE: COUNT DATA BY YEAR

# group
plot1 <- count_year_all %>% 
   group_by(Year, Position, group) %>%
   summarise(Sum = sum(Rel_abund_by_side)) %>% 
   ggplot(aes(x = Year, y = Sum, color = group)) +
   geom_point() +
   geom_line() +
   facet_wrap(~ Position, nrow = 2) + 
   theme_bw()


# body_size_avg_bin
plot2 <- count_year_all %>% 
    group_by(Year, Position, body_size_avg_bin) %>%
    summarise(Sum = sum(Rel_abund_by_side)) %>% 
    drop_na() %>% 
    ggplot(aes(x = Year, y = Sum, color = body_size_avg_bin)) +
    geom_point() +
    geom_line() +
    facet_wrap(~ Position, nrow = 2) + 
    theme_bw()

# morphology
plot3 <- count_year_all %>% 
    group_by(Year, Position, morphology1) %>%
    summarise(Sum = sum(Rel_abund_by_side)) %>% 
    ggplot(aes(x = Year, y = Sum, color = morphology1)) +
    geom_point() +
    geom_line() +
    facet_wrap(~ Position, nrow = 2) + 
    theme_bw()


# trophic level
plot4 <- count_year_all %>% 
    group_by(Year, Position, trophic_level) %>%
    summarise(Sum = sum(Rel_abund_by_side)) %>% 
    ggplot(aes(x = Year, y = Sum, color = trophic_level)) +
    geom_point() +
    geom_line() +
    facet_wrap(~ Position, nrow = 2) + 
    theme_bw()

# dietary_pref
plot5 <- count_year_all %>% 
    group_by(Year, Position, dietary_pref_c) %>%
    summarise(Sum = sum(Rel_abund_by_side)) %>% 
    ggplot(aes(x = Year, y = Sum, color = dietary_pref_c)) +
    geom_point() +
    geom_line() +
    facet_wrap(~ Position, nrow = 2) + 
    theme_bw()

# motility_adult
plot6 <- count_year_all %>% 
    group_by(Year, Position, motility_adult) %>%
    summarise(Sum = sum(Rel_abund_by_side)) %>% 
    ggplot(aes(x = Year, y = Sum, color = motility_adult)) +
    geom_point() +
    geom_line() +
    facet_wrap(~ Position, nrow = 2) + 
    theme_bw()

# epibiotic
plot7 <- count_year_all %>% 
    group_by(Year, Position, epibiotic) %>%
    summarise(Sum = sum(Rel_abund_by_side)) %>% 
    ggplot(aes(x = Year, y = Sum, color = epibiotic)) +
    geom_point() +
    geom_line() +
    facet_wrap(~ Position, nrow = 2) + 
    theme_bw()

# benthic
plot8 <- count_year_all %>% 
    group_by(Year, Position, benthic) %>%
    summarise(Sum = sum(Rel_abund_by_side)) %>% 
    ggplot(aes(x = Year, y = Sum, color = benthic)) +
    geom_point() +
    geom_line() +
    facet_wrap(~ Position, nrow = 2) + 
    theme_bw()

# invasive
plot9 <- count_year_all %>% 
    group_by(Year, Position, invasive) %>%
    summarise(Sum = sum(Rel_abund_by_side)) %>% 
    drop_na() %>% 
    ggplot(aes(x = Year, y = Sum, color = invasive)) +
    geom_point() +
    geom_line() +
    facet_wrap(~ Position, nrow = 2) + 
    theme_bw()

count_rel_abund_plots <- (plot1 + plot2 + plot3) / 
  (plot4 + plot5 + plot6) / 
  (plot7 + plot8 + plot9) +
  plot_annotation(title = "Relative Abundance (Count)")
ggsave("output/2022march/count_rel_abund_plots.png",
       count_rel_abund_plots,
       device = 'png')



# RELATIVE ABUNDANCE: COVER DATA

# group
(plot1 <- cover_year_all %>% 
    group_by(Year, Position, group) %>%
    summarise(Sum = sum(Rel_abund_by_side)) %>% 
    ggplot(aes(x = Year, y = Sum, color = group)) +
    geom_point() +
    geom_line() +
    facet_wrap(~ Position, nrow = 2) + 
    theme_bw())

# body_size_avg_bin
(plot2 <- cover_year_all %>% 
    group_by(Year, Position, body_size_avg_bin) %>%
    summarise(Sum = sum(Rel_abund_by_side)) %>% 
    drop_na() %>% 
    ggplot(aes(x = Year, y = Sum, color = body_size_avg_bin)) +
    geom_point() +
    geom_line() +
    facet_wrap(~ Position, nrow = 2) + 
    theme_bw())

# morphology
(plot3 <- cover_year_all %>% 
    group_by(Year, Position, morphology1) %>%
    summarise(Sum = sum(Rel_abund_by_side)) %>% 
    ggplot(aes(x = Year, y = Sum, color = morphology1)) +
    geom_point() +
    geom_line() +
    facet_wrap(~ Position, nrow = 2) + 
    theme_bw())

# trophic level
(plot4 <- cover_year_all %>% 
    group_by(Year, Position, trophic_level) %>%
    summarise(Sum = sum(Rel_abund_by_side)) %>% 
    ggplot(aes(x = Year, y = Sum, color = trophic_level)) +
    geom_point() +
    geom_line() +
    facet_wrap(~ Position, nrow = 2) + 
    theme_bw())

# dietary_pref
(plot5 <- cover_year_all %>% 
    group_by(Year, Position, dietary_pref_c) %>%
    summarise(Sum = sum(Rel_abund_by_side)) %>% 
    ggplot(aes(x = Year, y = Sum, color = dietary_pref_c)) +
    geom_point() +
    geom_line() +
    facet_wrap(~ Position, nrow = 2) + 
    theme_bw())

# motility_adult
(plot6 <- cover_year_all %>% 
    group_by(Year, Position, motility_adult) %>%
    summarise(Sum = sum(Rel_abund_by_side)) %>% 
    ggplot(aes(x = Year, y = Sum, color = motility_adult)) +
    geom_point() +
    geom_line() +
    facet_wrap(~ Position, nrow = 2) + 
    theme_bw())

# epibiotic
(plot7 <- cover_year_all %>% 
    group_by(Year, Position, epibiotic) %>%
    summarise(Sum = sum(Rel_abund_by_side)) %>% 
    ggplot(aes(x = Year, y = Sum, color = epibiotic)) +
    geom_point() +
    geom_line() +
    facet_wrap(~ Position, nrow = 2) + 
    theme_bw())

# benthic
(plot8 <- cover_year_all %>% 
    group_by(Year, Position, benthic) %>%
    summarise(Sum = sum(Rel_abund_by_side)) %>% 
    ggplot(aes(x = Year, y = Sum, color = benthic)) +
    geom_point() +
    geom_line() +
    facet_wrap(~ Position, nrow = 2) + 
    theme_bw())

# invasive
(plot9 <- cover_year_all %>% 
    group_by(Year, Position, invasive) %>%
    summarise(Sum = sum(Rel_abund_by_side)) %>% 
    drop_na() %>% 
    ggplot(aes(x = Year, y = Sum, color = invasive)) +
    geom_point() +
    geom_line() +
    facet_wrap(~ Position, nrow = 2) + 
    theme_bw())

cover_rel_abund_plots <- (plot1 + plot2 + plot3) / 
  (plot4 + plot5 + plot6) / 
  (plot7 + plot8 + plot9) +
  plot_annotation(title = "Relative Abundance (Cover)")
ggsave("output/2022march/cover_rel_abund_plots.png",
       cover_rel_abund_plots, device = 'png',
       width = 12)
