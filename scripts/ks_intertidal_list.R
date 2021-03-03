library(tidyverse)

#generate lists of unique species in all intertidal transects with more than 6 years of data

ct_data <- read.csv('./data/ct_clean.csv')
pc_data <- read.csv('./data/pc_clean.csv')

#pick transects
transect_choice <- c(1,2,5,7,13,15,20,22,24,26,28)

ct_sort <- ct_data %>% filter(Transect %in% transect_choice)
pc_sort <- pc_data %>% filter(Transect %in% transect_choice)

ct_sp <- unique(ct_sort$Organism)
pc_sp <- unique(pc_sort$Organism)

#as.data.frame for filtering
ct_sp <- as.data.frame(ct_sp)
pc_sp <- as.data.frame(pc_sp)

#filter list to sp in species list
#Jakes list
top_intertidal <- read.csv('./SEED_data/intertidal_data/top_intertidal_spp.csv')

ct_top <- ct_sp %>% filter(ct_sp %in% top_intertidal$name) %>%
  rename(Organism = ct_sp)
pc_top <- pc_sp %>% filter(pc_sp %in% top_intertidal$name) %>%
  rename(Organism = pc_sp)

#merge
inter_top <- merge(ct_top, pc_top, all.x = TRUE, all.y = TRUE)

#remove things that aren't plants or animals...
not_alive <- c('Bare rock', 'Black zone', 'Shell hash')
inter_top <- subset(inter_top, !(inter_top$Organism %in% not_alive))