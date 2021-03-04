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
top_intertidal <- read.csv('./data/top_intertidal_spp.csv')

ct_top <- ct_sp %>% filter(ct_sp %in% top_intertidal$name) %>%
  rename(Organism = ct_sp)
pc_top <- pc_sp %>% filter(pc_sp %in% top_intertidal$name) %>%
  rename(Organism = pc_sp)

#merge0 
inter_top <- merge(ct_top, pc_top, all.x = TRUE, all.y = TRUE)

#remove things that aren't plants or animals...
not_alive <- c('Bare rock', 'Black zone', 'Shell hash')
inter_top <- subset(inter_top, !(inter_top$Organism %in% not_alive))

#remove species we already have in our sheet
in_sheet <- c('Saccorhiza dermatodea','Saccharina latissima','Mytilus edulis',
  'Modiolus modiolus','Alaria esculenta','Strongylocentrotus droebachiensis',
  'Henricia sanguinolenta','Colpomenia peregrina','Codium fragile',
  'Cancer irroratus','Cancer borealis','Asterias rubens','Asterias forbesi',
  'Ascophylum nodosum','Schizoporella unicornis','Ptilota serrata','Porphyra',
  'Polysiphonia','Palmaria palmata','Ostrea edulis','Membranipora membranacea',
  'Mastocarpus stellatus','Lithophyllum ','Hildenbradia rubra',
  'Euthora cristata','Didemnum vexillum','Cystoclonium purpureum',
  'Crepidula fornicata','Corallina officinalis','Clathromorphum circumscriptum',
  'Chondrus crispus','Botryllus schlosseri','Botryllus violaceus',
  'Bonnemaisonia hamifera')

inter_unique <- subset(inter_top, !(inter_top$Organism %in% in_sheet))

 #of these lets sort out the ct and pc data with only these and >0 count

ct_filter <- ct_data %>% filter(Organism %in% inter_top$Organism & Count >= 1
                                & Transect %in% transect_choice)

pc_filter <- pc_data %>% filter(Organism %in% inter_top$Organism & Percent_cover > 0
                                & Transect %in% transect_choice)

#get a rough idea of counts (how many transects x years a taxon was found, not 'count')
table(ct_filter$Organism)

#turn into count data, needs to be factor
ct_filter$Organism <- as.factor(ct_filter$Organism)
ct_freq <- count(ct_filter, Organism)

ct_freq <- ct_freq %>%
  arrange(n)

#barplot of species frequencies
ct_freq_sp <- ggplot(ct_freq) +
  geom_col(aes(x = reorder(Organism, n), y = n)) +
  labs(title = 'Occurrences of CT species, year x transect x level x replicate', y = '', x = '', fill = '') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = 'italic'), legend.position = c(.9,.8))

ct_freq_sp

ggsave('20200303ct_freq_sp.png', plot = last_plot(), device = 'png')

ct_freq_25 <- ct_freq[1:25,]

#barplot of species frequencies
ct_bottom_25_sp <- ggplot(ct_freq_25) +
  geom_col(aes(x = reorder(Organism, n), y = n)) +
  labs(title = '25 species with lowest occurrence', y = '', x = '', fill = '') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = 'italic'), legend.position = c(.9,.8))

ct_bottom_25_sp

ggsave('20200303ct_bottom25_sp.png', plot = last_plot(), device = 'png')

