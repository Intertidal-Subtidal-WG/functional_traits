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

ct_filter <- ct_data %>% filter(Organism %in% inter_top$Organism & Count > 0
                                & Transect %in% transect_choice)

pc_filter <- pc_data %>% filter(Organism %in% inter_top$Organism & Percent_cover > 0
                                & Transect %in% transect_choice)




