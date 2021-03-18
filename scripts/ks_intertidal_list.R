library(tidyverse)
library(dplyr)
library(stringr)

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

#merge 
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

##CT filter first

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

ggsave('./output/20200303ct_freq_sp.png', plot = last_plot(), device = 'png')

ct_freq_25 <- ct_freq[1:25,]

#barplot of species frequencies
ct_bottom_25_sp <- ggplot(ct_freq_25) +
  geom_col(aes(x = reorder(Organism, n), y = n)) +
  labs(title = '25 species with lowest occurrence', y = '', x = '', fill = '') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = 'italic'), legend.position = c(.9,.8))

ct_bottom_25_sp

ggsave('./output/20200303ct_bottom25_sp.png', plot = last_plot(), device = 'png')

#extract the species we want
ct_select <- ct_freq %>% filter(n > 1000)
#ct_select <- ct_freq %>% filter(n < 1000 & n > 250)

write.table(ct_select$Organism, './output/ct_select.csv', 
          row.names = FALSE, col.names = FALSE)



##PC filter
#lets see rough counts
table(pc_filter$Organism)

#turn into count data, needs to be factor
pc_filter$Organism <- as.factor(pc_filter$Organism)
pc_freq <- count(pc_filter, Organism)

pc_freq <- pc_freq %>%
  arrange(n)

#barplot of species frequencies
pc_freq_sp <- ggplot(pc_freq) +
  geom_col(aes(x = reorder(Organism, n), y = n)) +
  labs(title = 'Occurrences of PC species, year x transect x level x replicate', y = '', x = '', fill = '') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = 'italic'), legend.position = c(.9,.8))

pc_freq_sp

ggsave('./output/20200303pc_freq_sp.png', plot = last_plot(), device = 'png')

pc_freq_25 <- pc_freq[1:25,]

#barplot of species frequencies
pc_bottom_25_sp <- ggplot(pc_freq_25) +
  geom_col(aes(x = reorder(Organism, n), y = n)) +
  labs(title = '25 PC species with lowest occurrence', y = '', x = '', fill = '') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = 'italic'), legend.position = c(.9,.8))

pc_bottom_25_sp

ggsave('./output/20200303pc_bottom25_sp.png', plot = last_plot(), device = 'png')

#extract the species we want
pc_select <- pc_freq %>% filter(n > 1000)

#pc_select <- pc_freq %>% filter(n < 1000 & n > 250)

#before we export lets make sure we aren't exporting duplicates
#first lets filter out the species we already selected from ct
pc_select2 <- pc_select %>% subset(!(pc_select$Organism %in% ct_select$Organism))


write.table(pc_select2$Organism, './output/pc_select.csv', 
            row.names = FALSE, col.names = FALSE)



#Lets check the years our species are found to make sure 
#we aren't excluding recent invaders
ct_check <- ct_freq %>% filter(n < 1000)
pc_check <- pc_freq %>% filter(n < 1000)

ct_check <- ct_freq %>% filter(n < 250)
pc_check <- pc_freq %>% filter(n < 250)

#filter data by under 1000 list
ct_check_data <- ct_data %>% filter(Organism %in% ct_check$Organism & Count >= 1
                                & Transect %in% transect_choice) %>%
                          select(Organism, Year) %>%
                          distinct(Organism, Year)

pc_check_data <- pc_data %>% filter(Organism %in% pc_check$Organism & Percent_cover > 0
                                & Transect %in% transect_choice) %>%
                          select(Organism, Year, Transect) %>%
                          distinct(Organism, Year)


check_all <- merge(ct_check_data, pc_check_data, all.x = TRUE, all.y = TRUE)


check_all$Year <- as.factor(check_all$Year)

ggplot(check_all, aes(x = Organism, y = Year, fill = Year)) +
  geom_bar(position="stack", stat="identity") +
  coord_flip()



#Keen dataset
keen_data <- read.csv('./data/keen_all_methods_site_merged.csv')

keen_sp <- unique(keen_data$SPECIES)
keen_sp <- as.data.frame(keen_sp)

in_sheet2 <- c('Saccorhiza dermatodea','Saccharina latissima','Alaria esculenta',
               'Colpomenia peregrina','Codium fragile','Ascophyllum nodosum',
               'Ptilota serrata','Porphyra','Polysiphonia','Palmaria palmata',
               'Mastocarpus stellatus','Lithophyllum ','Hildenbrandia rubra',
               'Euthora cristata','Cystoclonium purpureum','Corallina officinalis',
               'Clathromorphum circumscriptum','Chondrus crispus',
               'Bonnemaisonia hamifera','Verrucaria','Arthopyrenia',
               'Mytilus edulis','Modiolus modiolus',
               'Strongylocentrotus droebachiensis','Henricia sanguinolenta',
               'Cancer irroratus','Cancer borealis','Asterias rubens',
               'Asterias forbesi','Schizoporella unicornis','Ostrea edulis',
               'Membranipora membranacea','Didemnum vexillum','Crepidula fornicata',
               'Botryllus schlosseri','Botryllus violaceus','Littorina saxatilis',
               'Anurida maritima','Littorina littorea','Amphipoda',
               'Littorina obtusata','Nucella lapillus',
               'Semibalanus balanoides','Elachista fucicola',
               'Cyanobacteria','Phymatolithon lenormandii','Ulva lactuca',
               'Modiolus modiolus','Corallina officinalis','Carcinus maenus',
               'Isopoda','Metridium senile','Tectura testudinalis',
               'Prasiola stipitata','Strongylocentrotus droebachiensis',
               'Hiatella arctica','Spirorbis','Anomia simplex','Lacuna vincta',
               'Ophiopholis','Crepidula fornicata')

#get the unique species that aren't already in the list
keen_unique <- subset(keen_sp, !(keen_sp %in% in_sheet2))
#remove na's
keen_unique <- na.omit(keen_unique)
#check
keen_unique

#lets export this and manually remove some of the stuff
write.table(keen_unique$keen_sp, './output/keen_sp_raw.csv', 
            row.names = FALSE, col.names = FALSE)

#unidentified/vague entries to remove, already used genera
keen_remove <- c("UNID Juv Laminariales","Ulvaria","Amphipod tube mat",
                 "Barnacle","Encrusting coralline","Red Algal Turf",
                 "Unidentified Erect Coralline","Unidentified Red Blade",
                 "Blady Ulvoid","Diatom Tube Mat","Unidentified Filamentous Red",
                 "Tubular Ulvoid","Cliona","Myoxocephalus","Anomia","Phymatolithon")

#trimmed list
keen_trimmed <- subset(keen_unique, !(keen_sp %in% keen_remove))

write.table(keen_trimmed$keen_sp, './output/keen_sp_trimmed.csv', 
            row.names = FALSE, col.names = FALSE)



#comparing our data to the overall species list to generate a list of missed species
trait_sp <- read.csv('./data/traits_sp_list.csv')
ne_sp <- read.csv('./data/new-england_sp_list_jarrett.csv')

#trim whitespace and remove duplicates
ne_sp <- ne_sp %>%
  mutate(Species = str_trim(Species, side = "both")) %>% 
  distinct(ne_sp$Species, .keep_all = TRUE)

unused_sp <- ne_sp %>%
  filter(!(Species %in% trait_sp$species))
  
write.csv(unused_sp$Species, './data/unused_sp.csv')