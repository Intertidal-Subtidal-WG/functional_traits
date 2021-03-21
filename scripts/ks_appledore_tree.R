#making a tree of existing species with open tree of life

#one for algae, one for animals, drop all others

library(rotl)
library(dplyr)
library(stringr)
library(ape)

#our species list
trait_sp <- read.csv('./data/traits_sp_group.csv')

trait_sp$group <- as.factor(trait_sp$group)

#split into animals and plants lists
trait_sp_animals <- filter(trait_sp, group == 'Invertebrate' | group == 'Vertebrate')
trait_sp_algae <- filter(trait_sp, group == 'Algae')

#taxa search

taxa_animals <- tnrs_match_names(names = trait_sp_animals$species, context_name = 'Animals')
animals_clean <- filter(taxa_animals, flags == "")
#contexts are land plant based so idk if there's a better way to do this
taxa_algae <- tnrs_match_names(names = trait_sp_algae$species, context_name = 'All life')
algae_clean <- filter(taxa_algae, flags == "")

#trees
animals_tree <- tol_induced_subtree(ott_ids = animals_clean$ott_id, label_format = 'name')
algae_tree <- tol_induced_subtree(ott_ids = algae_clean$ott_id, label_format = 'name')
#test
plot(animals_tree, cex = .8, label.offset = .1, no.margin = TRUE)
plot(algae_tree, cex = .8, label.offset = .1, no.margin = TRUE)

write.tree(animals_tree, file = './data/animals-rotl.tre')

