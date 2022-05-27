###############################################################################
### #### ###### #### ###### #### ###### #### ###### #### ###### #### #### ### 
### Project Working group CIEE
## R-code  functional traits
## Calculating fucntional diversity metrics
#### last update:Nov 2021
### #### ###### #### ###### #### ###### #### ###### #### ###### #### ###### ###
# Loading packages --------------------------------------------------------
install.packages("iNEXT")
install.packages("data.table")
install.packages("factoextra")
install.packages("funrar")
install.packages("FD")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("ks")
install.packages("tidyverse")
install.packages("picante")
install.packages("tibble")



library(iNEXT)
library(data.table)
library(factoextra)
library(funrar)
library(FD)
library(ggplot2)
library(gridExtra)
library(ks)
library(tidyverse)
library(picante)
library(tibble)

# Data --------------------------------------------------------------------

algae_traits <- read_csv(traits, "20220525_1982-end_algae.csv") %>%
  filter(intertidal == 'yes') %>%
  select(!(c(intertidal, group, size_categories, feeding, 
             saccate_external_morphology,
             potential_asexual_reproduction,
             epilithic_use_space,
             epiphytic_env_position, epizoic_env_position,
             macroalgae)))

algae_traits <- algae_traits %>%
  mutate(maximun_longevity = str_replace_all(maximun_longevity, c(" \\(one-ten years\\)"= "")))


animal_traits <- read_csv(here(traits, "20220321_1982-end_animal.csv")) %>%
  filter(intertidal == 'yes') %>%
  select(!(c(intertidal, trophic_level, pelagic, deposit_feeder, benthic)))

#jm

algae_traits<-read_csv("20220525_algae_imputed_copy.csv")%>%
  filter(intertidal_gom =="1") %>%
  select(!(c(species, phylum, group, common_name_division_bgr,subtidal_gom, autotroph ))) 

View(algae_traits)


qfs <- quality_funct_space(algae_traits, traits_weights=NULL, nbdim=14, metric="Gower", dendro=FALSE, plot="quality_funct_space") 

# quality of spaces (low meanSD = high quality)
round( qfs$meanSD , 4)

# keeping coordiantes on the 4 dimensions, meanSD<0.004
fd.coord <- qfs$details_funct_space$mat_coord[,1:4]

write.csv(fd.coord, file="FE_4D_coord_jen.csv") #to use it for further analyses

#see variance explained by the PCoA axes
gower<-qfs$details_funct_space$mat_dissim

fit <- cmdscale(gower,eig=TRUE, k=4) # PCoA

# variance explained by the axes
cumsum(fit$eig[fit$eig>=0]) / sum(fit$eig[fit$eig>0])

#trying to plot

convhulln(m, options = "FA")

a<-convhulln(fd.coord, options = "FA")
a

plot(a)

plot(fd.coord[,1], fd.coord[,2], xlab = "PCoA 1", ylab = "PCoA 2", type="n")

plot(polygon(a))

conhull
convhulln
# Timeseries [copied from Kate's code] -------------------------------------------------------------

###
#Algae
### 

algae_82_95 <- algae_traits %>%
  filter(year <= 1994 & year >= 1982) %>%
  select(!(c(year, transect, position))) %>%
  distinct()

algae_96_06 <- algae_traits %>%
  filter(year >= 1996 & year < 2007) %>%
  select(!(c(year, transect, position))) %>%
  distinct()

algae_10_end <- algae_traits %>%
  filter(year <= 2011) %>%
  select(!(c(year, transect, position))) %>%
  distinct()


# exposed v sheltered beginning and end
algae_exposed_start <- algae_traits %>%
  filter(position == 'exposed' & year <= 1994 & year >= 1982) %>%
  select(!(c(year, transect, position))) %>%
  distinct()

algae_sheltered_start <- algae_traits %>%
  filter(position == 'sheltered' & year <= 1994 & year >= 1982) %>%
  select(!(c(year, transect, position))) %>%
  distinct()


algae_sheltered_middle <- algae_traits %>%
  filter(position == 'sheltered' & year >= 1996 & year < 2007) %>%
  select(!(c(year, transect, position))) %>%
  distinct()

algae_exposed_middle <- algae_traits %>%
  filter(position == 'exposed' & year >= 1996 & year < 2007) %>%
  select(!(c(year, transect, position))) %>%
  distinct()

algae_exposed_end <- algae_traits %>%
  filter(position == 'exposed' & year <= 2010) %>%
  select(!(c(year, transect, position))) %>%
  distinct()

algae_sheltered_end <- algae_traits %>%
  filter(position == 'sheltered' & year <= 2010) %>%
  select(!(c(year, transect, position))) %>%
  distinct()

###
#Animals
###

# filter
animal_82_95 <- animal_traits %>%
  filter(year <= 1994 & year >= 1982) %>%
  select(!(c(year, transect, position))) %>%
  distinct()

animal_96_06 <- animal_traits %>%
  filter(year >= 1996 & year < 2007) %>%
  select(!(c(year, transect, position))) %>%
  distinct()

animal_10_end <- animal_traits %>%
  filter(year <= 2011) %>%
  select(!(c(year, transect, position))) %>%
  distinct()

# exposed v sheltered beginning and end

animal_exposed_start <- animal_traits %>%
  filter(position == 'exposed' & year <= 1994 & year >= 1982) %>%
  select(!(c(year, transect, position))) %>%
  distinct()

animal_sheltered_start <- animal_traits %>%
  filter(position == 'sheltered' & year <= 1994 & year >= 1982) %>%
  select(!(c(year, transect, position))) %>%
  distinct()

animal_exposed_middle <- animal_traits %>%
  filter(position == 'exposed' & year >= 1996 & year < 2007) %>%
  select(!(c(year, transect, position))) %>%
  distinct()

animal_sheltered_middle <- animal_traits %>%
  filter(position == 'sheltered' & year >= 1996 & year < 2007) %>%
  select(!(c(year, transect, position))) %>%
  distinct()

animal_exposed_end <- animal_traits %>%
  filter(position == 'exposed' & year <= 2010) %>%
  select(!(c(year, transect, position))) %>%
  distinct()

animal_sheltered_end <- animal_traits %>%
  filter(position == 'sheltered' & year <= 2010) %>%
  select(!(c(year, transect, position))) %>%
  distinct()



# Distance Based Functional diversity indexes dbFD----------------------------------------------
# [See documentation https://search.r-project.org/CRAN/refmans/FD/html/dbFD.html]
# If x is a distance matrix is taken as it is 
# If  data frame that contains only continuous traits, no NAs, and that no weights are specified (i.e. w is missing), 
# a species-species Euclidean distance matrix is computed via dist. Otherwise, a Gower dissimilarity matrix is computed via gowdis. 

# Traits can be numeric, ordered, or factor. 
# Binary traits should be numeric and only contain 0 and 1. 
# character traits will be converted to factor. NAs are tolerated.
#In all cases, species labels are required.

fd_iNDICES <- dbFD(tr_mi_z2, mat,w.abun = TRUE, calc.FRic = TRUE, m = "max", stand.FRic = TRUE , calc.FDiv = TRUE,calc.CWM = FALSE, print.pco = FALSE, messages = TRUE)

dbFD
fd_iNDICES



