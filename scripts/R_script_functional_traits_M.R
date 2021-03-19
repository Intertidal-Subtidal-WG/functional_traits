###############################################################################
###############################################################################
## Proyect Working group CIEE
## R-code sub-group functional traits
## 
#### last update: March 5 2021
################################################################################
################################################################################
# Loading packages --------------------------------------------------------
# libraries for easier manipulation of data

install.packages("tidyverse") 
install.packages ("data.table")
install.packages ("extrafont")
installed.packages("lubridate")  #for dates
install.packages("car")

#Other libraries for data analyses
install.packages("vegan")
install.packages("ggplot2")
install.packages("devtools")
install.packages("lme4")
install.packages("knitr")
install.packages("ts")
#for functional traits
install.packages("pacman")
install.packages("factoextra")
install.packages("ggrepel")
install.packages("tibble")
install.packages("funrar") #to calculate functional uniqueness

#libraries 

library(pacman)
pacman::p_load(dplyr, plyr, readr, tbible, FD, ade4, cowplot, mice, reshape2, tidyr, ks, hypervolume, alphallhu, purrr, TTR, plotrix, agricolae, psych)

library(factoextra)
library(ggrepel)
library(tibble)
library(tidyverse)
library(ggplot2)
library(data.table)
library(extrafont)
library(visreg)
library(lubridate)
library(letsR)
library(reshape)
library(reshape2)
library(funrar)#to calculate functional uniqueness
#library(ts)

loadfonts()


#1. Functional space and hypervolume estimation. Code adapted from Cooke et al. 2019 and Gomez et al 2021
####################################################################################

# Load data ---------------------------------------------------------------

marine_traits<-read.csv("marine_traits.csv")

str(marine_traits)

#Scale the data if needed (Applies mosty for numerical traits such us body mass)
scale_traits <- function(x){
  (x - mean(x, na.rm=TRUE))/sd(x, na.rm=TRUE)
}

traits_scaled<-marine_traits %>% 
  mutate(body_size_scale=scale_traits(body_size))%>%
  as.data.frame

traits_scaled<-traits_scaled %>%
  column_to_rownames(var = "species")



# my data frame should only have species name as row names  and morphological traits that I want  as columns
#Colum to raw names
traits_scaled<-traits_scaled %>% 
  select(species, body_size_scale, dietart_preference, thropic_level)%>% 
  column_to_rownames(var = "species")

#### --------------------------------------------------------------
## PCA ##
#### --------------------------------------------------------------

#Run PCA for the whole comunity

#PCA

pca_community <- princomp(traits_scaled, cor = TRUE, scores = TRUE)
summary(pca_community)

plot(pca_community)

#Extract the scores
scoresPCATotal <- as.data.frame(pcaTotal$scores) %>% 
  tibble::rownames_to_column("binomial")

scoresPCATotal <- scoresPCATotal %>% 
  # convert long to wide
  tidyr::gather(key, value, -binomial) %>% 
  tidyr::unite(col, key) %>% 
  tidyr::spread(col, value) 

ggplot(pca_community, aes(x = Comp.1, y = Comp.2))

#######################

#Subset for species in each period (Need to define the lenght of this periods)

Y1982 <- traits_scaled %>% 
  filter(Y1982 == 1) %>% 
  select(species, body_size_scale, dietart_preference, thropic_level) %>% 
  left_join(scoresPCATotal, by = "species")

Y2002 <- traits_scaled %>% 
  filter(Y2002 == 1) %>% 
  select(species, body_size_scale, dietart_preference, thropic_level) %>% 
  left_join(scoresPCATotal, by = "species") 



# kernel density estimation for each period
pc_raw_2002 <- Y2002 %>% 
  # extract first two principal components
  dplyr::select(., species, Comp.1, Comp.2) %>% 
  tibble::column_to_rownames(var = "speciesl")



# save principal component data
write.csv(Y1982, file = "PCA_1982.csv", row.names = FALSE)
write.csv(Y2002, file = "PCA_2002.csv", row.names = FALSE)

write.csv(scoresPCATotal, file = "PCA_Total.csv")
#write.csv(loadingsPCATotal, file = "Loadings_Total.csv", row.names = FALSE)


#. Functional Uniqueness and distinctiveness
#####################################################################################

library(funrar)

# load data
trait <- read_csv("marine_traits.csv")


#Species Year Matrix
data <- read.csv(".csv") 

mat <- dat %>% 
  pivot_wider(names_from =  species, values_from = COUNT)

dist_mat <- 


# Compute distinctiveness for each species on each site
di_df = distinctiveness_stack(com_df = data,  # The site x species table
                              sp_col = "speciesl",  # Name of the species column
                              com = "Year",  # Name of the community column
                              abund = "COUNT",  # Relative abundances column (facultative)
                              dist_matrix = dist_mat)  # Functional Distance matrix


Di_Sum <- di_df %>% 
  group_by(species) %>%
  summarise_at(vars(Di), funs(mean(., na.rm=TRUE)))

Di_Sum <- Di_Sum[order(Di_Sum$Di),] %>% 
  mutate(ID = c(1:233)) %>% 
  right_join(trait, by = "species")


############Plots
# plot 1982
pca_plot_1982 <- ggplot(dcc_1982, aes(x = Var1, y = Var2)) +
  # coloured probabilty background
  geom_raster(aes(fill = value)) +
  scale_fill_gradientn(colours = rev(col_pal)) +
  # points for species
  geom_point(data = Y1982, aes(x = Comp.1, y = Comp.2), size = 0.3, alpha = 0.5, colour = "grey20") +
  geom_point(data = extir, aes(x = Comp.1, y = Comp.2), size = 1.5, alpha = 0.8, colour = "brown2") +
  
  # add arrows
  geom_segment(data = loadings_sc, aes(x = 0, y = 0, xend = Comp.1, yend = Comp.2), arrow = arrow(length = unit(0.2, "cm")), colour = "black") +
  # probability kernels
  geom_contour(aes(z = value), breaks = cl_50_1911, colour = "grey30", size = 1) +
  geom_contour(aes(z = value), breaks = cl_95_1911, colour = "grey60", size = 1) +
  geom_contour(aes(z = value), breaks = cl_99_1911, colour = "grey70", size = 1) +
  coord_equal() +
  # add arrows
  geom_segment(data = loadings_sc, aes(x = 0, y = 0, xend = Comp.1, yend = Comp.2), arrow = arrow(length = unit(0.2, "cm")), colour = "black") +
  # add dashed arrows ends
  geom_segment(data = loadings_sc, aes(x = 0, y = 0, xend = -Comp.1, yend = -Comp.2), lty = 5, colour = "darkgrey") +
  # add arrow labels
  geom_text_repel(data = loadings_sc, aes(x = Comp.1, y = Comp.2, label = trait), size = 4, nudge_x = 11, hjust = 0.5, direction = "y", segment.size = 0.5,segment.color = "grey89") +
  # axis labels - see comp_var
  labs(x = "PC1 - Body size (55%)", y = "PC2 - Dispersal ability (13%)") +
  xlim(-5,15) +
  ylim(-8,8) +
  # edit plot
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill='white', colour = "black"),
        axis.text = element_text(colour = "black"),
        legend.position = "none",
        text = element_text(size = 25)) + 
  geom_text(x= 5, y=8, label="1910s", color="black", size = 5)

# display plot
windows()
pca_plot_1982


############Thermal tolerance data cleaning

thermal<-read.csv("GlobalTherm_1.csv")
thermal2<-read.csv("GlobalTherm_2.csv")
species_list<-read.csv("species_list.csv")

thermal_species<-inner_join(thermal, species_list, by = "species")

View(thermal_species)

write.csv(thermal_species, "thermal_toleraces_selected_species.csv")


###End of script
