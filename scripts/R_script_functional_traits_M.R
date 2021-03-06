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

# Load data ---------------------------------------------------------------

marine_traits<-read.csv("marine_traits.csv")

str(marine_traits)

#Scale the data if needed
scale_traits <- function(x){
  (x - mean(x, na.rm=TRUE))/sd(x, na.rm=TRUE)
}

traits_scaled<-marine_traits %>% 
  mutate(body_size_scale=scale_traits(body_size))%>%
  as.data.frame

traits_scaled<-traits_scaled %>%
  column_to_rownames(var = "species")

# my data frame should only have species name as row names  and morphological traits that I want  as columns
traits_scaled<-traits_scaled %>% 
  select(species, body_size_scale)%>% 
  column_to_rownames(var = "species")

#PCA

pca_community <- princomp(traits_scaled, cor = TRUE, scores = TRUE)
summary(pca_community)

plot(pca_community)


ggplot(pca_community, aes(x = Comp.1, y = Comp.2))


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


############Thermal tolerance data cleaning

###End of script
