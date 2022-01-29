###############################################################################
###############################################################################
## Proyect Working group CIEE
## R-code sub-group functional traits
## 
#### last update:Nov 2021
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
install.packages("TPD") #Methods for Measuring Functional Diversity Based on Trait Probability Density
install.packages("Gifi") # for PCA on categorical data
install.packages("FactoMineR")

#libraries 
library(pacman)
pacman::p_load(dplyr, plyr, readr, tbible, FD, ade4, cowplot, mice, reshape2, tidyr, ks, hypervolume, alphallhu, purrr, TTR, plotrix, agricolae, psych)

library(ggplot2)
library(data.table)
library(extrafont)
library(visreg)
library(lubridate)
library(letsR)
library(reshape)
library(reshape2)
library(funrar)#to calculate functional uniqueness
library(factoextra)
library(ggrepel)
library(tibble)
library(tidyverse)

library(Gifi)# to do PCA on categorical data
library(TPD)# 
library(FactoMineR)
#library(ts)

loadfonts()


# Load data ---------------------------------------------------------------

traits<-read.csv("20210730_functional_traits_marine.csv",stringsAsFactors=FALSE) # the last version of the data
#traits_algae_imputed<-read.csv("20210806_algae_imputed.csv")
traits_algae_imputed<-read.csv("20220128_1982-1995_algae.csv")

#traits_animal_imputed<-read.csv("20210806_animal_imputed.csv")
traits_animal_imputed<-read.csv("20220128_1982-1995_animal.csv")

#traits_modified_algae<-read.csv("traits_mod_algae.csv") #traits modified in algae converted to ordinal values as suggested in "gifi" package
#traits_modified_animals<-read.csv("traits_modified_animals.csv")

#Matrices traits similarity
matrix_algae<-read.csv("20210806_algae_matrix.csv")

View(matrix_algae)
matrix_algae <- as.matrix(matrix_algae)
matrix_animals<-read.csv("20210806_animal_matrix.csv")
matrix_animal <- as.matrix(matrix_animals)
#View(matrix_algae)

##Matrices presence absence per year
matrix<-read.csv("species_year_matrix.csv")

##Abundance data
abundances<-read.csv("relative_abund_cover.csv") ## It probably wll be better to have this from the count data

###### MCA by year
algae_final<-read.csv("1.trait_algae_year_abun.csv")
animal_final<-read.csv("1.trait_animals_year_abun.csv")

# Creating matrix_species per year ----------------------------------------
species_year<-read.csv("spp_present_per_year_intertidal.csv", head=TRUE)
str(species_year)
#species_year$year<-as.character(species_year$year)
species_year <-species_year%>%mutate(value=1) 
#other way of doing it
species_year<-species_year %>%group_by(year,species) %>% rename(value=`n()`)
summarise(Total=sum(value))
species_year_matrix<-pivot_wider(species_year, names_from = year, values_from=value)
species_year_matrix<-species_year_matrix[is.na( species_year_matrix)] <- 0
View(species_year_matrix)
#write.csv(species_year_matrix, "species_year_matrix.csv")
str(species_year)
#matrix<-read<-("species_year_matrix.csv")



#Multivariate analyses

# MCA Multiple correspondence analyses -------------------------------------
####Alternative using MCA Multiple correspondence analyses
###
#https://rpubs.com/gaston/MCA
#http://rstudio-pubs-static.s3.amazonaws.com/472227_23cd8fc6608740df8619a1db47a434c9.html
#The plot below shows the relationship between the points.
#Similar rows are grouped together
#Negatively correlated rows are plotted on the oposite sides
#The distance from the orgin represents the quality of row points on the factor map.
#The quality of represtation is measured by squared cosine (cos2) which measures the contribution of variables to the two dimensional plot.
#The highest cos2, the better. Variables with low cos2 should be treated and interpreted with caution.

####
require(FactoMineR)
require(ggplot2)

# MCA_all_years_together ---------------------------------------------------
str(traits_algae_imputed) 
str(traits_animal_imputed)

####
#MCA_algae
####
str(traits_algae_imputed)
traits_algae_imputed_sel<-traits_algae_imputed[,4:11] #select columns of interest
cats=apply(traits_algae_imputed_sel, 2, function(x) nlevels(as.factor(x)))
cats

str(traits_algae_imputed_sel)
traits_algae_imputed_sel<-traits_algae_imputed_sel %>% mutate_at(vars(turf_subcanopy_canopy_algae,steneck_dethier_morphology_algae,body_size_avg_bin,benthic,epibiotic,intertidal,subtidal), list(as.factor)) 

#MCA
mca2 = MCA (traits_algae_imputed_sel, graph = TRUE)
summary (mca2)
mca2$eig
mca2$var
mca2$ind

variables_scores_algae<-mca2$var$eta2 
str(variables_scores_algae) 
head(mca2$var$coord)
mca2
plot(mca2)

str(traits_algae_imputed_sel)

#dimdesc(mca2)
######## Variable needed for the plot extracted from mca2
mca2_vars_df = data.frame(mca2$var$coord, Variable = rep(names(cats), 
                                                         cats))
mca2_obs_df = data.frame(mca2$ind$coord)

View(mca2_vars_df )

# plot of variable categories
ggplot(data = mca2_vars_df, aes(x = Dim.1, y = Dim.2, label = rownames(mca2_vars_df))) + 
  geom_hline(yintercept = 0, colour = "gray70") + geom_vline(xintercept = 0, 
                                                             colour = "gray70") + geom_text(aes(colour = Variable)) + ggtitle("Algae_MCA plot of variables using R package FactoMineR")

# MCA plot of observations and categories [Plot used for the presentation!]
ggplot(data = mca2_obs_df, aes(x = Dim.1, y = Dim.2)) + geom_hline(yintercept = 0, 
                                                                   colour = "gray70") + geom_vline(xintercept = 0, colour = "gray70") + geom_point(colour = "gray50", 
                                                                                                                                                   alpha = 0.7) + geom_density2d(colour = "gray80") + geom_text(data = mca2_vars_df, 
                                                                                                                                                                                                                aes(x = Dim.1, y = Dim.2, label = rownames(mca2_vars_df), colour = Variable)) + 
  ggtitle("Algae, MCA plot of variables using R package FactoMineR") + scale_colour_discrete(name = "Variable")+
  geom_segment(data = mca2_vars_df, aes(x = 0, y = 0, xend = Dim.1, yend = Dim.2), arrow = arrow(length = unit(0.2, "cm")), colour = "black") +
  theme_bw(20)+ xlab ("  Dim 1 Morphology, Body size  15.45%")+ ylab(" Body size-small 13 %")


plotellipses(mca2,keepvar=c(2:11))


###
#ANIMALS
###

traits_animal_imputed_sel<-traits_animal_imputed[,5:15] #select columns of interest
cats=apply(traits_animal_imputed_sel, 2, function(x) nlevels(as.factor(x)))
cats
#trait_animal_imputed_sel<-trait_animal_imputed_sel %>% mutate_at(vars(body_size_avg_bin, morphology1,dietary_pref_c,trophic_level, motility_juv, motility_adult, benthic,epibiotic, invasive, intertidal, subtidal), list(as.factor)) 

#MCA
mca1 = MCA (traits_animal_imputed_sel, graph = TRUE)
summary (mca1)
mca1$eig
variables_scores_animal<-mca1$var$eta2
write.csv(variables_scores_animal, "animal_variable_scores.csv")
str(variables_scores_animal)
head(mca1$var$coord)
mca1
plot(mca1)

#dimdesc(mca1)
######## Variable needed for the plot extracted from mca1
mca1_vars_df = data.frame(mca1$var$coord, Variable = rep(names(cats), 
                                                         cats))
mca1_obs_df = data.frame(mca1$ind$coord)

#####MCA plot
# plot of variable categories
ggplot(data = mca1_vars_df, aes(x = Dim.1, y = Dim.2, label = rownames(mca1_vars_df))) + 
  geom_hline(yintercept = 0, colour = "gray70") + geom_vline(xintercept = 0, 
                                                             colour = "gray70") + geom_text(aes(colour = Variable)) + ggtitle("animals MCA plot of variables using R package FactoMineR")


# MCA plot of observations and categories [Plot used for the presentation!]
ggplot(data = mca1_obs_df, aes(x = Dim.1, y = Dim.2)) + geom_hline(yintercept = 0, 
                                                                   colour = "gray70") + geom_vline(xintercept = 0, colour = "gray70") + geom_point(colour = "gray50", 
                                                                                                                                                   alpha = 0.7) + geom_density2d(colour = "gray80") + geom_text(data = mca1_vars_df, 
                                                                                                                                                                                                                aes(x = Dim.1, y = Dim.2, label = rownames(mca1_vars_df), colour = Variable)) + 
  ggtitle("Animals, MCA plot of variables using R package FactoMineR") + scale_colour_discrete(name = "Variable")+
  geom_segment(data = mca1_vars_df, aes(x = 0, y = 0, xend = Dim.1, yend = Dim.2), arrow = arrow(length = unit(0.2, "cm")), colour = "black") +
  theme_bw(20)+ xlab ("  Dim 1 ")+ ylab(" Dim 2")


  #plot individual variables  in the multispace
  plotellipses(mca1,keepvar=c(2:18))

#Extract particular variables of interest

a<-plotellipses(mca1, keepvar =4,graph.type="ggplot")
a+geom_density2d(colour = "red")
a + geom_hline(yintercept = 0,  colour = "gray70")+  geom_vline(xintercept = 0, colour = "gray70") + geom_point(colour = "gray50", alpha = 0.7) + geom_density2d(colour = "red") + ggtitle("animals MCA plot of variables using R package FactoMineR") + scale_colour_discrete(name = "Variable")+
  theme_bw(20) + xlab(" Body size + Trophic level+ motility (18%) ") + ylab("Diet+Morphology")


###########ANALYSES BY YEAR################
###########MCA BY YEAR 

#Lets extract the abundance for years of interest

abundances<-read.csv("relative_abund_cover.csv") ## It probably wll be better to have this from the count data

# 1983 seems to be a weird year so talking to Kylla she suggested that 1985 will be of more interest
str(abundances)
year1985_abun<-abundances %>% filter(year==1985) %>% select(species, rel_abund_by_side, position) 
year1985_abun<-rename(year1985_abun, Historical.Abundance=rel_abund_by_side)

str(year1982_abun)

year2020_abun<- abundances %>% filter(year==2020) %>% select(species, rel_abund_by_side, position) 
year2020_abun<-rename(year2020_abun, Current.Abundance=rel_abund_by_side)
View(year2020_abun)


#### The traits data with score

scores_total_animal_mca<-read.csv("scores_total_animal_mca.csv")
scores_total_algae_mca<-read.csv("scores_total_algae_mca.csv")

## the matrix with the years

species_year_matrix<-read.csv("species_year_matrix.csv")
View(species_year_matrix)

######MCA by year

###Animal
scores_total_animal_mca_years<-left_join(scores_total_animal_mca, species_year_matrix, by="species")
write.csv(scores_total_animal_mca_years, "scores_total_animal_mca_years.csv")
scores_total_animal_mca_years_abun<-inner_join(scores_total_animal_mca_years,year2017_abun, by="species")
scores_total_animal_mca_years_abun<-inner_join(scores_total_animal_mca_years_abun,year1982_abun, by="species")
str(scores_total_animal_mca_years_abun)
write.csv(scores_total_animal_mca_years_abun, "1.trait_animals_year_abun.csv")

#Algae
scores_total_algae_mca_years<-left_join(scores_total_algae_mca, species_year_matrix, by="species")
write.csv(scores_total_algae_mca_years, "scores_total_animal_mca_years.csv")
scores_total_algae_mca_years_abun<-inner_join(scores_total_algae_mca_years,year2017_abun, by="species")
scores_total_algae_mca_years_abun<-inner_join(scores_total_algae_mca_years_abun,year1982_abun, by="species")
str(scores_total_algae_mca_years_abun)
write.csv(scores_total_algae_mca_years_abun, "1.trait_algae_year_abun.csv")


###### MCA by year 

###Year 1982

algae_final<-read.csv("1.trait_algae_year_abun.csv")
animal_final<-read.csv("1.trait_animals_year_abun.csv")

#Algae subtidal and intertidal
str(algae_final)
algae_final_1982<-algae_final %>% filter(X1995==1)

#algae
algae_final_sel_1982<-algae_final_1982[,15:21] #select columns of interest
cats=apply(algae_final_sel_1982, 2, function(x) nlevels(as.factor(x)))
cats

algae_final_sel_1982<-algae_final_sel_1982 %>% mutate_at(vars(body_size_avg_bin, morphology1,dietary_pref_c,trophic_level, motility_juv, motility_adult, benthic,epibiotic, invasive, intertidal, subtidal), list(as.factor)) 

#MCA
mca1 = MCA (algae_final_sel_1982, graph = TRUE)
summary (mca1)
mca1$eig
variables_scores_algae<-mca1$var$eta2
#write.csv(variables_scores_algae, "algae_variable_scores.csv")
str(variables_scores_algae)
head(mca1$var$coord)
mca1
plot(mca1)

#dimdesc(mca1)

######## Variable needed for the plot extracted from mca1

mca1_vars_df = data.frame(mca1$var$coord, Variable = rep(names(cats), 
                                                         cats))
mca1_obs_df = data.frame(mca1$ind$coord)

mca1_vars_df

#####MCA plot

# plot of variable categories
ggplot(data = mca1_vars_df, aes(x = Dim.1, y = Dim.2, label = rownames(mca1_vars_df))) + 
  geom_hline(yintercept = 0, colour = "gray70") + geom_vline(xintercept = 0, 
                                                             colour = "gray70") + geom_text(aes(colour = Variable)) + ggtitle("algaes MCA plot of variables using R package FactoMineR")

# MCA plot of observations and categories
ggplot(data = mca1_obs_df, aes(x = Dim.1, y = Dim.2)) + geom_hline(yintercept = 0, 
                                                                   colour = "gray70") + geom_vline(xintercept = 0, colour = "gray70") + geom_point(colour = "gray50", 
                                                                                                                                                   alpha = 0.7) + geom_density2d(colour = "gray80") + geom_text(data = mca1_vars_df, 
                                                                                                                                                                                                                aes(x = Dim.1, y = Dim.2, label = rownames(mca1_vars_df), colour = Variable)) + 
  ggtitle(" Algae year 1982 using R package FactoMineR") + scale_colour_discrete(name = "Variable")+
  geom_segment(data = mca1_vars_df, aes(x = 0, y = 0, xend = Dim.1, yend = Dim.2), arrow = arrow(length = unit(0.2, "cm")), colour = "black") +
  theme_bw(20)+ xlab ("  Dim 1 ")+ ylab(" Dim 2")
