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
install.packages("TPD") #Methods for Measuring Functional Diversity Based on Trait Probability Density
install.packages("Gifi") # for PCA on categorical data

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

library(Gifi)# to do PCA on categorical data
library(TPD)
#library(ts)

loadfonts()


# Load data ---------------------------------------------------------------

traits<-read.csv("20210730_functional_traits_marine.csv",stringsAsFactors=FALSE) # the last version of the data

traits_algae_imputed<-read.csv("algae_imputed.csv")
trait_animal_imputed<-read.csv("animal_imputed.csv")
traits_modified<-read.csv("traits_mod.csv") #traits converted to ordinal values as suggested in "gifi" package

View(traits)
View(traits_modified)

#Matrices

matrix_algae<-read.csv("20210606_algae_matrix.csv")
View(matrix_algae)
matrix_algae <- as.matrix(matrix_algae)
matrix_animals<-read.csv("20210606_animal_matrix.csv")
matrix_animal <- as.matrix(matrix_animals)


#Compared with the data from Gomez and from Cooke we have categorical and numerical data in out traits.
#One solution is to use Gifi package: Multivariate Analysis with Optimal Scaling
#Implements categorical principal component analysis ('PRINCALS'), multiple correspondence analysis ('HOMALS'), monotone regression analysis ('MORALS'). It replaces the 'homals' package.

#install.packages("Gifi")
#library(Gifi)


# Functional space --------------------------------------------------------

# If I leave the variables as nominal they do not work, 
traits_algae_imputed_1<-traits_algae_imputed[,4:11] #restric the variables to traits that are informative (e.g species and class are removed)
traits_algae_imputed_1<-as.data.frame(traits_algae_imputed_1)
#recode(traits_algae_imputed1$morphology1)
#princals(traits_algae_imputed_1,ordinal=FALSE)
#View(traits_algae_imputed_1)


# 1.1 Ordinal PCA ---------------------------------------------------------
# With the animal data base I convert them to ordinal numbers, note that this do not imply that we are doing a regular pca, we are specifying that we are using ordinal variables
#Some interesting blog about PCA interpretation : http://strata.uga.edu/8370/lecturenotes/principalComponents.html and decisions on number of components to be extracted

View(traits_modified) # File with variables transformed to ordinal numbers
traits_modified_1<-traits_modified[,2:6] #select columns of interest

animals<-princals(traits_modified_1,ordinal=TRUE,ndim = 3) #PCA on ordinal data ndim= number of components to be extracted, should be less than the number of variables
summary(animals)

View(animals)

plot(animals, plot.type = "transplot")
plot(animals, "loadplot", main = "Loadings Plot ABC Data")  ## aspect ratio = 1
plot(animals, "biplot", labels.scores = TRUE, main = "Biplot ABC Data")
plot(animals, "screeplot")


#Extract the score for each species, I think object score is the way to do it but will be interesting to underestand what scoremat means.

#library (tidyverse)
scoresanimals <- as.data.frame(animals$objectscores) %>% 
  tibble::rownames_to_column("species")

# convert long to wide
#tidyr::gather(key, value, -species) %>% 
#tidyr::unite(col, key) %>% 
#tidyr::spread(col, value) 

#TRying to combine this two but I can cause the column species was removed from one and no the other
#scores_totals_animals<-inner_join(traits_modified,scoresanimals, by = "species")
#traits_modified
#write.csv(scoresanimals, file = "scoreanimals.csv", row.names = FALSE)


# I combined then manually
TotalPCA_scores<-read.csv("traits_mod_1.csv")
View(TotalPCA_scores)

# PCA over time periods -----------------------Still no working----------------------------


#Subset for species in each period (Need to define the lenght of this periods)

#Y1982 <- traits_scaled %>% 
  filter(Y1982 == 1) %>% 
  select(species, body_size_scale, dietart_preference, thropic_level) %>% 
  left_join(scoresPCATotal, by = "species")

#Y2002 <- traits_scaled %>% 
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


#  3. TPD (trait probability density functions) ---------------------------

#####################################################################################
#Methods for Measuring Functional Diversity Based on Trait Probability Density
#Tools to calculate trait probability density functions (TPD) at any scale (e.g. populations, species, communities). 
#TPD functions are used to compute several indices of functional diversity, as well as its partition across scales

library(TPD)

#Create new database for community TPD to compare extirpated, new additions and share specieswith Total PCA scores
TotalPCA<- TotalPCA_scores %>% 
  mutate(SD = 1) %>%    #agregamos una variable SD
  select(species,Comp.1, Comp.2, Comp.3,id,POP,Historical.Abundance, Current.Abundance, SD) #POP = groups of species, A = Extirpated, B = Novel additions, C = Shared species

#PC1
TRA <- matrix(c(TotalPCA$Comp.1), ncol = 1)
SD <- matrix(c(TotalPCA$SD), ncol=1)
POP <- TotalPCA$POP
ABUN <- TotalPCA %>% 
  select(species, Current.Abundance,POP) %>% 
  pivot_wider(names_from = species, values_from = Current.Abundance) %>% 
  column_to_rownames('POP')

ABUN[is.na(ABUN)] <- 0

library(TPD)
tpdmean<- TPDsMean(species = TotalPCA$species, means = TRA, sds=SD, alpha = 1, samples = POP)

TPDc <- TPDc(TPDs = tpdmean, sampUnit = ABUN )

sapply(TPDc$TPDc$TPDc, sum)

plotTPD(TPD = TPDc, nRowCol = c(3,3))


##Graphs of TPDs (trait probability density) for PC1, 2 and 3

#Multiply PC2 and 3 by -1 to ease interpretation of increasing values.
TotalPCA1 <- TotalPCA %>% 
  mutate(Comp2M = Comp.2 *-1) %>% 
  mutate(Comp3M = Comp.3 *-1)

#PC1
windows()
p = ggplot(TotalPCA1, aes(x = Comp.1,fill = POP))
p = p + geom_density(alpha = 0.6) + scale_fill_manual(name = "POP", values = c("orange", "grey", "blue")) 
p = p + theme_bw(20) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text=element_text(size=20),axis.title=element_text(size=20), axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")))
p <- p + xlab("PC1 - Body size (55%) ") + ylab("Trait prbability density") + ggtitle(" ")
p = p + theme(legend.position = c(0.75, 0.9))+ geom_text(x = 1, y = 0.40, label = " p = 0.04" , size = 6)+
  scale_fill_manual(values=c("orange", "grey", "blue"), 
                    name=NULL,breaks=c("A", "B", "C"),labels=c("Extirpated species", "Novel additions", "Shared species"))

p

#####

## Still Trying to underestand this 
#Density comparisons with kolmogorov-smirnov test
library("sm")

A <- subset(TotalPCA1, TotalPCA1$POP == "A")
B <- subset(TotalPCA1, TotalPCA1$POP == "B")
C <- subset(TotalPCA1, TotalPCA1$POP == "C")

#KS test
ks.test(A$Comp.1, B$Comp.1)#P = 0.04

ks.test(A$Comp.2, B$Comp.2)# P = 0.0008

ks.test(A$Comp.3, B$Comp.3)# P = 0.003



###Example from the package to calculate PCA with categorical data
ABC
ABC6 <- ABC[,6:11]

fitord <- princals(ABC6) 

View (ABC6)

## ordinal PCA
fitord <- princals(ABC6,ndim =6)  ## ordinal PCA
fitord
summary(fitord)
plot(fitord, plot.type = "transplot")
plot(fitord, "loadplot", main = "Loadings Plot ABC Data")  ## aspect ratio = 1
plot(fitord, "biplot", labels.scores = TRUE, main = "Biplot ABC Data")
plot(fitord, "screeplot")



scoresfitord <- as.data.frame(fitord$scoremat) %>% 
  tibble::rownames_to_column("species")

## linear restrictions (mimics standard PCA)
abc_knots <- knotsGifi(ABC6, "E")     ## 0 interior knotsx
fitlin <- princals(ABC6, knots = abc_knots, degrees = 1)
fitlin
summary(fitlin)
fitlin$evals
plot(fitlin, plot.type = "transplot")
## compare with standard PCA
ABCnum <- makeNumeric(ABC6)
fitpca <- prcomp(ABCnum, scale = TRUE)
fitpca$sdev^2

summary(fitpca)
## End(Not run)

###### 


