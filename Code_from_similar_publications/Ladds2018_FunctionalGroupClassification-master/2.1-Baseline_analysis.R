rm(list = ls())
source("Code/functions.R")
#read in data
fishes <- read.csv("Data/fishes.csv" , na.strings = c("","NA"))

#######Changing continuous variables to nominal
fishes$trophic_bin <- cut(fishes$Trophic_level,breaks = c(-Inf,3,3.5,4,Inf),
                          labels = c("Low", "Medium", "High", "VHigh"))
fishes$maxDepth_bin <- cut(fishes$max_depth,breaks = c(-Inf, 50,200,500,1000, Inf),
                           labels = c("Reef", "Shallow", "Ocean", "Deep", "Bathy"))
fishes$CommaxDepth_bin <- cut(fishes$DepthRangeComDeep_F,breaks = c(-Inf, 50,200,500, Inf),
                              labels = c("Reef", "Shallow", "Ocean", "Deep"))
fishes$maxLength_bin <- cut(fishes$maxLength,breaks = c(-Inf, 15,50,100,250,500, Inf),
                            labels = c("Tiny","VSmall", "Small", "Medium", "Large", "VLarge"))


    ################################################################################
### ------------------  Baseline missing data imputation  ----------------------------###
    ################################################################################

library(missForest) #for creating missing values
library(compareDF)
library(dplyr)

##Test the accuracy of the imputing method Mode


##---------Baseline Imputation--------------##
#create a dataset with known missing values
change_output <- NULL


#run a simulation on different missing data - change the proportion of missing data
#something about this is broken and I don't understand it well enough to know what
for(i in seq(0.05,0.45,0.05)){
  
replicate_BASE <- replicate(100, simulate_BASE(dataSET[,2:14],i))
save(replicate_BASE,file=paste0("output/Baseline/data/Baseline_accuracy_",i,
                                "missing.RData"))

png(filename = paste0("output/Baseline/histograms/Baseline_accuracy_",i,"missing.png"), 
    height = 10, width = 12, res=300,units = "cm")
hist(replicate_BASE, main = "", xlab = "Accuracy", cex = 2, breaks = 7, xlim = c(0,1), 
     ylim = c(0,30))
dev.off()
}

##use the mode to impute the data

fish <- fishes[fishes$Occurance_TBGB=="RAR"|fishes$Occurance_TBGB=="RES",
               c("group_name",
                 "Scientific_name",
                 "CommonName",
                 "Class",
                 "Order",
                 "Family",
                 #"Reference",
                 #"IUCN_class",
                 "Occurance_TBGB",
                 "Diet",
                 "trophic_bin",
                 "CommaxDepth_bin",
                 "maxDepth_bin",
                 #"DepthRangeComShallow_F",
                 #"DepthRangeComDeep_F",
                 "Temp_type",
                 "Vertical_Habitat",
                 "Horizontal_Habitat",
                 #"Estuarine_use",
                 "Pelvic_finPosition",
                 "Caudal_finShape",
                 #"Dorsal_finShape",
                 "Swimming_mode",
                 "Body_form",
                 "Eye_position",
                 "Oral_gapePosition",
                 #"Spine",
                 #"Colour",
                 "maxLength_bin",
                 #"Reproductive_Season",
                 "Reproductive_strategy",
                 "Sexual_differentation",
                 "Migration",
                 "Parental_care",
                 "Egg_attach",
                 "Reproduction_location",
                 "Shooling_type",
                 "pop_double")]


##Fill in NA with Mode of each variable
for(var in 1:ncol(fish)){
fish[is.na(fish[,var]),var] <- mode(fish[,var])
}
round(apply(fish, 2, count_NAs), 2)
ClusterImp <- fish
save(ClusterImp, file = "./data/Baseline_imputed.RData")

