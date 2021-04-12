rm(list = ls())
source("0-functions.R")
#read in data
fishes <- read.csv("./data/fishes.csv", na.strings = c("","NA")) #<- this is the key i was missing

#######Changing continuous variables to nominal
fishes$trophic_bin <- cut(fishes$Trophic_level,breaks = c(-Inf,3,3.5,4,Inf),
                          labels = c("Low", "Medium", "High", "VHigh"))
fishes$maxDepth_bin <- cut(fishes$max_depth,breaks = c(-Inf, 50,200,500,1000, Inf),
                           labels = c("Reef", "Shallow", "Ocean", "Deep", "Bathy"))
fishes$CommaxDepth_bin <- cut(fishes$DepthRangeComDeep_F,breaks = c(-Inf, 50,200,500, Inf),
                              labels = c("Reef", "Shallow", "Ocean", "Deep"))
fishes$maxLength_bin <- cut(fishes$maxLength,breaks = c(-Inf, 15,50,100,250,500, Inf),
                            labels = c("Tiny","VSmall", "Small", "Medium", "Large", "VLarge"))



###Missing data imputation
library(mice) #for imputation
library(missForest) #for creating missing values
library(compareDF)
library(dplyr)

##Test the accuracy of the imputing method missMDA

##Create a dataset with no missing values
dataSET <- fishes[fishes$Occurance_TBGB=="RAR"|fishes$Occurance_TBGB=="RES",
                  c("Scientific_name",
                    "Occurance_TBGB",
                    "Diet",
                    "maxDepth_bin",
                    "Temp_type",
                    "Vertical_Habitat",
                    "Horizontal_Habitat",
                    "Pelvic_finPosition",
                    #"Caudal_finShape",
                    #"Dorsal_finShape",
                    "Swimming_mode",
                    "Body_form",
                    "Eye_position",
                    "Oral_gapePosition",
                    "Colour",
                    "maxLength_bin")]
#Check there are no missing values
dataSET <- droplevels(dataSET)
round(apply(dataSET, 2, count_NAs), 2)
summary(dataSET)

#create a dataset with known missing values
change_output <- NULL

#run a simulation on different missing data - change the proportion of missing data
for(i in seq(0.05,0.45,0.05)){
  
replicate_MICE <- replicate(100, simulate_MICE(dataSET[,2:14],i))
save(replicate_MICE,file=paste0("output/MICEpolyreg/data/MICEpolyreg_accuracy_",i,
                                "missing.RData"))

png(filename = paste0("output/MICEpolyreg/histograms/MICEpolyreg_accuracy_",i,"missing.png"), 
    height = 10, width = 12, res=300,units = "cm")
hist(replicate_MICE, main = "", xlab = "Accuracy", cex = 2, breaks = 7, xlim = c(0,1), 
     ylim = c(0,30))
dev.off()
}

##use the package to impute the data

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
                 #"Pelvic_finPosition",
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

glimpse(fish)
#character must be factor
fish[sapply(fish, is.character)] <- lapply(fish[sapply(fish, is.character)], 
                                       as.factor)

fish <- fish[-c(50),]

#this gave me a lot of logged events, look into this later
#removing diet helps, change column 8 to column 9
cluster.imp <- mice(fish[,9:ncol(fish)], m=5, method = 'polyreg', print = F)
ClusterImp <- complete(cluster.imp, 1)
ClusterImp <- cbind(fish[,1:7],ClusterImp)
save(ClusterImp, file = "./data/MICE_imputed.RData")

#to see logged events
#cluster.imp$loggedEvents

