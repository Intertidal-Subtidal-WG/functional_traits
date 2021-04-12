rm(list = ls())
source("0-functions.r")

library(dplyr)  #for data cleaning
library(caret)  ## create a full set of dummy variables (binary categorical variables)
###Missing data imputation
library(mice) #for imputation


#clustering package options
#library(fpc) #flexible processes for clustering
#library(vegan)
#library(ecodist)

  #read in data
  all_fishes <- read.csv("data/fishes.csv", na.strings = c("","NA"))
  #row.names(fishes) <- fishes$CommonName
  
  #extract fish that occur in TBGB
  fishes <- all_fishes[all_fishes$Occurance_TBGB=="RAR"|all_fishes$Occurance_TBGB=="RES",]
  
  
  ###------------------------Discretization------------------------------###
  
  #######Changing continuous variables to nominal
  #tropic level
  fishes$trophic_bin <- cut(fishes$Trophic_level,breaks = c(-Inf,3,3.5,4,Inf),
                            labels = c("Low", "Medium", "High", "VHigh"))
  
  #maximum depth
  fishes$maxDepth_bin <- cut(fishes$max_depth,breaks = c(-Inf, exp(3), exp(4), exp(5), exp(6), Inf),
                             labels = c("Reef", "Shallow", "Ocean", "Deep", "Bathy"))
  
  #common maximum depth
  fishes$CommaxDepth_bin <- cut(fishes$DepthRangeComDeep_F,
                                breaks = c(-Inf, exp(3), exp(4), exp(5), exp(6), Inf),
                                labels = c("Reef", "Shallow", "Ocean", "Deep", "Bathy"))
  
  #maximum length
  fishes$maxLength_bin <- cut(fishes$maxLength, breaks = c(-Inf, exp(3), exp(4), exp(5), Inf),
                              labels = c("Small", "Medium", "Large", "VLarge"))
  
  


  ##----------------- Data Checking ----------------------##

    #check the completenesss of the data i.e. how many missing? 
    #>20% of cases missing don't include
    116*.25
    #[1] 29
    
    ##---- split into integer and factor variables

        #--first factor variables
        is.fact <- sapply(fishes, is.factor)
        fishes_fact <- fishes[, is.fact]
        
        round(apply(fishes_fact, 2, count_NAs), 2)
    
        #--second numerical values
        fishes_num <- fishes[, !is.fact]
        
        round(apply(fishes_num, 2, count_NAs), 2)

    

    ##----------------- Selecting Variables ----------------------##
    
    ##choose variables to cluster species by
    ##including all variables with less than 20% missing
        
    #Raw data
        fish <- fishes[, c("group_name",
                         "Scientific_name",
                         "CommonName",
                         "Class",
                         "Order",
                         "Family",
                         #"Distribution",
                         #"IUCN_class",
                         "Occurance_TBGB",
                         "Diet",
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
                         #"Reproductive_Season",
                         "Reproductive_strategy",
                         "Sexual_differentation",
                         "Migration",
                         "Parental_care",
                         "Egg_attach",
                         "Reproduction_location",
                         "Shooling_type",
                         "pop_double",
                         "maxLength_bin",
                         "trophic_bin",
                         "CommaxDepth_bin",
                         "maxDepth_bin")]

  ###----------------Missing data imputation------------------------##

        ##### Make sure your variables that need imputation are factors
        fish$pop_double <- as.factor(fish$pop_double)
        fish$Reproduction_location <- as.factor(fish$Reproduction_location)
        fish$Migration <- as.factor(fish$Migration)
        fish$Shooling_type <- as.factor(fish$Shooling_type)
        fish$Parental_care <- as.factor(fish$Parental_care)
        
        #all at once
        fish[,8:28] <- lapply(fish[,8:28], factor)
        
        
#Use MICE to impute missing data
  #cluster.imp <- mice(fish[,8:29], m=5, method = 'polyreg', print = F)
  cluster.imp <- mice(fish[,9:29], m=5, method = 'polyreg', print = F)
  ClusterImp <- complete(cluster.imp)
  ClusterImp <- cbind(ClusterImp,fish[,1:7])
  
  save(ClusterImp, file = "data/fish_imputed.RData")
     
  
#the fact this is getting many events seems to mean it is overparameterized.     
cluster.imp$imp
glimpse(fish)
        
head(cluster.imp$loggedEvents, 2)

outlist4 <- as.character(cluster.imp$loggedEvents[, "out"])
outlist4