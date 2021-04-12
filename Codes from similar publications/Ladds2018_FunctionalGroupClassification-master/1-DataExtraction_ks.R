rm(list = ls())
source("0-functions.R")

require(rfishbase)

data("fishbase")

my_fish <- read.csv("./data/fishes.csv",header = T, stringsAsFactors = F)
validation <- validate_names(my_fish$Scientific_name)
validation

##Home page data
tbgb_home <- species(my_fish$Scientific_name)
#i don't know what this is doing or why. 
#something here is the remnants of the lapply workflow
#tbgb_home2 <- as.data.frame(tbgb_home$V1) 

write.csv(tbgb_home, "data/home_fishbase.csv")


##Ecology
tbgb_ecol <- ecology(my_fish$Scientific_name)

tbgb_ecol <- as.data.frame(tbgb_ecol)

#looks like this is counting NAs? potentially very helpful in the future
round(apply(tbgb_ecol, 2, count_NAs), 2)

write.csv(tbgb_ecol, "data/ecology_fishbase.csv")



#new_zealand <- species_list("New Zealand", "distribution", ~fishbase)
