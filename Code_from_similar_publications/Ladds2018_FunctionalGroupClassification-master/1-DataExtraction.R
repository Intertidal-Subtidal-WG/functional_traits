rm(list = ls())
source("0-functions.R")

require(rfishbase)

data("fishbase")

my_fish <- read.csv("fish_list.csv",header = F, stringsAsFactors = F)
validation <- validate_names(my_fish)
validation

##Home page data
tbgb_home <- lapply(my_fish, species)
tbgb_home <- as.data.frame(tbgb_home$V1)

write.csv(tbgb_home, "data/home_fishbase.csv")


##Ecology
tbgb_ecol <- lapply(my_fish, ecology)

tbgb_ecol <- as.data.frame(tbgb_ecol)

round(apply(tbgb_ecol, 2, count_NAs), 2)

write.csv(tbgb_ecol, "data/ecology_fishbase.csv")



#new_zealand <- species_list("New Zealand", "distribution", ~fishbase)
