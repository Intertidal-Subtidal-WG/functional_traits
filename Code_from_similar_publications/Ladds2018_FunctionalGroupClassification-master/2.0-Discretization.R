rm(list = ls())
source("0-functions.R")
#docDir <- "C://Users/laddsmo/Victoria University of Wellington - STAFF/OneDrive - Victoria University of Wellington - STAFF/Papers_Projects/TropicGroupClassification/"

#read in data
fishes <- read.csv("data/fishes.csv" , na.strings = c("","NA"))

library(Hmisc)  ##for cutting variables into quintiles (cut2)
library(discretization)  ##for cutting data and retaining the original distribution (mdlp)
##code for if using these libraries:
#--fishes$maxLength_quin <- cut2(fishes$maxLength,g=5)
#--cut_fish <- mdlp(num_fish)$Disc.data
#--barchart(fishes$maxLength_quin)

library(ggplot2)
library(ggthemes)
library(grid)
library(gridExtra)

is.num <- sapply(fishes, is.numeric)
small_fish <- fishes[fishes$Occurance_TBGB=="RAR"|fishes$Occurance_TBGB=="RES",is.num]


################################################################################
### ------------------  Discretization  ----------------------------###
################################################################################

#Changing continuous variables to nominal

## --------------------- Trophic Level ---------------------------------##

#Raw data
hist(small_fish$Trophic_level)  

small_fish <- small_fish[order(small_fish$Trophic_level),]
plot(small_fish$Trophic_level)

  #Make the cuts
small_fish$trophic_bin <- cut(small_fish$Trophic_level,breaks = c(-Inf,3,3.5,4,Inf),
                              labels = c("Low", "Medium", "High", "VHigh"))

scaleFUN <- function(x) sprintf("%.0f", x)
#---Plot it
trop_binPlot <- ggplot(small_fish[complete.cases(small_fish$trophic_bin),], aes(trophic_bin))+
  geom_bar()+
  scale_y_continuous("", expand = c(0,0), limits = c(0,52))+
  scale_x_discrete("Trophic category")+
  theme_classic(base_size = 12)+
  annotate("text", x=0.65, y=45, label = "b)", size = 5)+
  theme(axis.text.x = element_text(color = 'black', size = 12),
        axis.text.y = element_text(color = 'black', size = 12))

trop_levPlot <- ggplot(small_fish[complete.cases(small_fish$Trophic_level),], aes(Trophic_level))+
  geom_histogram(binwidth = 0.5)+
  scale_y_continuous("Count", expand = c(0,0), limits = c(0,52), labels = scaleFUN)+
  scale_x_continuous("Trophic level")+
  theme_classic(base_size = 12)+
  annotate("text", x=1.75, y=45, label = "a)", size = 5)+
  theme(axis.text.x = element_text(color = 'black', size = 12),
        axis.text.y = element_text(color = 'black', size = 12))

#Plot one inside the other 
#somethign about this is broken but its just a figure so idgaf for now
pdf("trophic_discretize.pdf",width = 12, height = 10)
vp <- viewport(width = 0.38, height = 0.38, x = 0.275, y = 0.65)
print(trop_levPlot)
print(trop_binPlot, vp = vp)
dev.off()



## --------------------- MAX DEPTH ---------------------------------##

#Plot the raw data
hist(log(small_fish$max_depth), breaks = 5)

small_fish <- small_fish[order(small_fish$max_depth),]
plot(small_fish$max_depth)

#Log the data
small_fish$LogMax_depth <- log(small_fish$max_depth)

#Make the cuts
small_fish$maxDepth_bin <- cut(small_fish$max_depth,breaks = c(-Inf, exp(3), exp(4), exp(5), exp(6), Inf),
                               labels = c("Reef", "Shallow", "Ocean", "Deep", "Bathy"))

#---Plot it
depth_binPlot <- ggplot(small_fish[complete.cases(small_fish$maxDepth_bin),], 
                        aes(maxDepth_bin))+
  geom_bar()+
  scale_y_continuous("", expand = c(0,0), limits = c(0,52))+
  scale_x_discrete("Maximum depth category")+
  theme_classic(base_size = 12)+
  annotate("text", x=0.65, y=45, label = "d)", size = 5)+
  theme(axis.text.x = element_text(color = 'black', size = 12),
        axis.text.y = element_text(color = 'black', size = 12))

depth_levPlot <- ggplot(small_fish[complete.cases(small_fish$max_depth),], aes(LogMax_depth))+
  geom_histogram(binwidth = 1)+
  scale_y_continuous("Count", expand = c(0,0), limits = c(0,52), labels = scaleFUN)+
  scale_x_continuous("Depth (m)", breaks = c(1,2, 3, 4, 5, 6, 7, 8),
                     labels = c(round(exp(1),1),round(exp(2),1),
                                round(exp(3),1),round(exp(4),1),
                                round(exp(5),1),round(exp(6),1),
                                round(exp(7),1),round(exp(8),1)))+  
  annotate("text", x=0.5, y=45, label = "c)", size = 5)+
  theme_classic(base_size = 12)+
  theme(axis.text.x = element_text(color = 'black', size = 12),
        axis.text.y = element_text(color = 'black', size = 12))

#Plot one inside the other
pdf("max_depth_discretize.pdf",width = 12, height = 10)
vp <- viewport(width = 0.38, height = 0.38, x = 0.28, y = 0.7)
print(depth_levPlot)
print(depth_binPlot, vp = vp)
dev.off()

## --------------------- COMMON DEPTH ---------------------------------##

#RAW DATA
hist(log(small_fish$DepthRangeComDeep_F), breaks = 5)

small_fish <- small_fish[order(small_fish$DepthRangeComDeep_F),]
plot(small_fish$DepthRangeComDeep_F)


#Log the data
small_fish$LogMax_depthCom <- log(small_fish$DepthRangeComDeep_F)

#Make the cuts
small_fish$CommaxDepth_bin <- cut(small_fish$DepthRangeComDeep_F,
                                  breaks = c(-Inf, exp(3), exp(4), exp(5), exp(6), Inf),
                                  labels = c("Reef", "Shallow", "Ocean", "Deep", "Bathy"))

#---Plot it
comDepth_binPlot <- ggplot(small_fish[complete.cases(small_fish$CommaxDepth_bin),], 
                           aes(CommaxDepth_bin))+
  geom_bar()+
  scale_y_continuous("", expand = c(0,0), limits = c(0,52))+
  scale_x_discrete("Common depth category")+
  theme_classic(base_size = 12)+
  annotate("text", x=0.65, y=45, label = "f)", size = 5)+
  theme(axis.text.x = element_text(color = 'black', size = 12),
        axis.text.y = element_text(color = 'black', size = 12))

comDepth_levPlot <- ggplot(small_fish[complete.cases(small_fish$LogMax_depthCom),], 
                           aes(LogMax_depthCom))+
  geom_histogram(binwidth = 1)+
  scale_y_continuous("Count", expand = c(0,0), limits = c(0,52))+
  scale_x_continuous("Depth (m)", breaks = c(1,2, 3, 4, 5, 6, 7, 8),
                     labels = c(round(exp(1),1),round(exp(2),1),
                                round(exp(3),1),round(exp(4),1),
                                round(exp(5),1),round(exp(6),1),
                                round(exp(7),1),round(exp(8),1)))+
  annotate("text", x=0.5, y=45, label = "e)", size = 5)+
  theme_classic(base_size = 12)+
  theme(axis.text.x = element_text(color = 'black', size = 12),
        axis.text.y = element_text(color = 'black', size = 12))

#Plot one inside the other 
pdf("Commax_depth_discretize.pdf",width = 12, height = 10)
vp <- viewport(width = 0.38, height = 0.38, x = 0.275, y = 0.8)
print(comDepth_levPlot)
print(comDepth_binPlot, vp = vp)
dev.off()

## --------------------- LENGTH ---------------------------------##

#Raw data plot
hist(log(fishes$maxLength))
#Log the data
small_fish$Log_length <- log(small_fish$maxLength)

small_fish <- small_fish[order(small_fish$maxLength),]
plot(small_fish$maxLength)

##make the categories
small_fish$maxLength_bin <- cut(small_fish$maxLength, breaks = c(-Inf, exp(3), exp(4), exp(5), Inf),
                                labels = c("Small", "Medium", "Large", "VLarge"))

#---Plot it
Length_binPlot <- ggplot(small_fish[complete.cases(small_fish$maxLength_bin),], 
                         aes(maxLength_bin))+
  geom_bar()+
  scale_y_continuous("Count", expand = c(0,0), limits = c(0,52))+
  scale_x_discrete("Length category")+
  theme_classic(base_size = 12)+
  annotate("text", x=0.65, y=45, label = "h)", size = 5)+
  theme(axis.text.x = element_text(color = 'black', size = 12),
        axis.text.y = element_text(color = 'black', size = 12))

Length_levPlot <- ggplot(small_fish[complete.cases(small_fish$Log_length),], 
                         aes(Log_length))+
  geom_histogram(binwidth = 1)+
  scale_y_continuous("Count", expand = c(0,0))+
  scale_x_continuous("Length (cm)", breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8),
                     labels = c(round(exp(0),1),
                                round(exp(1),1),round(exp(2),1),
                                round(exp(3),1),round(exp(4),1),
                                round(exp(5),1),round(exp(6),1),
                                round(exp(7),1),round(exp(8),1)))+
  annotate("text", x=-0.5, y=45, label = "g)", size = 5)+
  theme_classic(base_size = 12)+
  theme(axis.text.x = element_text(color = 'black', size = 12),
        axis.text.y = element_text(color = 'black', size = 12))

#Plot one inside the other 
pdf("Length_discretize.pdf",width = 12, height = 10)
vp <- viewport(width = 0.38, height = 0.38, x = 0.275, y = 0.8)
print(Length_levPlot)
print(Length_binPlot, vp = vp)
dev.off()


discretize_plot <- grid.arrange(trop_levPlot, trop_binPlot,
             depth_levPlot, depth_binPlot,
             comDepth_levPlot, comDepth_binPlot,
             Length_levPlot, Length_binPlot,
             ncol = 2)
ggsave(discretize_plot, device = "pdf", width = 10.27, height = 11.69, units = "in",
       filename = "discretize_plot.pdf")
