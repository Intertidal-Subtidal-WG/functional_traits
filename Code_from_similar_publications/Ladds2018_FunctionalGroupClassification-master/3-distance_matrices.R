rm(list=ls())


#docDir <- "C://Users/laddsmo/Victoria University of Wellington - STAFF/OneDrive - Victoria University of Wellington - STAFF/Papers_Projects/TropicGroupClassification/"
#source("R/data_prep.R") 
source("0-functions.R")

library(cluster) #for calculating gower distance
library(dplyr)   #for cleaning data
library(clusterCrit)  #for validation
#library(flexclust)  #for calculating rand index
library(nomclust) ##for making distance matrices
library(ggplot2)
library(reshape)
library(ggthemes) #for exporting into latex
library(fpc) #for bootstrapping
#library(caret)
library(gridExtra)

##list of the possible internal validation measures
nomclustEvals <- c("WCM", "WCE", "PSTau", "PSU", "PSFM", "PSFE")
#List of the possible linkage methods
clusterMethod <- c("average", "complete", "single")


#Colours for the different distance matrices
group.colours <- c("Goodall" = "black",
                   "Goodall_2" = "#CCCCCC",
                   "Goodall_3" = "#999999",
                   "Goodall_4" = "#666666",
                   "Eskin"="blue",
                   "IOF"="orange", 
                   "OF" = "#D55E00",
                   "Lin"="purple",
                   "Lin_1" = "#9999CC",
                   "Simple matching"="darkgreen")

subDist <- c("Goodall","Eskin", "IOF", "Lin", "Simple matching")

######

##read in data##
load("./data/fish_imputed.RData")
ClusterImp <- droplevels(ClusterImp)
#Remove Pelvic_finPosition
ClusterImp <- select(ClusterImp,-Pelvic_finPosition)

##-----------Cluster using the "nomclust" package------------------##

##Make a NULL dataset for the output from "nomclust"
combdf <- combn(ClusterImp$CommonName,2)
comparison<-data.frame(combdf[1,], combdf[2,])
colnames(comparison) <- c("c1", "c2")

#Select the variables to use from the imputed data
dataSET <- ClusterImp[,c(1:21)]
namesComm <- ClusterImp[,"CommonName"]

evaluation <- NULL
cmin = 2
cmax = 25


fit_nomclust <- function(distanceMethod,clusterMethod, cmin, cmax)
{nomclust(dataSET, measure = distanceMethod, 
          method = clusterMethod,
          clu.low = cmin, clu.high = cmax)}


# #heirachical clustering#
 for(i in 1:length(clusterMethod))  {
# 
#   ##---Goodall 1
# 
#   #fit the model
   good_fit<-fit_nomclust(good1,clusterMethod[i], cmin, cmax)
   comparison<-groupfits(good_fit,"good",clusterMethod[i])
# 
#   #Extract the validation metrics
   good_eval <- melt(good_fit$eval, id.vars = "cluster")
   good_eval$distance <- "Goodall"
   good_eval$method <-  clusterMethod[i]
# 
#   ##---Goodall 2
# 
   good2_fit<-fit_nomclust(good2,clusterMethod[i], cmin, cmax)
   comparison<-groupfits(good2_fit,"good2",clusterMethod[i])
# 
#   #Extract the validation metrics
  good2_eval <- melt(good_fit$eval, id.vars = "cluster")
   good2_eval$distance <- "Goodall_3"
  good2_eval$method <-  clusterMethod[i]
# 
#   ##---Goodall 3
#   good3_fit<-fit_nomclust(good2,clusterMethod[i], cmin, cmax)
#   comparison<-groupfits(good2_fit,"good2",clusterMethod[i])
# 
#   #Extract the validation metrics
#   good3_eval <- melt(good_fit$eval, id.vars = "cluster")
#   good3_eval$distance <- "Goodall_3"
#   good3_eval$method <-  clusterMethod[i]
# 
# 
#   ##---Goodall 4
# 
#   good4_fit<-fit_nomclust(good4,clusterMethod[i], cmin, cmax)
#   comparison<-groupfits(good4_fit,"good2",clusterMethod[i])
# 
#   #Extract the validation metrics
#   good4_eval <- melt(good_fit$eval, id.vars = "cluster")
#   good4_eval$distance <- "Goodall_4"
#   good4_eval$method <-  clusterMethod[i]
# 
#   ##---Eskin
# 
#   eskin_fit <- fit_nomclust(eskin,clusterMethod[i], cmin, cmax)
#   comparison<-groupfits(eskin_fit,"eskin",clusterMethod[i])
# 
#   eskin_eval <- melt(eskin_fit$eval, id.vars = "cluster")
#   eskin_eval$distance <- "Eskin"
#   eskin_eval$method <- clusterMethod[i]
# 
# 
#   #--IOF
# 
#   iof_fit <- fit_nomclust(iof,clusterMethod[i], cmin, cmax)
#   comparison<-groupfits(iof_fit,"iof",clusterMethod[i])
# 
#   iof_eval <- melt(iof_fit$eval, id.vars = "cluster")
#   iof_eval$distance <- "IOF"
#   iof_eval$method <- clusterMethod[i]
# 
#   #--OF
# 
#   of_fit <- fit_nomclust(of,clusterMethod[i], cmin, cmax)
#   comparison<-groupfits(of_fit,"of",clusterMethod[i])
# 
#   of_eval <- melt(of_fit$eval, id.vars = "cluster")
#   of_eval$distance <- "OF"
#   of_eval$method <- clusterMethod[i]
# 
# 
#   #--Lin
# 
#   lin_fit <- fit_nomclust(lin,clusterMethod[i], cmin, cmax)
#   comparison<-groupfits(lin_fit,"lin",clusterMethod[i])
# 
#   lin_eval <- melt(lin_fit$eval, id.vars = "cluster")
#   lin_eval$distance <- "Lin"
#   lin_eval$method <- clusterMethod[i]
# 
# 
#   #--Lin1
# 
#   lin1_fit <- fit_nomclust(lin1,clusterMethod[i], cmin, cmax)
#   comparison<-groupfits(lin1_fit,"lin",clusterMethod[i])
# 
#   lin1_eval <- melt(lin1_fit$eval, id.vars = "cluster")
#   lin1_eval$distance <- "Lin_1"
#   lin1_eval$method <- clusterMethod[i]
# 
# 
#   #--Simple matching
# 
#   sm_fit <- fit_nomclust(sm,clusterMethod[i], cmin, cmax)
#   comparison<-groupfits(sm_fit,"sm",clusterMethod[i])
# 
#   sm_eval <- melt(sm_fit$eval, id.vars = "cluster")
#   sm_eval$distance <- "Simple matching"
#   sm_eval$method <- clusterMethod[i]
# 
   evaluation <- rbind(evaluation, good_eval, good2_eval,
#                       good3_eval, good4_eval,eskin_eval,
#                       iof_eval, of_eval, lin_eval,
#                       lin1_eval, sm_eval
)
# 
 }

save(evaluation, file = "output/evaluation_2-25.RData")


######
load("output/evaluation.RData")
evaluation$distance <- as.factor(evaluation$distance)
evaluation$value_scaled <- evaluation$value/max(evaluation$value)

##------------ VALIDATION

## Validation from the nomclust package internal measures. 
#Sulc (2016) thesis; pg. 31


#Plot and export the different measures  
for(i in 1:length(nomclustEvals)){
  EvalData <- evaluation[evaluation$variable==nomclustEvals[i],]
  EvalPlot <- plotEvals(EvalData, nomclustEvals[i])
  ggsave(EvalPlot, device = "pdf",
         filename = paste0("output/nomclust_evaluation/", nomclustEvals[i],"_plot.pdf"))
}


###PSFE - higher value indicates better grouping.
EvalData <- evaluation[evaluation$variable==nomclustEvals[6]&!evaluation$cluster==1,]

##If you want to subset further
EvalData <- EvalData[!EvalData$cluster==2&EvalData$distance %in% subDist,]

EvalPlotPSFE <- ggplot(EvalData, 
                       aes(x = cluster, y = value, color = factor(distance)))+
  geom_point()+geom_line()+
  theme_base()+
  ylab("PSFE")+
  xlab("Clusters")+
  scale_x_discrete(limits = c(3:25))+
  facet_grid(variable~method)+
  #scale_y_continuous(limits = c(lowylim,highylim))+
  scale_color_manual(values = group.colours)+
  theme(panel.grid.minor.x	= element_line(color = "grey"),
        panel.grid.major.x	= element_line(color = "grey"),
        panel.grid.major.y	= element_line(color = "grey"),
        #axis.text.x = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.87, 0.68), # c(0,0) bottom left, c(1,1) top-right.
        legend.background = element_rect(fill = "white", colour = NA),
        plot.background = element_blank())

ggsave(EvalPlotPSFE, filename = paste0(docDir,"documents/figures/EvalPlotPSFE.pdf"),
       width = 9, height = 4)


###PSMF - higher value indicates better grouping.
EvalData <- evaluation[evaluation$variable==nomclustEvals[5]&!evaluation$cluster==1,]

EvalData <- EvalData[!EvalData$cluster==2&EvalData$distance %in% subDist,]
EvalData$distance <- droplevels(EvalData$distance)
EvalPlotPSFM <- ggplot(EvalData, 
                       aes(x = cluster, y = value, color = factor(distance)))+
  geom_point()+geom_line()+
  theme_base()+
  ylab("PSFM")+
  xlab("Clusters")+
  scale_x_discrete(limits = c(3:10))+
  facet_grid(variable~method)+
  #scale_y_continuous(limits = c(lowylim,highylim))+
  scale_color_manual(values = group.colours)+
  theme(panel.grid.minor.x	= element_line(color = "grey"),
        panel.grid.major.x	= element_line(color = "grey"),
        panel.grid.major.y	= element_line(color = "grey"),
        #axis.text.x = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.87, 0.78), # c(0,0) bottom left, c(1,1) top-right.
        plot.background = element_blank())

ggsave(EvalPlotPSFM, filename = paste0(docDir,"documents/figures/EvalPlotPSFM.pdf"), 
       width = 9, height = 4)


####-----------Choosing the best number of clusters and distance

##--WCE
EvalData <- evaluation[evaluation$variable==nomclustEvals[2]&!evaluation$cluster==1,]
EvalData$value_scaled <- rbind(0,data.frame(abs(diff(EvalData$value))))
EvalData$value_scaled <- unlist(EvalData$value_scaled)
EvalData$PSFE <- evaluation[evaluation$variable==nomclustEvals[6]&!evaluation$cluster==1,3]
WCE_score <- EvalData[EvalData$cluster==2,3]
EvalData <- 
  EvalData[!EvalData$cluster==2,] %>%
  group_by(distance, method) %>%
  mutate(my_ranks = order(order(PSFE, decreasing=TRUE)))
write.csv(EvalData,"hacking.csv")
EvalData <- read.csv("hacking.csv")

EvalData <- EvalData[!EvalData$cluster==2&EvalData$distance %in% subDist,]


group.values <- c("1" = "#D55E00", "2" = "#000000", "3" = "#333333", 
                  "4" = "#666666", "5" = "#999999", "6" = "#CCCCCC",
                  "7" = "#CCCCCC", "8" = "#CCCCCC")


WCE_results <- ggplot(EvalData, aes(fill = as.factor(my_ranks)))+
  geom_bar(aes(x = cluster, y = value_scaled), stat = "identity")+
  facet_grid(method~distance)+
  theme_base()+
  ylab("WCE difference")+
  xlab("Clusters")+
  #geom_text()+
  #annotate(WCE_score[1], x=1, y=1)+
  scale_x_discrete(limits = c(3:25))+
  #scale_fill_manual(values = group.values)+
  theme(panel.grid.minor.x	= element_line(color = "grey"),
        panel.grid.major.x	= element_line(color = "grey"),
        panel.grid.major.y	= element_line(color = "grey"),
        #axis.text.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        plot.background = element_blank())

ggsave(WCE_results, filename = paste0(docDir, "documents/figures/WCE_results.pdf"), 
       width = 13, height = 8)

##---WCM
##Repeat with mutability instead of entropy
EvalData <- evaluation[evaluation$variable==nomclustEvals[1]&!evaluation$cluster==1,]
EvalData$value_scaled <- rbind(0,data.frame(abs(diff(EvalData$value))))
EvalData$value_scaled <- unlist(EvalData$value_scaled)
EvalData$PSFM <- evaluation[evaluation$variable==nomclustEvals[5]&!evaluation$cluster==1,3]
EvalData <- 
  EvalData[!EvalData$cluster==2,] %>%
  group_by(distance, method) %>%
  mutate(my_ranks = order(order(PSFM, decreasing=TRUE)))
write.csv(EvalData,"hacking.csv")
EvalData <- read.csv("hacking.csv")

EvalData <- EvalData[!EvalData$cluster==2&EvalData$distance %in% subDist,]

group.values <- c("1" = "#D55E00", "2" = "#000000", "3" = "#333333", 
                  "4" = "#666666", "5" = "#999999", "6" = "#CCCCCC",
                  "7" = "#CCCCCC", "8" = "#CCCCCC")


WCM_results <- ggplot(EvalData, aes(fill = as.factor(my_ranks)))+
  geom_bar(aes(x = cluster, y = value_scaled), stat = "identity")+
  facet_grid(method~distance)+
  theme_base()+
  ylab("WCM difference")+
  xlab("")+
  scale_x_discrete(limits = c(3:10))+
  scale_fill_manual(values = group.values)+
  theme(panel.grid.minor.x	= element_line(color = "grey"),
        panel.grid.major.x	= element_line(color = "grey"),
        panel.grid.major.y	= element_line(color = "grey"),
        #axis.text.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "none")

ggsave(WCM_results, filename = paste0(docDir,"documents/figures/WCM_results.pdf"),
       width = 12, height = 8)


##within-cluster entropy coefficient (WCE) - lower value is better (more homogeneous)
EvalData <- evaluation[evaluation$variable==nomclustEvals[2]&!evaluation$cluster==1,]
EvalData <- EvalData[!EvalData$cluster==2&EvalData$distance %in% subDist,]

EvalPlotWCE <- ggplot(EvalData[!EvalData$cluster==2,], 
                      aes(x = cluster, y = value, color = distance))+
  geom_point()+geom_line()+
  theme_base()+
  ylab("WCE")+
  xlab("")+
  scale_x_discrete(limits = c(3:10))+
  facet_grid(.~method)+
  #scale_y_continuous(limits = c(lowylim,highylim))+
  scale_color_manual(values = group.colours)+
  theme(panel.grid.minor.x	= element_line(color = "grey"),
        panel.grid.major.x	= element_line(color = "grey"),
        panel.grid.major.y	= element_line(color = "grey"),
        #axis.text.x = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.8,0.3),
        plot.background = element_blank())

ggsave(EvalPlotWCE, filename = paste0(docDir,"documents/figures/EvalPlotWCE.pdf"), 
       width = 9, height = 4)

##---WCM

EvalData <- evaluation[evaluation$variable==nomclustEvals[1]&!evaluation$cluster==1,]
EvalData <- EvalData[!EvalData$cluster==2&EvalData$distance %in% subDist,]

EvalPlotWCM <- ggplot(EvalData[!EvalData$cluster==2,], 
                      aes(x = cluster, y = value, color = distance))+
  geom_point()+geom_line()+
  theme_base()+
  ylab("WCM")+
  xlab("")+
  scale_x_discrete(limits = c(3:10))+
  facet_grid(.~method)+
  #scale_y_continuous(limits = c(lowylim,highylim))+
  scale_color_manual(values = group.colours)+
  theme(panel.grid.minor.x	= element_line(color = "grey"),
        panel.grid.major.x	= element_line(color = "grey"),
        panel.grid.major.y	= element_line(color = "grey"),
        #axis.text.x = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.8,0.3),
        plot.background = element_blank())

ggsave(EvalPlotWCM, filename = paste0(docDir,"documents/figures/EvalPlotWCM.pdf"), 
       width = 9, height = 4)

##PSU and PSTau - almost exact opposite of above - don't really need to use

EvalPlotPSU <- ggplot(evaluation)+
  geom_bar(data = evaluation[evaluation$variable==nomclustEvals[4],], aes(x = cluster, y = value), stat = "identity")+
  facet_grid(method~distance)+
  theme_base()+
  ylab("PSU")+
  xlab("")+
  scale_x_discrete(limits = c(2:10))+
  #scale_color_brewer(evaluation[,evaluation$variable==nomclustEvals[5]],)
  #  facet_grid(.~method)+
  #scale_y_continuous(limits = c(lowylim,highylim))+
  scale_color_manual(values = group.colours)+
  theme(panel.grid.minor.x	= element_line(color = "grey"),
        panel.grid.major.x	= element_line(color = "grey"),
        panel.grid.major.y	= element_line(color = "grey"),
        #axis.text.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "top",
        plot.background = element_blank())

EvalData <- evaluation[evaluation$variable==nomclustEvals[3]&!evaluation$cluster==1,]
EvalPlotPSTau <- ggplot(EvalData, 
                        aes(x = cluster, y = value, color = distance))+
  geom_point()+geom_line()+
  theme_base()+
  facet_grid(.~method)+
  ylab("PSTau")+
  xlab("")+
  scale_x_discrete(limits = c(2:10))+
  #scale_y_continuous(limits = c(lowylim,highylim))+
  scale_color_manual(values = group.colours)+
  theme(panel.grid.minor.x	= element_line(color = "grey"),
        panel.grid.major.x	= element_line(color = "grey"),
        panel.grid.major.y	= element_line(color = "grey"),
       # axis.text.x = element_blank(),
        legend.title = element_blank(),
        legend.position = c())


#ggsave(nomClust_evalPlot, "output/nomclust_evaluation/nomClust_evalPlot.pdf",device = "pdf")




###----bootstrap the clusters to find the most stable
#https://www.r-bloggers.com/bootstrap-evaluation-of-clusters/


output <- NULL

#The first loop can have as many clusters as you want to test [j]
for(j in c(seq(3,9,2))){
  stabilities <- NULL
  
  #this loop runs the bootstrapping procedure - 
  #at the moment it runs over the 10 distance matrices available from nomclust.
  
  #The options for clustermethod are:
  #claraCBI
  #pamkCBI
  #distnoisemclustCBI
  #disttrimkmeansCBI
  
    for(i in 1:length(distanceMatrices)){
    cboot.mod <- clusterboot(distanceMatrices[[i]], B=100, distances = TRUE, 
                             bootmethod = "boot", showplots = FALSE,
                             k = j, clustermethod = pamkCBI)
       # The vector of cluster stabilities. 
    # Values close to 1 indicate stable clusters
    stabilities <- rbind(stabilities,cboot.mod$bootmean)
    new.stabs <- melt(stabilities)
    new.stabs$clusts <- j
    }
  output <- rbind(output,new.stabs)
  
}

##Name the distance matrices
  output$X1 <- rep(1:10) 
#Name the columns
  colnames(output) <- c("Distance","Clusters","value","clusts")
#Label and factorise the distance variable
  output$Distance <- factor(output$Distance, labels = c("Goodall","Goodall 2","Goodall 3", "Goodall 4",
                                                      "Eskin","OF" ,"IOF", "Lin", "Lin 1","SM"))

##save the output for each bootstrapping procedure type
save(output, file= "output/stability_analysis/stability_distnoisemclustCBI.RData")

load("output/stability_analysis/stability_pamkCBI.RData")
subDist <- c("Goodall","Eskin","IOF","Lin","SM")
output$clusts <- as.factor(output$clusts)
##Create a summary table
sumStab <- output[output$Distance %in% subDist,] %>% group_by(clusts,Distance) %>%
  summarise(average = mean(value),
            minV = min(value),
            maxV = max(value),
            sdV = sd(value))

###If you want to just plot one cluster number
#PlotData <- output[output$clusts==5,]


##Plot everything as a bar chart
stabPlot <- ggplot(output[output$Distance %in% subDist,], aes(x = Clusters, y = value))+
                  geom_bar(stat = "identity")+
                  facet_grid(clusts~Distance)+
                  geom_hline(data = sumStab, aes(yintercept = average))+
                  theme_base()+
                  ylab("Cluster stabilities")+
                  xlab("Clusters")+
                  scale_x_discrete(limits = seq(1,9,2))+
                  scale_y_continuous(limits = c(0,1))+
                  scale_color_manual(values = group.colours)+
                  theme(#panel.grid.minor.x	= element_line(color = "grey"),
                        panel.grid.major.x	= element_blank(),
                        legend.title = element_blank(),
                        #axis.text.x = element_blank(),
                        axis.ticks.x = element_blank(),
                        plot.background = element_blank())

ggsave(stabPlot, file = "output/stability_analysis/stabBar_pamkCBI.pdf",
       device = "pdf")

stabPlot <- ggplot(sumStab, aes(x = clusts, y = average))+
                    geom_point()+
                    geom_errorbar(aes(ymin = minV, ymax = maxV))+
                    facet_grid(.~Distance)+
                    theme_base()+
                    ylab("Cluster stabilities")+
                    xlab("Clusters")+
                    scale_x_discrete(labels = seq(3,9,2))+
                    scale_y_continuous(limits = c(0,1))+
                    scale_color_manual(values = group.colours)+
                    theme(panel.grid.minor.x	= element_line(color = "grey"),
                      panel.grid.major.x	= element_line(color = "grey"),
                      legend.title = element_blank(),
                      #axis.text.x = element_blank(),
                      axis.ticks.x = element_blank(),
                      plot.background = element_blank())

ggsave(stabPlot, file = paste0(docDir,"documents/figures/stab_errorBar_pamkCBI.pdf"),
       device = "pdf", width = 7.5, height = 4)


#--------select number of clusters using sil-width

##------Make distance matrices
distanceMatrices <- list(good1(ClusterImp[,1:21]),good2(ClusterImp[,1:21]),
                         good3(ClusterImp[,1:21]),good4(ClusterImp[,1:21]),
                         eskin(ClusterImp[,1:21]),iof(ClusterImp[,1:21]), 
                         of(ClusterImp[,1:21]),lin(ClusterImp[,1:21]),
                         lin1(ClusterImp[,1:21]),sm(ClusterImp[,1:21]))

#select number of clusters using sil-width
ClusterNo <- sil_width(distanceMatrices[[1]])

#pdf("output/SilhouetteWidth.pdf", width = 3.729, height =  2.7324)
png("output/SilhouetteWidth.png", width = 30, height =  15)

plot(1:10, ClusterNo[1:10],
     xlab = "Number of clusters",pch=19,xlim = c(2,10),
     ylab = "Silhouette Width", ylim = c(0,0.4))
lines(1:10, ClusterNo[1:10])
points(1:10, sil_width(distanceMatrices[[2]])[1:10],col="#CCCCCC",pch=19)
lines(1:10, sil_width(distanceMatrices[[2]])[1:10],col="#CCCCCC")
points(1:10, sil_width(distanceMatrices[[3]])[1:10],col="#999999",pch=19)
lines(1:10, sil_width(distanceMatrices[[3]])[1:10],col="#999999")
points(1:10, sil_width(distanceMatrices[[4]])[1:10],col="#666666",pch=19)
lines(1:10, sil_width(distanceMatrices[[4]])[1:10],col="#666666")
points(1:10, sil_width(distanceMatrices[[5]])[1:10],col="blue",pch=19)
lines(1:10, sil_width(distanceMatrices[[5]])[1:10],col="blue")
points(1:10, sil_width(distanceMatrices[[6]])[1:10],col="orange",pch=19)
lines(1:10, sil_width(distanceMatrices[[6]])[1:10],col="orange")
points(1:10, sil_width(distanceMatrices[[7]])[1:10],col="#D55E00",pch=19)
lines(1:10, sil_width(distanceMatrices[[7]])[1:10],col="#D55E00")
points(1:10, sil_width(distanceMatrices[[8]])[1:10],col="purple",pch=19)
lines(1:10, sil_width(distanceMatrices[[8]])[1:10],col="purple")
points(1:10, sil_width(distanceMatrices[[9]])[1:10],col="#9999CC",pch=19)
lines(1:10, sil_width(distanceMatrices[[9]])[1:10],col="#9999CC")
points(1:10, sil_width(distanceMatrices[[10]])[1:10],col="darkgreen",pch=19)
lines(1:10, sil_width(distanceMatrices[[10]])[1:10],col="darkgreen")

# legend(8, 0.5, legend = c("Goodall","Eskin", "IOF", "Lin", "SM"), 
#        fill = c("black", "blue", "orange", "purple", "green"))
dev.off()


