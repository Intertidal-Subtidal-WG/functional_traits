#run data_prep.R first
rm(list = ls())
source("0. functions.R")

docDir <- "C://Users/laddsmo/Victoria University of Wellington - STAFF/OneDrive - Victoria University of Wellington - STAFF/Papers_Projects/TropicGroupClassification/"


library(ggplot2) 
library(ggthemes)
library(ggdendro)
library(dplyr)   #for cleaning data
library(nomclust)  #for sil width
library(dplyr)
library(caret)  ##for calculating sensitivity and specificity
library(entropy)  #for calculating rand index
library(data.table)
library(reshape2)
library(Rtsne) # for t-SNE plotlibrary(optCluster)
library(gridExtra)

#read in data
load("data/fish_imputed.RData")
fishes <- droplevels(ClusterImp)
row.names(ClusterImp) <- ClusterImp$Scientific_name

ClusterImp <- droplevels(ClusterImp)
ClusterImp <- ClusterImp[,-c(5)]

##Clustering with mixed data types 
  dataSET <- ClusterImp[,1:21]

  distanceMethod <- c("Goodall","Goodall 2","Goodall 3", "Goodall 4",
                      "Eskin","OF" ,"IOF", "Lin", "Lin 1","SM")
  
  
  distanceMatrices <- list(good1(ClusterImp[,1:21]),good2(ClusterImp[,1:21]),
                           good3(ClusterImp[,1:21]),good4(ClusterImp[,1:21]),
                           eskin(ClusterImp[,1:21]),iof(ClusterImp[,1:21]), 
                           of(ClusterImp[,1:21]),lin(ClusterImp[,1:21]),
                           lin1(ClusterImp[,1:21]),sm(ClusterImp[,1:21]))
  
  #rownames(distanceMatrices[[1]]) <- fishes$CommonName

#shows the most similar and most dissimilar pair of observations
  similar(distanceMatrices[[8]],fishes)
  dissimilar(distanceMatrices[[8]], fishes)


  ###------------Heirachical clustering

#define options for heirarchcal clustering 
  methodsClust <- c("single", "complete", "average")
  
  #build the model and select the number of clusters
  dissMatrix <- as.dist(distanceMatrices[[8]])   ##Change distance matrix
  final_fit <- hclust(dissMatrix, method = methodsClust[3])  #change cluster method
  clust <- cutree(final_fit, 3)  # change number of clusters
  
  #make into dendro data for plotting
  dendr <- dendro_data(final_fit, type="rectangle") 
  
  #add labels
  labs <- label(dendr)
  labs$label <- as.numeric(as.character(labs$label))
  labs <- labs[order(labs$label),]
  labs$label <- ClusterImp$Scientific_name
  labs$cluster <- as.factor(clust)
  
  ##---PLOT IT! Goodall/Lin with 3 groups
  nominalDendrogram <- ggplot() + 
    geom_segment(data=segment(dendr), aes(x=x, y=y, xend=xend, yend=yend)) + 
    geom_text(data=labs, aes(x=x, y=y, label=label, hjust=0, color = labs$cluster), size=2) +
    coord_flip() + scale_y_reverse(expand=c(0.1, 0), limits = c(1.1,-0.07)) + 
    scale_x_discrete(expand=c(0.01, 0))+
    expand_limits(c(0,0))+
    scale_colour_manual(values=c( "#E69F00", "#009999", "#FF6633", "#0072B2", "#D55E00","#999999", "#CC0033","#009E73")) +
    theme_base()+
    #rect.dendrogram(k=9,horiz=TRUE)+
    geom_hline(yintercept = .93, color = "red")+   #for goodall
    #geom_hline(yintercept = .83, color = "red")+
    theme(axis.line.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          panel.background=element_rect(fill="white"),
          panel.grid=element_blank(),
          panel.border = element_blank(),
          legend.title = element_blank(),
          legend.position = "none",
          plot.background = element_blank())
  
  nominalDendrogram
  
  ggsave(nominalDendrogram, filename = paste0(docDir,"documents/figures/dendrogram_LinAverage3.pdf"),
        device = "pdf", height = 8.27, width = 11.27)
  
  ##---PLOT IT! - Eskin with 9 groups
  nominalDendrogram <- ggplot() + 
    geom_segment(data=segment(dendr), aes(x=x, y=y, xend=xend, yend=yend)) + 
    geom_text(data=labs, aes(x=x, y=y, label=label, hjust=0, color = labs$cluster), size=2) +
    coord_flip() + scale_y_reverse(expand=c(0.1, 0), limits = c(0.9,-0.05)) + 
    scale_x_discrete(expand=c(0.01, 0))+
    expand_limits(c(0,0))+
    scale_colour_manual(values=c( "#E69F00", "#009999", "#FF6633", "#0072B2", "#D55E00","#999999", "#CC0033","#009E73")) +
    theme_base()+
    #rect.dendrogram(k=9,horiz=TRUE)+
    geom_hline(yintercept = .73, color = "red")+
    #geom_hline(yintercept = .53, color = "red")+
    theme(axis.line.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          panel.background=element_rect(fill="white"),
          panel.grid=element_blank(),
          legend.title = element_blank(),
          legend.position = "none",
          legend.background = element_blank())
  
  ggsave(nominalDendrogram, filename = "documents/figures/dendrogram_LinAverage8com.pdf",
         device = "pdf")
  
  #---Plot the best fitting models from the evaluation stage
  
  ##---A good way to plot the clusters
  #http://dpmartin42.github.io/blogposts/r/cluster-mixed-types
  
  #The function to do the plotting
  tsneFunction <- function(tsne, diss, fit, clusts, labs, xmin, ymax){
    tsne_data <- tsne$Y %>%
      data.frame() %>%
      setNames(c("X", "Y")) %>%
      mutate(cluster = factor(fit),   ##Change the fit and the number of clusters
             name = ClusterImp$CommonName)
    
    ggplot(aes(x = X, y = Y), data = tsne_data) +
      geom_point(aes(color = cluster), size = 2)+
      annotate("text", x = xmin, y = ymax, label = labs, size = 5,
               parse = TRUE)+
      #scale_shape_manual(values = c(1,5,11,18,22,9,3,2,15))+
      theme_base()+
      theme(legend.position = "none",
            axis.title = element_blank())
  }
  
    ##########---------------------------------------------#################
          ###---------------Complete linkage-------------####
  ##########---------------------------------------------#################
  
  ##Eskin 
  set.seed(1)
  #eskin_fit <- fit_nomclust(eskin,methodsClust[2])
  dissMatrix <- as.dist(distanceMatrices[[5]])
  tsne_obj <- Rtsne(dissMatrix, is_distance = TRUE, initial_dims = 25, 
                    perplexity = 5, theta = 0)

  eskinPlot3 <- tsneFunction(tsne_obj, dissMatrix, eskin_fit$mem$clu_3, 
                             3, "Eskin-3", -25, -35)
  eskinPlot5 <- tsneFunction(tsne_obj, dissMatrix, eskin_fit$mem$clu_5, 
                             5, "Eskin-5", -25, -35)
  eskinPlot7 <- tsneFunction(tsne_obj, dissMatrix, eskin_fit$mem$clu_7, 
                              7, "Eskin-7", -25, -35)
  eskinPlot9 <- tsneFunction(tsne_obj, dissMatrix, eskin_fit$mem$clu_9, 
                             9, "Eskin-9", -25, -35)
  
  eskinPlot <- grid.arrange(eskinPlot3,eskinPlot5,eskinPlot7,eskinPlot9,
                            nrow = 2)
  
  #Change name of linkage method
  ggsave(eskinPlot, filename = "output/t-SNE clusters/eskin_completev2.pdf",
         device = "pdf", width = 9.04, height = 6.84)
  
  
  #IOF
  #iof_fit<-fit_nomclust(iof,methodsClust[2])
  dissMatrix <- as.dist(distanceMatrices[[6]])
  tsne_obj <- Rtsne(dissMatrix, is_distance = TRUE)
  
  iofPlot3 <- tsneFunction(tsne_obj, dissMatrix, iof_fit$mem$clu_3, 
                           3, "IOF-3", -3,4.5)
  iofPlot5 <- tsneFunction(tsne_obj, dissMatrix, iof_fit$mem$clu_5, 
                           5,"IOF-5", -3,4.5)
  iofPlot7 <- tsneFunction(tsne_obj, dissMatrix, iof_fit$mem$clu_7, 
                           7,"IOF-7", -3,4.5)
  iofPlot9 <- tsneFunction(tsne_obj, dissMatrix, iof_fit$mem$clu_9, 
                           9,"IOF-9", -3,4.5)
  
  iofPlot <- grid.arrange(iofPlot3,iofPlot5,iofPlot7,iofPlot9, nrow = 2)
  
  #Change name of linkage method
  ggsave(iofPlot, filename = "output/t-SNE clusters/iof_completev2.pdf",
         device = "pdf", width = 9.04, height = 6.84)
  
  #---Goodall
  #good_fit<-fit_nomclust(good1,methodsClust[2])
  dissMatrix <- as.dist(distanceMatrices[[1]])
  tsne_obj <- Rtsne(dissMatrix, is_distance = TRUE)
  
  goodPlot3 <- tsneFunction(tsne_obj, dissMatrix, good_fit$mem$clu_3, 
                            3, "Goodall-3",-4.5,-6)
  goodPlot5 <- tsneFunction(tsne_obj, dissMatrix, good_fit$mem$clu_5, 
                            5, "Goodall-5",-4.5,-6)
  goodPlot7 <- tsneFunction(tsne_obj, dissMatrix, good_fit$mem$clu_7, 
                            7, "Goodall-7",-4.5,-6)
  goodPlot9 <- tsneFunction(tsne_obj, dissMatrix, good_fit$mem$clu_9, 
                            9, "Goodall-9",-4.5,-6)
  
  goodPlot <- grid.arrange(goodPlot3,goodPlot5, goodPlot7,goodPlot9, 
                           nrow = 2)
  
  #Change name of linkage method
  ggsave(goodPlot, filename = "output/t-SNE clusters/good_completev2.pdf",
         device = "pdf", width = 9.04, height = 6.84)
  
  
  #---Lin
  #lin_fit<-fit_nomclust(lin,methodsClust[2])
  dissMatrix <- as.dist(distanceMatrices[[8]])
  tsne_obj <- Rtsne(dissMatrix, is_distance = TRUE)
  
  linPlot3 <- tsneFunction(tsne_obj, dissMatrix, lin_fit$mem$clu_3, 
                           3, "Lin-3",4,4.5)
  linPlot5 <- tsneFunction(tsne_obj, dissMatrix, lin_fit$mem$clu_5, 
                           5, "Lin-5",4,4.5)
  linPlot7 <- tsneFunction(tsne_obj, dissMatrix, lin_fit$mem$clu_7, 
                           7, "Lin-7",4,4.5)
  linPlot9 <- tsneFunction(tsne_obj, dissMatrix, lin_fit$mem$clu_9, 
                            9, "Lin-9",4,4.5)
  
  linPlot <- grid.arrange(linPlot3,linPlot5,linPlot7,linPlot9, nrow = 2)
  
  #Change name of linkage method
  ggsave(linPlot, filename = "output/t-SNE clusters/lin_completev2.pdf",
         device = "pdf", width = 9.04, height = 6.84)
  
  
  all_plot <- grid.arrange(eskinPlot3,eskinPlot5,eskinPlot7,eskinPlot9,
                           iofPlot3,iofPlot5, iofPlot7,iofPlot9,
                           goodPlot3,goodPlot5, goodPlot7,goodPlot9,
                           linPlot3,linPlot5, linPlot7, linPlot9, ncol = 4)

  ggsave(all_plot, filename = "documents/figures/all_complete.pdf",
         device = "pdf", width = 12.04, height = 9.84)
  

##########---------------------------------------------#################
      ####---------------Average linkage-------------####
##########---------------------------------------------#################
  
  
  ##Eskin
  set.seed(1)
  eskin_fit <- fit_nomclust(eskin,methodsClust[3])
  dissMatrix <- as.dist(distanceMatrices[[5]])
  tsne_obj <- Rtsne(dissMatrix, is_distance = TRUE, initial_dims = 25, 
                    perplexity = 5, theta = 0)
  
  eskinPlot3 <- tsneFunction(tsne_obj, dissMatrix, eskin_fit$mem$clu_3, 
                             3, "Eskin-3", 40, -30)
  eskinPlot5 <- tsneFunction(tsne_obj, dissMatrix, eskin_fit$mem$clu_5, 
                             5, "Eskin-5", 40, -30)
  eskinPlot7 <- tsneFunction(tsne_obj, dissMatrix, eskin_fit$mem$clu_7, 
                             7, "Eskin-7", 40, -30)
  eskinPlot9 <- tsneFunction(tsne_obj, dissMatrix, eskin_fit$mem$clu_9, 
                             9, "Eskin-9", 40, -30)
  
  eskinPlot <- grid.arrange(eskinPlot3,eskinPlot5,eskinPlot7,eskinPlot9,
                            nrow = 2)
  
  #Change name of linkage method
  ggsave(eskinPlot, filename = "output/t-SNE clusters/eskin_averagev2.pdf",
         device = "pdf", width = 9.04, height = 6.84)
  
  
  #IOF
  iof_fit<-fit_nomclust(iof,methodsClust[3])
  dissMatrix <- as.dist(distanceMatrices[[6]])
  tsne_obj <- Rtsne(dissMatrix, is_distance = TRUE)
  
  iofPlot3 <- tsneFunction(tsne_obj, dissMatrix, iof_fit$mem$clu_3, 
                           3, "IOF-3", 3,4.5)
  iofPlot5 <- tsneFunction(tsne_obj, dissMatrix, iof_fit$mem$clu_5, 
                           5,"IOF-5", 3,4.5)
  iofPlot7 <- tsneFunction(tsne_obj, dissMatrix, iof_fit$mem$clu_7, 
                           7,"IOF-7", 3,4.5)
  iofPlot9 <- tsneFunction(tsne_obj, dissMatrix, iof_fit$mem$clu_9, 
                           9,"IOF-9", 3,4.5)
  
  iofPlot <- grid.arrange(iofPlot3,iofPlot5,iofPlot7,iofPlot9, nrow = 2)
  
  #Change name of linkage method
  ggsave(iofPlot, filename = "output/t-SNE clusters/iof_averagev2.pdf",
         device = "pdf", width = 9.04, height = 6.84)
  
  #---Goodall
  good_fit<-fit_nomclust(good1,methodsClust[3])
  dissMatrix <- as.dist(distanceMatrices[[1]])
  tsne_obj <- Rtsne(dissMatrix, is_distance = TRUE)
  
  goodPlot3 <- tsneFunction(tsne_obj, dissMatrix, good_fit$mem$clu_3, 
                            3, "Goodall-3",4,7)
  goodPlot5 <- tsneFunction(tsne_obj, dissMatrix, good_fit$mem$clu_5, 
                            5, "Goodall-5",4,7)
  goodPlot7 <- tsneFunction(tsne_obj, dissMatrix, good_fit$mem$clu_7, 
                            7, "Goodall-7",4,7)
  goodPlot9 <- tsneFunction(tsne_obj, dissMatrix, good_fit$mem$clu_9, 
                            9, "Goodall-9",4,7)
  
  goodPlot <- grid.arrange(goodPlot3,goodPlot5, goodPlot7,goodPlot9, 
                           nrow = 2)
  
  #Change name of linkage method
  ggsave(goodPlot, filename = "output/t-SNE clusters/good_averagev2.pdf",
         device = "pdf", width = 9.04, height = 6.84)
  
  
  #---Lin
  lin_fit<-fit_nomclust(lin,methodsClust[3])
  dissMatrix <- as.dist(distanceMatrices[[8]])
  tsne_obj <- Rtsne(dissMatrix, is_distance = TRUE)
  
  linPlot3 <- tsneFunction(tsne_obj, dissMatrix, lin_fit$mem$clu_3, 
                           3, "",4,4)
  linPlot5 <- tsneFunction(tsne_obj, dissMatrix, lin_fit$mem$clu_5, 
                           5, "Lin-5",4,4)
  linPlot7 <- tsneFunction(tsne_obj, dissMatrix, lin_fit$mem$clu_7, 
                           7, "Lin-7",4,4)
  linPlot9 <- tsneFunction(tsne_obj, dissMatrix, lin_fit$mem$clu_9, 
                           9, "Lin-9",4,4)
  
  linPlot <- grid.arrange(linPlot3,linPlot5,linPlot7,linPlot9, nrow = 2)
  
  #Change name of linkage method
  ggsave(linPlot, filename = "output/t-SNE clusters/lin_averagev2.pdf",
         device = "pdf", width = 9.04, height = 6.84)
  
  
  all_plot <- grid.arrange(eskinPlot3,eskinPlot5,eskinPlot7,eskinPlot9,
                           iofPlot3,iofPlot5, iofPlot7,iofPlot9,
                           goodPlot3,goodPlot5, goodPlot7,goodPlot9,
                           linPlot3,linPlot5, linPlot7, linPlot9, ncol = 4)
  
  ggsave(all_plot, filename = paste0(docDir,"documents/figures/all_average.pdf"),
         device = "pdf", width = 12.04, height = 9.84)
  

  # ClusterImp$lin3_a <- lin_fit$mem$clu_3
  # ClusterImp$lin5_a <- lin_fit$mem$clu_5
  # ClusterImp$lin7_a <- lin_fit$mem$clu_7
  # ClusterImp$lin9_a <- lin_fit$mem$clu_9
  # ClusterImp$good3_a <- good_fit$mem$clu_3
  # ClusterImp$good5_a <- good_fit$mem$clu_5
  # ClusterImp$good7_a <- good_fit$mem$clu_7
  # ClusterImp$good9_a <- good_fit$mem$clu_9
  # ClusterImp$iof3_a <- iof_fit$mem$clu_3
  # ClusterImp$iof5_a <- iof_fit$mem$clu_5
  # ClusterImp$iof7_a <- iof_fit$mem$clu_7
  # ClusterImp$iof9_a <- iof_fit$mem$clu_9
  # ClusterImp$eskin3_a <- eskin_fit$mem$clu_3
  # ClusterImp$eskin5_a <- eskin_fit$mem$clu_5
  # ClusterImp$eskin7_a <- eskin_fit$mem$clu_7
  # ClusterImp$eskin9_a <- eskin_fit$mem$clu_9

  