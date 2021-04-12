rm(list = ls())

docDir <- "C://Users/laddsmo/Victoria University of Wellington - STAFF/OneDrive - Victoria University of Wellington - STAFF/Papers_Projects/TropicGroupClassification/"


library(mclust)
library(xtable)

load(file = "data/Final_clusters.RData")
rownames(ClusterImp)<-ClusterImp$CommonName

Cmethod <- colnames(ClusterImp[,c(29:60)])

#calculate the % of agreement for each species pair across the differnt grouping methods
par(mar = c(3,3,1,1))
groupseq <- seq(1,32,4)
comparison$sum3 <- rowSums(comparison[,c(Cmethod[groupseq])]=="Match")/8
hist(comparison$sum3,main = "")

groupseq <- seq(1,15,4)
comparison$sum3c <- rowSums(comparison[,c(Cmethod[groupseq])]=="Match")/4
hist(comparison$sum3c,main = "")

groupseq <- seq(17,30,4)
comparison$sum3a <- rowSums(comparison[,c(Cmethod[groupseq])]=="Match")/4
hist(comparison$sum3a,main = "")

groupseq7_8 <- seq(10,68,8)
comparison$sum6 <- rowSums(comparison[,groupseq7_8]=="Match")/8
hist(comparison$sum6,main = "", xlab = "Proportion of matches")


#which fish always group together?
match3<-comparison[comparison$sum3>=0.8,1:2]
nrow(comparison[comparison$sum5>=0.5,1:2])
nomatch<-comparison[comparison$sum5<0.2,1:2]



cor.test <- cor(ClusterImp[,c(seq(29,60,4))])

table(ClusterImp$good3_a,ClusterImp$good3_c) #0.9151252
table(ClusterImp$good3_a,ClusterImp$lin3_a) #0.9584182
table(ClusterImp$good3_a,ClusterImp$lin3_c) #0.8900839
table(ClusterImp$good3_c,ClusterImp$lin3_a) #0.8940058


##NULL dataset for the output
combdf <- combn(ClusterImp$CommonName,2)
comparison<-data.frame(combdf[1,], combdf[2,])
colnames(comparison) <- c("c1", "c2")


for(j in c(29:60)){
cor.matrix <- as.matrix(dist(ClusterImp[,j],method = "manhattan"))
row.names(cor.matrix) <- row.names(ClusterImp)
colnames(cor.matrix)<- row.names(ClusterImp)
m2 <- melt(cor.matrix)[melt(upper.tri(cor.matrix))$value,]
m2$value<-ifelse(m2$value==0,"Match","NoMatch")
names(m2) <- c("c1", "c2", Cmethod[j-28])
comparison <- merge(comparison,m2,by=c("c1","c2"))
}

save(comparison,file="output/comparison3-9.RData")

#Compute comparison scores
Indexij <- comparison[,3:34]
Indexij <- as.data.frame(unclass(Indexij))
n <- ncol(comparison[,3:34])

#Rand index
RandInd <- matrix(0,n,n)

for ( i in 1:n)
{
  for (j in i:n)
  {
    RandInd[i,j] <- adjustedRandIndex(Indexij[,i],Indexij[,j])
  }
}
RandInd <- t(RandInd)

row.names(RandInd) <- Cmethod
colnames(RandInd) <- Cmethod

randThree <- RandInd[seq(1,32,4),seq(1,32,4)]
compareIndex <- xtable(randThree, digits = 2,
                       caption = "Binary classification test results (Adjusted Rand's index) indicating 
                      similarity between clustering methods using three clusters")
print(compareIndex, file = paste0(docDir,"documents/tables/compareIndex3.tex"))


randNine <- RandInd[seq(4,32,4),seq(4,32,4)]
compareIndex <- xtable(randNine, digits = 2,
                       caption = "Binary classification test results (Adjusted Rand's index) indicating 
                      similarity between clustering methods using nine clusters")
print(compareIndex, file = paste0(docDir,"documents/tables/compareIndex9.tex"))


#save(RandInd, file = "output/RandIndex.RData")


#Sensitivity
SenseInd <- matrix(0,n,n)

for ( i in 1:n)
{
  for (j in i:n)
  {
    SenseInd[i,j] <- sensitivity(Indexij[,i],Indexij[,j])
  }
}
SenseInd <- t(SenseInd)
save(SenseInd, file = "output/SensitivityIndex.RData")


#Specificity
SpecInd <- matrix(0,n,n)

for ( i in 1:n)
{
  for (j in i:n)
  {
    SpecInd[i,j] <- specificity(Indexij[,i],Indexij[,j])
  }
}
SpecInd <- t(SpecInd)
save(SpecInd, file = "output/SpecificityIndex.RData")

#Precision (positive predictive value)
PPVInd <- matrix(0,n,n)

for ( i in 1:n)
{
  for (j in i:n)
  {
    PPVInd[i,j] <- posPredValue(Indexij[,i],Indexij[,j])
  }
}

PPVInd <- t(PPVInd)
save(PPVInd, file = "output/PPVIndex.RData")

#Negative predictive value
NPVInd <- matrix(0,n,n)

for ( i in 1:n)
{
  for (j in i:n)
  {
    NPVInd[i,j] <- negPredValue(Indexij[,i],Indexij[,j])
  }
}
NPVInd <- t(NPVInd)
save(NPVInd, file = "output/NVPIndex.RData")


sumMatrix <- t(matrix(mapply(mean, SenseInd,SpecInd, MoreArgs=list(na.rm=T)),ncol=32))

sumMatrix <- data.frame(sumMatrix, row.names = Cmethod)
colnames(sumMatrix) <- Cmethod


compare <- data.frame(RandInd[,1], SenseInd[,1], SpecInd[,1],
                      NPVInd[,1], PPVInd[,1], 
                      RandInd[,2], SenseInd[,2], SpecInd[,2],
                      NPVInd[,2], PPVInd[,2],
                      row.names = Cmethod)
colnames(compare) <- c("Rand Index","Sensitivity","Specificity",
                       "Precision","Negative predictive value",
                       "Rand Index","Sensitivity","Specificity",
                       "Precision","Negative predictive value")
compareIndex <- xtable(compare, digits = 2,
                       caption = "Binary classification test results (Rand's index, sensitivity,
                      specificity, precision and negative predictive value) indicating 
                      similarity between clustering methods and expert evalution and diet classification")
print(compareIndex, file = "documents/tables/compareIndex2-8.tex")