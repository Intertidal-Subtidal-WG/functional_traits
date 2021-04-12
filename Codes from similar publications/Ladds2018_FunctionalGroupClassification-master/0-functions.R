plotEvals <- function(dat, measure){
  ggplot(dat, 
         aes(x = cluster, y = value, color = distance))+
    geom_point()+geom_line()+
    theme_base()+
    ylab(measure)+
    xlab("Clusters")+
    scale_x_discrete(limits = c(2:9))+
    #scale_y_continuous(limits = c(lowylim,highylim))+
    scale_color_manual(values = group.colours)+
    theme(panel.grid.minor.x	= element_line(color = "grey"),
          panel.grid.major.x	= element_line(color = "grey"),
          panel.grid.major.y	= element_line(color = "grey"),
          legend.title = element_blank())
}


trend.plot <- function(num){
  group <- as.character(clusters[clusters$X==num,4])
  group <- paste(group,".catch",sep="")
  
  num.group <- select(effort, one_of(group))
  num.group$year <- as.numeric(effort$date)
  summed.num <- num.group %>% group_by(year) %>% summarise_each(funs(sum))
  
  num.group <- melt(summed.num,id.vars = "year")
  
  ggplot(num.group, aes(x = year, y = value, col = variable))+
    geom_line()+
    theme_classic()
}




ggproportions <- function(var, xaxis, title){
  ggplot(melted[melted$variable==var,], aes(x=groups, y=count, fill = value))+
  geom_bar(position = "fill", stat = "identity")+
  ylab("Proportions")+
  xlab("")+
  scale_fill_manual(values=cbPalette)+
  labs(fill = "")+
  ggtitle(title)+
  guides(fill = guide_legend(nrow = 1))+
  theme(legend.position = "top",
        axis.text.x = xaxis,
        legend.key.height=unit(0, "cm"),      
        legend.margin=margin(t=0, r=0, b=0, l=0, unit="cm"),
        plot.margin = unit(c(0.5,0.5,0,0.5), "lines"))
}


comPartRep <- function(x, A, B, C, D, E, G, H, I){
  wardsA <- comPart(comparison[,x],comparison[,A])
  wardsB <- comPart(comparison[,x],comparison[,B])
  wardsC <- comPart(comparison[,x],comparison[,C])
  wardsD <- comPart(comparison[,x],comparison[,D])
  wardsE <- comPart(comparison[,x],comparison[,E])
  wardsG <- comPart(comparison[,x],comparison[,G])
  wardsH <- comPart(comparison[,x],comparison[,H])
  wardsI <- comPart(comparison[,x],comparison[,I])
  wards <- melt(rbind(wardsA, wardsB, wardsC, wardsD, 
                      wardsE, wardsG, wardsH, wardsI))
}


#Rand index
compRandInd <- function(n,Indexij){
  m<- matrix(0,n,n)
  
  for ( i in 1:n)
  {
    for (j in i:n)
    {
      m[i,j] <- randIndex(Indexij[,i],Indexij[,j])
    }
  }
  m <- t(m)
  return(m)
}

#Sensitivity
compSenseInd <- function(n,Indexij){
  m<-matrix(0,n,n)
  
  for ( i in 1:n)
  {
    for (j in i:n)
    {
      m[i,j] <- sensitivity(Indexij[,i],Indexij[,j])
    }
  }
  m <- t(m)
  return(m)
}

#Specificity
compSpecInd <- function(n,Indexij){
  m<-  matrix(0,n,n)
  
  for ( i in 1:n)
  {
    for (j in i:n)
    {
      m[i,j] <- specificity(Indexij[,i],Indexij[,j])
    }
  }
  m <- t(m)
  return(m)
}

#Precision (positive predictive value)
compPPVInd <- function(n,Indexij){
  m<-  matrix(0,n,n)
  
  for ( i in 1:n)
  {
    for (j in i:n)
    {
      m[i,j] <- posPredValue(Indexij[,i],Indexij[,j])
    }
  }
  
  m <- t(m)
  return(m)
}

#Negative predictive value
compNPVInd <-function(n,Indexij){
  m<-   matrix(0,n,n)
  
  for ( i in 1:n)
  {
    for (j in i:n)
    {
      m[i,j] <- negPredValue(Indexij[,i],Indexij[,j])
    }
  }
  m <- t(m)
  return(m)
}


fit_nomclust <- function(distanceMethod,clusterMethod, cmin, cmax)
                        {nomclust(dataSET, measure = distanceMethod, 
                                           method = clusterMethod,
                                          clu_low = cmin, clu_high = cmax)}


groupfits <- function(final_fit,distMeasure,clusterMethod){
  dataSET$groups <- final_fit$mem
  cor.matrix <- sapply(final_fit$mem, function(x) as.matrix(dist(x,method = "manhattan")))
  
  for(k in 1:8) {
    m2 <- melt(matrix(cor.matrix[,k],nrow = nrow(dataSET)))[melt(upper.tri(matrix(cor.matrix[,k],nrow = nrow(dataSET))))$value,]
    m2$value<-ifelse(m2$value==0,"Match","NoMatch")
    names(m2) <- c("c1", "c2", paste0(distMeasure,"-",clusterMethod,"_",k+1))
    m2$c1<-factor(m2$c1,labels = namesComm[1:nrow(dataSET)-1])
    m2$c2<-factor(m2$c2,labels = namesComm[2:nrow(dataSET)])
    comparison <- merge(comparison,m2,by=c("c1","c2"))
  }
  return(comparison)
}


errorPlots <- function(dat,ylab, ylim1, ylim2, xlabel){
  scaleFUN <- function(x) sprintf("%.2f", x)
  
  ggplot(dat, aes(Method, value))+
  geom_bar(aes(fill = type), stat = "identity", position = "dodge")+
  theme_bw()+
    ylab(ylab)+
    xlab("")+
    scale_y_continuous(limits = c(ylim1,ylim2), labels = scaleFUN)+
    scale_fill_brewer(palette="Dark2")+
  theme(panel.border = element_blank(),
        axis.title.x = element_text(size=11,vjust=0),
        axis.title.y = element_text(size=11,vjust=2),
        axis.text.x = element_text(size=10,vjust=2, angle = 90),
        axis.text.y = element_text(size=11, color="black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        legend.key=element_blank(),
        legend.position="none")}



deleteANDreplace <- function(dat,dat2,prop){
  #pick rows from which we'll delete a column
  idx = sample(nrow(dat), prop*nrow(dat)*ncol(dat))
  
  #pick a column corresponding to each row to delete
  col_no = sample(ncol(dat), length(idx), replace = TRUE)
  
  #group sets of rows for which the same column will be deleted
  delDF = split(data.frame(idx, col_no), f = col_no)
  
  for (ii in seq_along(delDF)) 
    dat[delDF[[ii]]$idx, delDF[[ii]][1L, 'col_no']] = NA
  
  cluster.imp <- missForest(dat, variablewise = TRUE)
  
  dat <- cluster.imp$ximp
  output<-NULL
  for (ii in seq_along(idx)){ 
    x<-dat[idx[ii], col_no[ii]]
    y<-dat2[idx[ii], col_no[ii]]
    xy<-cbind(x,y)
    output<-rbind(output,xy)
  }
  output<-data.frame(output)
  output$diff<-output$x-output$y
  return(sum(output$diff==0)/nrow(output))
}



ggdendPlot <- function(dst, hca, k, clust, dat){
  library(ggplot2)
  library(ggdendro)     # for dendro_data(...)
  dendr    <- dendro_data(hca, type="rectangle") # convert for ggplot
  clust.df <- data.frame(label=rownames(dat), cluster=factor(clust))
  dendr[["labels"]]   <- merge(dendr[["labels"]],clust.df, by="label")
  rect <- aggregate(x~cluster,dendr$labels,range)
  rect <- data.frame(rect$cluster,rect$x)
  ymax <- mean(hca$height[length(hca$height)-((k-2):(k-1))])
  
  ggplot() + 
    geom_segment(data=dendr$segments, aes(x=x, y=y, xend=xend, yend=yend)) + 
    geom_text(data=dendr$labels, aes(x, y, label=label, hjust=0,  size=1)) +
    geom_rect(data=rect, aes(xmin=X1-0, xmax=X2+0, ymin=0, ymax=ymax), 
              color="red", fill=NA)+
    ylab("Distance")+
    coord_flip() + scale_y_reverse(element_blank(),expand=c(0, 2)) + 
    scale_x_continuous(element_blank(),expand = c(0, 1))+
    theme_bw()+
    theme(panel.border = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size=11,vjust=2),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size=11, color="black"),
          axis.ticks = element_blank(),
          axis.line=element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          axis.line.x = element_line(colour = "black"),
          axis.line.y = element_line(colour = "black"),
          legend.key=element_blank(),
          legend.position="none")
}

count_blanks <- function(x) sum(x == "")/length(fishes$group)

count_zeros <- function(x) sum(x == 0)

count_NAs <- function(x) sum(is.na(x))

replaceBlanks <- function(x){
  x[x == ""] <- NA
}

scaled <- function(x){var<-(x - min(x, na.rm = TRUE))/(max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
return(var)}

normalised <- function(x) log(x)/log(max(x, na.rm = TRUE))



cmapply <- function(FUN, ..., MoreArgs = NULL, SIMPLIFY = TRUE, 
                    USE.NAMES = TRUE)
{
  l <- expand.grid(..., stringsAsFactors=FALSE)
  r <- do.call(mapply, c(
    list(FUN=FUN, MoreArgs = MoreArgs, SIMPLIFY = SIMPLIFY, USE.NAMES = USE.NAMES), 
    l
  ))
  if (is.matrix(r)) r <- t(r) 
  cbind(l, r)
}



similar <- function(gower_mat, dat)  dat[which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]), arr.ind = TRUE)[1,],]
dissimilar <- function(gower_mat, dat) dat[which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]), arr.ind = TRUE)[1,],]

#Calculate the sil width for plotting

sil_width <- function(dist){
  width <- c(NA)
  
  for(i in 2:50){
    pam_fit <- pam(dist,
                   diss = TRUE,
                   k = i)
    
    width[i] <- pam_fit$silinfo$avg.width
  }
  return(width)
  
}


###Plot the silohette widths
plot_silWidth <- function(title, width){
  
  png(filename = paste0("documents/figures/", title, ".png"), 
      height = 400, width = 600)
  plot(1:50, width, 
       xlab = "Number of clusters",
       ylab = "Silhouette width",
       ylim = c(0,1),
       main = paste0(title, " ",
                     round(max(width, na.rm = TRUE),4)))
  lines(1:50, width)   
  
  dev.off()
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#summarise the results of the clustering model
pam_results <- function(data, variables, final_fit){
  
  data %>%
    dplyr::select(variables) %>%
    mutate(cluster = final_fit$clustering) %>%
    group_by(cluster) %>%
    do(the_summary = summary(.))
  
}

hclust_results <- function(dat, variables, clust){
dat %>%
  dplyr::select(variables) %>%
  mutate(cluster = clust) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
}


printbold <-
  function(x, which = NULL, each = c("column", "row"), max = TRUE,
           NA.string = "", type = c("latex", "html"),
           sanitize.text.function = force,
           sanitize.rownames.function = NULL,
           sanitize.colnames.function = NULL, ...)
  {
    stopifnot(inherits(x, "xtable"))
    each <- match.arg(each)
    type <- match.arg(type)
    digits <- rep(digits(x), length = ncol(x)+1)
    if (!is.null(which)) {
      stopifnot(nrow(which) == nrow(x))
      stopifnot(ncol(which) == ncol(x))
      boldmatrix <- which
    } else {
      boldmatrix <- matrix(FALSE, ncol = ncol(x), nrow = nrow(x))
      ## round values before calculating max/min to avoid trivial diffs
      for (i in 1:ncol(x)) {
        if (!is.numeric(x[,i])) next
        x[,i] <- round(x[,i], digits = digits[i+1])
      }
      if (each == "column") {
        max <- rep(max, length = ncol(x))
        for (i in 1:ncol(x)) {
          xi <- x[,i]
          if (!is.numeric(xi)) next
          if (is.na(max[i])) next
          imax <- max(xi, na.rm = TRUE)
          if (!max[i])
            imax <- min(xi, na.rm = TRUE)
          boldmatrix[xi == imax, i] <- TRUE
        }
      } else if (each == "row") {
        max <- rep(max, length = nrow(x))
        for (i in 1:nrow(x)) {
          xi <- x[i,]
          ok <- sapply(xi, is.numeric)
          if (!any(ok)) next
          if (is.na(max[i])) next
          imax <- max(unlist(xi[ok]), na.rm = TRUE)
          if (!max[i])
            imax <- min(unlist(xi[ok]), na.rm = TRUE)
          whichmax <- sapply(xi, identical, imax)
          boldmatrix[i, whichmax] <- TRUE
        }
      }
    }
    ## need to convert to character
    ## only support per-column formats, not cell formats
    display <- rep(display(x), length = ncol(x)+1)
    for (i in 1:ncol(x)) {
      if (!is.numeric(x[,i])) next
      ina <- is.na(x[,i])
      x[,i] <- formatC(x[,i], digits = digits[i+1],
                       format = display[i+1])
      x[ina, i] <- NA.string
      display(x)[i+1] <- "s"
      ## embolden
      yes <- boldmatrix[,i]
      if (type == "latex") {
        x[yes,i] <- paste("\\textbf{", x[yes,i], "}", sep = "")
      } else {
        x[yes,i] <- paste("<strong>", x[yes,i], "</strong>", sep = "")
      }
    }
    print(x, ..., type = type, NA.string = NA.string,
          sanitize.text.function = sanitize.text.function,
          sanitize.rownames.function = sanitize.rownames.function,
          sanitize.colnames.function = sanitize.colnames.function)
  }