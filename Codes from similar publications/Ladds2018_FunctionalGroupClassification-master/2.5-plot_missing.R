
library(dplyr)
library(reshape)
library(ggplot2)
library(ggthemes)
docDir <- "C://Users/laddsmo/Victoria University of Wellington - STAFF/OneDrive - Victoria University of Wellington - STAFF/Papers_Projects/TropicGroupClassification/"


output<-NULL
for(i in seq(0.05,0.4,0.05)){
  load(paste0("output/Baseline/data/Baseline_accuracy_",i,"missing.RData"))
  load(paste0("output/MICEpolyreg/data/MICEpolyreg_accuracy_",i,"missing.RData"))
  load(paste0("output/missForest/data/missForest_accuracy_",i,"missing.RData"))
  load(paste0("output/missMDA/data/missMDA_accuracy_",i,"missing.RData"))
  
  replicate_BASE <- data.frame(replicate_BASE)
  colnames(replicate_BASE) <- "accuracy"
  replicate_BASE$prop <- i
  replicate_BASE$method <- "BASE"
  
  replicate_MICE <- data.frame(replicate_MICE)
  colnames(replicate_MICE) <- "accuracy"
  replicate_MICE$prop <- i
  replicate_MICE$method <- "MICE"
  replicate_MissForest <- data.frame(replicate_MissForest)
  colnames(replicate_MissForest) <- "accuracy"
  replicate_MissForest$prop <- i
  replicate_MissForest$method <- "missForest"
  replicate_missMDA <- data.frame(replicate_missMDA)
  colnames(replicate_missMDA) <- "accuracy"
  replicate_missMDA$prop <- i
  replicate_missMDA$method <- "missMDA"
  
  output <- rbind(output, replicate_BASE)
  
  output <- rbind(output, replicate_MICE)
  output <- rbind(output, replicate_MissForest)
  output <- rbind(output, replicate_missMDA)
}

ALLsum <- output %>% group_by(method,prop) %>% 
  summarise(average = mean(accuracy),
            st.dev = sd(accuracy))

missPlot <- ggplot(data=ALLsum, aes(x=prop, y=average, col=method))+
  geom_errorbar(aes(ymin=average-st.dev, ymax=average+st.dev), width=0.05,
                position = position_dodge(width = 0.02))+
  scale_color_manual("method", breaks=c(1,2,3,4),values=c("black","#D55E00",  "darkblue", "darkgrey"))+
  xlab("Proportion missing (deleted)")+
  ylab("Accuracy")+
  geom_point(position = position_dodge(width = 0.02))+
  theme_base()+
  theme(legend.position = c(.9,.6),
        legend.title = element_blank(),
        plot.background = element_blank())


ggsave(missPlot, filename = paste0(docDir,"documents/figures/missing_plot.pdf"), 
       device = "pdf", width = 7.6, height = 4.6)
ggsave(missPlot, filename = paste0(docDir,"documents/figures/missing_plot.eps"), 
       device = "eps", width = 7.6, height = 4.6)
