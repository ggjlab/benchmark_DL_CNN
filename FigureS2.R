
library(ggthemes)
library(ggplot2)
library(ggpubr)
library(ggpointdensity) 
library(gridExtra)

######################### scatter smooth ##############################
library(cowplot)
levels_order <- c("mESC_BulkSeq", "mESC_MicrowellSeq", "mESC_DropSeq", 
                  "mESC_MARSseq", "mESC_CELseq2", "mESC_SmartSeq","mESC_SCRBseq", "mESC_SmartSeq2", 
                  "mESC_MAGIC-MicrowellSeq",  "mESC_MAGIC-MARSseq", "mESC_MAGIC-DropSeq",
                  "mESC_MAGIC-CELseq2", "mESC_MAGIC-SmartSeq", "mESC_MAGIC-SCRBseq", "mESC_MAGIC-SmartSeq2")

levels_order <- c("mESC_MARSseq","mESC_MAGIC-MARSseq",
                  "mESC_MicrowellSeq", "mESC_MAGIC-MicrowellSeq",  
                  "mESC_DropSeq", "mESC_MAGIC-DropSeq",
                  "mESC_10X_V2", "mESC_MAGIC-10X_V2",
                  "mESC_CELseq2","mESC_MAGIC-CELseq2",
                  "mESC_SmartSeq","mESC_MAGIC-SmartSeq", 
                  "mESC_SCRBseq",   "mESC_MAGIC-SCRBseq",
                  "mESC_SmartSeq2",  "mESC_MAGIC-SmartSeq2")


prediction <- read.csv(paste0("mESC_benchmark_scTech/", "mESC_10X_V2", "/Test/test_mode_pred_prob.csv"), row.names = 1)
target_orig <- read.csv(paste0("mESC_benchmark_scTech/", "mESC_10X_V2", "/Test/test_target_prob.csv"), row.names = 1)
result <- as.data.frame(cbind(target_orig[,1], prediction[,1]))
colnames(result) <- c("target", "prediction")

generate_p <- function(i){
  print(i)
  prediction <- read.csv(paste0("mESC_benchmark_scTech/", i, "/Test/test_mode_pred_prob.csv"), row.names = 1)
  target_orig <- read.csv(paste0("mESC_benchmark_scTech/", i, "/Test/test_target_prob.csv"), row.names = 1)
  result <- as.data.frame(cbind(target_orig[,1], prediction[,1]))
  colnames(result) <- c("target", "prediction")
  # result <- log(result + 0.001)
  #titlename <- strsplit(i, '_')[[1]][2]
  titlename <- i
  p <- ggplot(result, aes(x=target, y=prediction)) +
    # geom_pointdensity() 
    geom_point(size=0.5) +
    geom_smooth(method = 'lm') +
    geom_rug() +
    labs(title = titlename) +#colnames(target_orig)[1]) +
    theme_bw() +
    theme(
      panel.grid = element_blank(),        
      panel.background = element_blank(), 
      panel.border = element_rect(fill = NA, colour = "black")  
    )+
    theme(plot.title = element_text(hjust = 0.5, size = 8,vjust=-2),
          axis.text = element_text(size=6),
          axis.title = element_text(size=8))  
  # stat_density2d(colour="grey")
  return(p)
}
ps <- lapply(levels_order, generate_p)


#ml <- marrangeGrob(ps, nrow=3, ncol=5)
ml <- marrangeGrob(ps, nrow=4, ncol=4)
print(ml)

ggsave("./Figures/scatter_mESCs.png", plot = ml, 
       width = 12, height = 16, units = "cm", dpi = 300,
       device = "png", bg = "white") 





mw <- read.csv(paste0("mESC_benchmark_scTech/mESC_MicrowellSeq/Test/test_target_prob.csv"), row.names = 1)
sm2 <- read.csv(paste0("mESC_benchmark_scTech/mESC_SmartSeq2/Test/test_target_prob.csv"), row.names = 1)
result <- as.data.frame(cbind(mw[,1], sm2[,1]))
colnames(result) <- c("mw", "sm2")
# result <- log(result + 0.001)
p <- ggplot(result, aes(x=mw, y=sm2)) +
  # geom_pointdensity() + 
  geom_point(size=0.5) +
  geom_smooth(method = 'lm') +
  geom_rug() +
  labs(title = "tech") +#colnames(target_orig)[1]) +
  # theme_base() +
  theme(plot.title = element_text(hjust = 0.5, size = 10))  
# stat_density2d(colour="grey")
p
