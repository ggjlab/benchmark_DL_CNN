rm(list=ls())
gc()

library(ggthemes)
library(ggplot2)
library(ggpubr)


########################################### 1.smartseq2
setwd("./atac_from_fjl/scRNA_arch_smartseq2/")
#setwd("/home/ggj/Galaxy/01_benmark_data/atac_from_fjl/scRNA_arch_smartseq2/")
#level_order <- c("Baseline", "scRNA_DeepCNN", "scRNA_CBAM", "scRNA_ResNet18", "scRNA_Transformer")
level_order <- c("Baseline", "DeepCNN", "CBAM", "ResNet18", "Transformer")

IC.fpaths <- paste0(level_order, "/Motif/meme_IC.csv")
IC.fpaths <- IC.fpaths[file.exists(IC.fpaths)]

IC.list <- lapply(IC.fpaths, function(IC.fname){
  print(IC.fname)
  IC <- read.csv(IC.fname)
  if (dim(IC)[-1] == 4){
    IC <- IC[,1:3]
  }
  IC$type <- strsplit(IC.fname, '/')[[1]][1]
  return(IC)
})
IC <- do.call(rbind, IC.list)

# sort factor
IC$type <- factor(IC$type, levels = level_order)
IC <- IC[order(IC$type),]
# IC$IC <- ifelse(IC$IC > 8, 8, IC$IC)
# IC$IC <- ifelse(IC$IC < 6, 6, IC$IC)
dim(IC); head(IC)


P4 <- ggplot(IC, aes(x=type, y=IC, fill=type)) + labs(x = NULL)+
  geom_boxplot() + theme_bw() +
  theme(panel.grid = element_blank(),       
        panel.background = element_blank(),  
        panel.border = element_rect(fill = NA, colour = "black"))+
  ylab("IC") + scale_fill_brewer(palette = 'Set3')+
  theme(axis.text.x = element_text(angle = 45,hjust = 1, size=8),
        axis.text.y = element_text(size=8),
        axis.title.y = element_text(vjust = -0.6,size=10),
        legend.position="none",legend.title = element_blank())

P4
ggsave("./Figure4/FigureS4_IC_smartseq2.png", plot = P4, 
       width = 4, height = 4, units = "cm", dpi = 300,
       device = "png", bg = "white") 

########################################### 2.10x
setwd("./atac_from_fjl/scRNA_arch_10X/")
#setwd("/home/ggj/Galaxy/01_benmark_data/atac_from_fjl/scRNA_arch_smartseq2/")
#level_order <- c("Baseline", "scRNA_DeepCNN", "scRNA_CBAM", "scRNA_ResNet18", "scRNA_Transformer")
level_order <- c("Baseline", "DeepCNN", "CBAM", "ResNet18", "Transformer")

IC.fpaths <- paste0(level_order, "/Motif/meme_IC.csv")
IC.fpaths <- IC.fpaths[file.exists(IC.fpaths)]

IC.list <- lapply(IC.fpaths, function(IC.fname){
  print(IC.fname)
  IC <- read.csv(IC.fname)
  if (dim(IC)[-1] == 4){
    IC <- IC[,1:3]
  }
  IC$type <- strsplit(IC.fname, '/')[[1]][1]
  return(IC)
})
IC <- do.call(rbind, IC.list)

# sort factor
IC$type <- factor(IC$type, levels = level_order)
IC <- IC[order(IC$type),]
# IC$IC <- ifelse(IC$IC > 8, 8, IC$IC)
# IC$IC <- ifelse(IC$IC < 6, 6, IC$IC)
dim(IC); head(IC)


P5 <- ggplot(IC, aes(x=type, y=IC, fill=type)) + labs(x = NULL)+
  geom_boxplot() + theme_bw() +
  theme(panel.grid = element_blank(),       
        panel.background = element_blank(),  
        panel.border = element_rect(fill = NA, colour = "black"))+
  ylab("IC") + scale_fill_brewer(palette = 'Set3')+
  theme(axis.text.x = element_text(angle = 45,hjust = 1, size=8),
        axis.text.y = element_text(size=8),
        axis.title.y = element_text(vjust = -0.6,size=10),
        legend.position="none",legend.title = element_blank())

P5
ggsave("./Figure4/FigureS4_IC_10X.png", plot = P5, 
       width = 4, height = 4, units = "cm", dpi = 300,
       device = "png", bg = "white") 


########################################### 3.ATAC-seq
setwd("./atac_from_fjl/atac_arch/")
level_order <- c("Baseline", "DeepCNN", "CBAM", "ResNet18", "Transformer")

IC.fpaths <- paste0(level_order, "/Motif/meme_IC.csv")
IC.fpaths <- IC.fpaths[file.exists(IC.fpaths)]

IC.list <- lapply(IC.fpaths, function(IC.fname){
  print(IC.fname)
  IC <- read.csv(IC.fname)
  if (dim(IC)[-1] == 4){
    IC <- IC[,1:3]
  }
  IC$type <- strsplit(IC.fname, '/')[[1]][1]
  return(IC)
})
IC <- do.call(rbind, IC.list)

# sort factor
IC$type <- factor(IC$type, levels = level_order)
IC <- IC[order(IC$type),]

dim(IC); head(IC)


P6 <- ggplot(IC, aes(x=type, y=IC, fill=type)) + labs(x = NULL)+
  geom_boxplot() + theme_bw() +
  theme(panel.grid = element_blank(),       
        panel.background = element_blank(),  
        panel.border = element_rect(fill = NA, colour = "black"))+
  ylab("IC") + scale_fill_brewer(palette = 'Set3')+
  theme(axis.text.x = element_text(angle = 45,hjust = 1, size=8),
        axis.text.y = element_text(size=8),
        axis.title.y = element_text(vjust = -0.6,size=10),
        legend.position="none",legend.title = element_blank())

P6
ggsave("./Figure4/FigureS4_IC_ATAC.png", plot = P6, 
       width = 4, height = 4, units = "cm", dpi = 300,
       device = "png", bg = "white") 
