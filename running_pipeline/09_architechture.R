
rm(list=ls())
gc()



library(ggthemes)
library(ggplot2)
library(ggpubr)


########################################### 1.smartseq2
setwd("./atac_from_fjl/scrna_scatac_arch/scRNA_Arch_Smartseq2/")

level_order <- c("Baseline", "DeepCNN", "CBAM", "ResNet18", "Transformer")

metric.fpaths <- paste0(level_order, "/Metric.csv")
metric.fpaths <- metric.fpaths[file.exists(metric.fpaths)]
metric.list <- lapply(metric.fpaths, function(metric.fname){
  print(metric.fname)
  metric <- as.data.frame(t(read.csv(metric.fname, header=T, row.names = 1)))
  metric$type <- strsplit(metric.fname, '/')[[1]][1]
  return(metric)
})
metric <- do.call(rbind, metric.list)
head(metric,3)

# sort factor
metric$type <- factor(metric$type, levels = level_order)

metric <- metric[order(metric$type),]
# metric$value <- ifelse(metric$value > 0.5, metric$value, 1 - metric$value)
dim(metric); head(metric)

P1 <- ggplot(metric, aes(x=type, y=auroc, fill=type)) + labs(x = NULL)+
  geom_boxplot() + theme_bw() + ylim(0.65,0.87) +
  theme(panel.grid = element_blank(),       
    panel.background = element_blank(),  
    panel.border = element_rect(fill = NA, colour = "black"))+
  theme(axis.text.x = element_text(angle = 0, vjust = .5, size=10), legend.position="none") +
  ylab("AUROC") + scale_fill_brewer(palette = 'Set3')+
  theme(axis.text.x = element_text(angle = 45, vjust = .6, size=10),
        axis.text.y = element_text(hjust = -0.6,size=10),
        axis.text = element_text(colour = "black"),
        axis.title.y = element_text(size=10),
        legend.position="none",legend.title = element_blank())

P1
ggsave("./Figure4/Figure4_smartseq_new.png", plot = P1, 
       width = 7, height = 7, units = "cm", dpi = 300,
       device = "png", bg = "white") 

tmp1 <- metric %>% group_by(type) %>% summarise(mean(auroc))
tmp1
########################################### 2. 10X

setwd("./atac_from_fjl/scRNA_arch_10X")
level_order <- c("Baseline", "DeepCNN", "CBAM", "ResNet18", "Transformer")

metric.fpaths <- paste0(level_order, "/Test/Metric.csv")
metric.fpaths <- metric.fpaths[file.exists(metric.fpaths)]
metric.list <- lapply(metric.fpaths, function(metric.fname){
  print(metric.fname)
  metric <- as.data.frame(t(read.csv(metric.fname, header=T, row.names = 1)))
  metric$type <- strsplit(metric.fname, '/')[[1]][1]
  return(metric)
})
metric2 <- do.call(rbind, metric.list)
head(metric2,3)

# sort factor
metric2$type <- factor(metric2$type, levels = level_order)

metric2 <- metric2[order(metric2$type),]
# metric$value <- ifelse(metric$value > 0.5, metric$value, 1 - metric$value)
dim(metric2); head(metric2)

P2 <- ggplot(metric2, aes(x=type, y=auroc, fill=type)) + labs(x = NULL)+
  geom_boxplot() + theme_bw() + ylim(0.65,0.87) +
  theme(panel.grid = element_blank(),       
        panel.background = element_blank(),  
        panel.border = element_rect(fill = NA, colour = "black"))+
  theme(axis.text.x = element_text(angle = 0, vjust = .5, size=10), legend.position="none") +
  ylab("AUROC") + scale_fill_brewer(palette = 'Set3')+
  theme(axis.text.x = element_text(angle = 45, vjust = .6, size=10),
        axis.text.y = element_text(hjust = -0.6,size=10),
        axis.title.y = element_text(size=10),
        axis.text = element_text(colour = "black"),
        legend.position="none",legend.title = element_blank())
P2
ggsave("./Figure4/Figure4_10X_new.png", plot = P2, 
       width = 7, height = 7, units = "cm", dpi = 300,
       device = "png", bg = "white") 

tmp2 <- metric2 %>% group_by(type) %>% summarise(mean(auroc))
tmp2
########################################### 3. ATAC
setwd("./atac_from_fjl/scrna_scatac_arch/scATAC_arch_100cell/")
level_order <- c("Baseline", "DeepCNN", "CBAM", "ResNet18", "Transformer","scBasset","NvwaCE")

metric.fpaths <- paste0(level_order, "/Metric.csv")
metric.fpaths <- metric.fpaths[file.exists(metric.fpaths)]
metric.list <- lapply(metric.fpaths, function(metric.fname){
  print(metric.fname)
  metric <- as.data.frame(t(read.csv(metric.fname, header=T, row.names = 1)))
  metric$type <- strsplit(metric.fname, '/')[[1]][1]
  return(metric)
})
metric <- do.call(rbind, metric.list)
head(metric,3)

# sort factor
metric$type <- factor(metric$type, levels = level_order)

metric <- metric[order(metric$type),]
# metric$value <- ifelse(metric$value > 0.5, metric$value, 1 - metric$value)
dim(metric); head(metric)

P3 <- ggplot(metric, aes(x=type, y=auroc, fill=type)) + labs(x = NULL)+
  geom_boxplot() + theme_bw() +
  theme(panel.grid = element_blank(),       
        panel.background = element_blank(),  
        panel.border = element_rect(fill = NA, colour = "black"))+
  theme(axis.text.x = element_text(angle = 0, vjust = .5, size=10), legend.position="none") +
  ylab("AUROC") + scale_fill_brewer(palette = 'Set3')+
  theme(axis.text.x = element_text(angle = 45,  size=10),
        axis.text.y = element_text(hjust = -0.6,size=10),
        axis.text = element_text(colour = "black"),
        axis.title.y = element_text(size=10),
        legend.position="none",legend.title = element_blank())
P3
ggsave("./Figure4/Figure4_ATAC_new2.png", plot = P3, 
       width = 7.5, height = 7, units = "cm", dpi = 300,
       device = "png", bg = "white") 


tmp <- metric %>% group_by(type) %>% summarise(mean(auroc))
tmp
########################################### scATAC

###########################################
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
  theme(axis.text.x = element_text(angle = 0, vjust = .5, size=10), legend.position="none") +
  ylab("Information Content") + scale_fill_brewer(palette = 'Set3')+
  theme(axis.text.x = element_text(angle = 45, vjust = .6, size=10),
        axis.text.y = element_text(hjust = -0.6,size=10),
        axis.title.y = element_text(size=10),
        legend.position="none",legend.title = element_blank())
P4
ggsave("./Figure4/FigureS4_IC_smartseq2.png", plot = P4, 
       width = 4, height = 4, units = "cm", dpi = 300,
       device = "png", bg = "white") 



#pdf("./benchmark_Arch_scRNA_IC_test.pdf", width = 4, height = 6)
ggplot(IC, aes(x=type, y=IC, fill=type)) + 
  geom_boxplot() + theme_base() +
  theme(axis.text.x = element_text(angle = 45, vjust = .5, size=10), legend.position="none") +
  ylab("IC(Information Content)") + scale_fill_brewer(palette = 'Set3') 
#+#scale_fill_manual()
# stat_compare_means(aes(group = type), method="wilcox.test", label = "p.format", label.y = 1) 
dev.off()

###########################################
baseline <- metric[metric$type == "scRNA_N128L15PaP15ReLU", 'auroc']
alter <- metric[metric$type == "scRNA_DeepCNN", 'auroc']
wilcox.test(alter, baseline, 'greater')

baseline <- IC[IC$type == "scRNA_N128L15PaP15ReLU", 'IC']
alter <- IC[IC$type == "scRNA_N128L25PaP15ReLU", 'IC']
wilcox.test(baseline, alter, 'greater')

