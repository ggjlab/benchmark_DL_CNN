
rm(list=ls())
gc()



library(ggthemes)
library(ggplot2)
library(ggpubr)
library(ggpointdensity)
library(gridExtra)
library(reshape2)
library(RColorBrewer)


###################### metric 1 #################################
##### Batch
metric1 <- read.csv("./robustness/Test/Metric.csv", row.names = 1)
metric1 <- as.data.frame(t(metric1))
colnames(metric1) <- c('AUROC','AUPR')
metric1 <- as.data.frame(metric1)
head(metric1,3)

a2 <- data.frame(reshape2::colsplit(rownames(metric1),"[_]",names = 1:2)[1])
a2 <- data.frame(reshape2::colsplit(a2$X1,"[.]",names = 1:2)[2])
unique(a2)
a3 <- str_replace(a2$X2, "[AB]$", "")
unique(a3)
metric1$tech <- a3
dim(metric1); head(metric1)
metric1 <- metric1[!metric1$tech %in% c('BulkSeq.E','BulkSeq.ESC'),]
#table(metric1$tech)
# metric1$PCC <- as.numeric(metric1$PCC); metric1$SCC <- as.numeric(metric1$SCC)
#reorder
levels_order_t <- c("10X","MicrowellSeq", "DropSeq", "MARSseq", "CELseq2","SmartSeq","SCRBseq", "SmartSeq2")
metric1$type <- factor(metric1$type, levels = levels_order_t)
metric1 <- metric1[order(metric1$type),]
#metric1$Group <- "Batch"
metric1$Group <- "Single cell"

dim(metric1);head(metric1)


###################### metric 2 #################################
##### BatchRemoved
setwd("./03_Benchmark_scTech_mESC_AUROC_binary/")
levels_order <- c("mESC_10X","mESC_MicrowellSeq", "mESC_DropSeq","mESC_MARSseq", 
                  "mESC_CELseq", "mESC_SmartSeq", "mESC_SCRBseq", "mESC_SmartSeq2")
metric2.fpaths <- paste0(levels_order, "/Test/Metric.csv")
# metric2.fpaths <- paste0("mESC_benchmark_scTech/", levels_order, "/Test/test_mode_correlation.csv")
metric2.list <- lapply(metric2.fpaths, function(metric2.fname){
  print(metric2.fname)
  metric2 <- as.data.frame(t(read.csv(metric2.fname, row.names = 1)))
  colnames(metric2) <- c('AUROC','AUPR')
  #metric2$type <- strsplit(metric2.fname, '/')[[1]][2]
  #metric2$type <- strsplit(metric2$type, '_')[[1]][2]
  return(metric2)
})
metric2 <- do.call(rbind, metric2.list)
#metric$type <- gsub("_Trial.", "", metric$type)
#unique(metric2$type)

head(metric2)
# metric2$PCC <- as.numeric(metric2$PCC); metric2$SCC <- as.numeric(metric2$SCC)
# reorder
levels_order_t <- c("MicrowellSeq", "DropSeq", "MARSseq", "CELseq","SmartSeq","SCRBseq", "SmartSeq2")
#metric2$type <- factor(metric2$type, levels = levels_order_t)
#metric2 <- metric2[order(metric2$type),]

a2 <- data.frame(reshape2::colsplit(rownames(metric2),"[_]",names = 1:2)[1])
a2 <- data.frame(reshape2::colsplit(a2$X1,"[.]",names = 1:2)[2])
unique(a2)
a3 <- str_replace(a2$X2, "[AB]$", "")
unique(a3)
metric2$tech <- a3
#metric2$Group <- "BatchRemoved"
metric2$Group <- "Platform"
dim(metric2); head(metric2)


###################### metric 3 (rep) ###########################
# no Microwellseq
setwd("./d03_BIB_BatchEffect/")

levels_order <- c("mESC_DropSeqA", "mESC_DropSeqB", "mESC_MARSseqA", "mESC_MARSseqB",  
                  "mESC_CELseq2A", "mESC_CELseq2B","mESC_SmartSeqA", "mESC_SmartSeqB",
                  "mESC_SCRBseqA", "mESC_SCRBseqB",  "mESC_SmartSeq2A", "mESC_SmartSeq2B")
#metric3.fpaths <- paste0("./Benchmark_scTech_mESC/",levels_order, "/Test/Metric.csv")
metric3.fpaths <- paste0(levels_order, "/Test/Metric.csv")
metric3.fpaths <- metric3.fpaths[file.exists(metric3.fpaths)]
metric3.list <- lapply(metric3.fpaths, function(metric3.fname){
  print(metric3.fname)
  metric3 <- as.data.frame(t(read.csv(metric3.fname, row.names = 1)))
  return(metric3)
})
metric3 <- do.call(rbind, metric3.list)
# metric$type <- gsub("_Trial.", "", metric$type)
#unique(metric3$type)
# metric3$X <- rownames(metric3)
dim(metric3);head(metric3)
# order: c('PCC','SCC','type','Group')
table(metric3$Group)

#dim(metric3);head(metric3)

a2 <- data.frame(reshape2::colsplit(rownames(metric3) ,"[_]",names = 1:2)[1])
a2 <- data.frame(reshape2::colsplit(a2$X1,"[.]",names = 1:2)[2])
unique(a2)
#reshape2::colsplit(DropSeqA,"[AB]",names = 1:2)
a3 <- str_replace(a2$X2, "[AB]$", "")
unique(a3)
a4 <- str_sub(a2$X2,-1,-1)
unique(a4)
metric3$tech <- a3
metric3$Group <- paste0("Replicate",a4)
colnames(metric3) <- c('AUROC','AUPR','tech','Group')
head(metric3)
table(metric3$tech)
# table(metric3$Group)
# a <- metric3

######################## metric merge ##########################
# metric3 colnames change
# check the class
#class(metric1$PCC); class(metric2$PCC); class(metric3$PCC); 
head(metric1,3); head(metric2,3); head(metric3,3);
#table(metric1$type); table(metric2$type); table(metric3$type)
table(metric1$tech); table(metric2$tech); table(metric3$tech)

metric <- rbind(metric1,metric2, metric3)
dim(metric); head(metric)
table(metric$tech)
table(metric$Group)

# reorder
# sort factor
#metric$Group <- factor(metric$Group, levels = c('Batch','BatchRemoved', 'ReplicateA','ReplicateB'))
metric$Group <- factor(metric$Group, levels = c('Single cell','Platform', 'ReplicateA','ReplicateB'))

metric <- metric[order(metric$Group),]


levels_order_t <- c("10X","MicrowellSeq" ,"DropSeq", "MARSseq", "CELseq2","SmartSeq","SCRBseq", "SmartSeq2" )
metric$tech <- factor(metric$tech, levels = levels_order_t)
metric <- metric[order(metric$tech),]
# metric$value <- ifelse(metric$value > 0.5, metric$value, 1 - metric$value)
metric <- metric[!is.na(metric$tech),]
dim(metric); head(metric) 


metric <- metric %>%
  mutate(Platform = recode(tech,
                           "MARSseq" = "MARS-seq", 
                           'MicrowellSeq'='Microwell-seq',
                           "DropSeq"="Drop-seq",
                           'CELseq2'='CEL-seq2',
                           'SmartSeq'='Smart-seq',
                           'SCRBseq'='SCRB-seq',
                           'SmartSeq2'='Smart-seq2',
                           "10X" = "10X-V2"))

###### plot ########
# AUROC

P0 <- ggplot(metric, aes(x=tech, y=AUROC, fill=Group)) + 
  geom_boxplot(varwidth = FALSE, outlier.size = 0.2, outlier.alpha = 0.8,
               notch = FALSE,notchwidth = 0.5) + 
  scale_y_continuous(limits = c(0.6, 0.9), expand = expansion(mult = c(0, 0.1))) +
  theme_base() +
  theme(axis.text.x = element_text(angle = 45, vjust = .6, size=10),
        axis.text.y = element_text(size=10),
        axis.text = element_text(colour = "black"),
        axis.title.y = element_text(size = 10,vjust = 1)) +
  theme(legend.title = element_blank(),
        legend.position="none")+
  scale_fill_manual(values = c(brewer.pal(12,'Paired')[c(1)], 
                               brewer.pal(8,'Set2')[c(3)],brewer.pal(12,'Set3')[c(6,4)]))+
  ylab("AUROC") + labs(x = NULL) 
P0

ggsave("./Figures/robustness_new.png", plot = P0, 
       width = 10, height = 7, units = "cm", dpi = 300,
       device = "png", bg = "white") 


L1 <- ggplot(metric, aes(x=tech, y=AUROC, fill=Group)) + 
  geom_boxplot(varwidth = FALSE, outlier.size = 0.2, outlier.alpha = 0.8,
               notch = FALSE,notchwidth = 0.5) + 
  scale_y_continuous(limits = c(0.6, 0.9), expand = expansion(mult = c(0, 0.1))) +
  theme_base() +
  theme(axis.text.x = element_text(angle = 45, vjust = .6, size=10),
        axis.text.y = element_text(size=10),
        axis.title.y = element_text(size = 10,vjust = 1)) +
  theme(legend.title = element_blank())+
  scale_fill_manual(values = c(brewer.pal(12,'Paired')[c(1)], 
                               brewer.pal(8,'Set2')[c(3)],brewer.pal(12,'Set3')[c(6,4)]))+
  ylab("AUROC") + labs(x = NULL) 
L1

ggsave("./Figures/robustness_new.png", plot = L1, 
       width = 10, height = 7, units = "cm", dpi = 300,
       device = "png", bg = "white") 




# reorder
# sort factor
metric$Group <- factor(metric$Group, levels = c('Batch', 'BatchRemoved', 'ReplicateA','ReplicateB'))
metric <- metric[order(metric$Group),]


levels_order_t <- c("MicrowellSeq" ,"DropSeq", "MARSseq", "CELseq2","SmartSeq","SCRBseq", "SmartSeq2" )
metric$type <- factor(metric$type, levels = levels_order_t)
metric <- metric[order(metric$type),]
# metric$value <- ifelse(metric$value > 0.5, metric$value, 1 - metric$value)

dim(metric); head(metric) 