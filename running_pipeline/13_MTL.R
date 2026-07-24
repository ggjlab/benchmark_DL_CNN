rm(list=ls())
gc()
setwd("./Benchmark_MTL_MCA/")

library(ggthemes)
library(ggplot2)
library(ggpubr)

color=c(brewer.pal(12, "Set3")[-c(2,3,9,12)],"#b3b3b3",
        brewer.pal(5, "Set1")[2],
        brewer.pal(3, "Dark2")[1],
        "#fc4e2a","#fb9a99","#f781bf","#e7298a"
        )
color
############################### AUroc ##########################
metric1<-read.table("./MCA_MTL_Celltype/Test/test_mode_roc.csv", sep=",", head=T)
colnames(metric1)<-c("ID", "value")
metric1$type <- "MTL_Celltype"
head(metric1)

metric2<-read.table("./MCA_MTL_Cluster/Test/test_mode_roc.csv", sep=",", head=T)
colnames(metric2)<-c("ID", "value")
metric2$type= "MTL_Lineage"
head(metric2)

metric3<-read.table("./MCA_scMTLoss_Celltype/Test/test_mode_roc.csv", sep=",", head=T)
colnames(metric3)<-c("ID", "value")
metric3$type= "Loss_Celltype"
head(metric3)

metric4<-read.table("./MCA_scMTLoss_Cluster/Test/test_mode_roc.csv", sep=",", head=T)
colnames(metric4)<-c("ID", "value")
metric4$type= "Loss_Lineage"
head(metric4)

metric5<-read.table("./MCA_N128L15PaP15ReLU/Test/test_mode_roc.csv", sep=",", head=T)
colnames(metric5)<-c("ID", "value")
metric5$type= "Baseline"
head(metric5)

# rbind
metric <- rbind(metric1, metric2, metric3, metric4, metric5)
# sort factor
metric$type <- factor(metric$type, levels = c("Baseline", 
                                              "MTL_Lineage", "MTL_Celltype",
                                              "Loss_Lineage", "Loss_Celltype"))
metric <- metric[order(metric$type),]
dim(metric); head(metric)

####################################### Annotation ##########################################
Clu<-read.table("./MCA_cellatlas.annotation.20210125.txt", head=T, sep = "\t")
dim(Clu); Clu[1:5,]

rownames(Clu)<-Clu$Cell
Clu_use<-Clu[as.character(metric$ID),]
Clu_use[1:2,]

metric$Cluster<-Clu_use$Cellcluster
metric$Celltype<-Clu_use$Celltype

metric<-metric[metric$Cluster != "Unkown",]
metric<-metric[metric$Cluster != "Other",]
metric<-metric[!is.na(metric$Cluster),]
metric <- metric[grep("Testis_Guo", metric$Celltype, invert = T),]

p <- ggplot(metric, aes(x=Cluster, y=value, fill=type)) + 
  geom_boxplot(outlier.size = 0.2, outlier.alpha = 0.8) + 
  theme_bw() +
  theme(
    panel.grid = element_blank(),        # 移除所有网格线
    panel.background = element_blank(),  # 透明背景（或 fill = "white"）
    panel.border = element_rect(fill = NA, colour = "black")  # 保留外边框
  )+
  scale_fill_brewer(palette = 'Set3') +
  theme(axis.text.x = element_text(angle = 45, vjust = .6, size=9,face='bold'),
      axis.text.y = element_text(hjust = -0.6,size=9),
      axis.title.y = element_text(size=10),
      legend.title = element_blank(),
      legend.text = element_text(size = 8),
      legend.position="top")+
  xlab("") +
  ylab("AUROC")
p

p <- ggplot(metric, aes(x=Cluster, y=value, fill=type)) + 
  geom_boxplot(outlier.size = 0.2, outlier.alpha = 0.8) + 
  theme_bw() +
  theme(
    panel.grid = element_blank(),        # 移除所有网格线
    panel.background = element_blank(),  # 透明背景（或 fill = "white"）
    panel.border = element_rect(fill = NA, colour = "black")  # 保留外边框
  )+
  scale_fill_brewer(palette = 'Set3') +
  theme(
    # 统一X轴标签与Y轴标题的字体样式
    axis.text.x = element_text(
      angle = 45, 
      vjust = 0.6, 
      size = 9,        # 与Y轴标题大小一致
      face = "plain"    # 与Y轴标题相同的普通字体（非加粗）
    ),
    axis.title.y = element_text(size = 10),  # Y轴标题样式（保持不变）
    axis.text.y = element_text(hjust = -0.6, size = 9),
    axis.text = element_text(color = "black"),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    legend.position = "top"
  ) +
  xlab("") +
  ylab("AUROC")

# pdf("./AUROC_MCA_benchmark.pdf", width = 15, height = 6)
# p
# dev.off()

p
ggsave("./Figure5/auroc_celllineage_new.png", plot = p, 
       width = 15, height = 8, units = "cm", dpi = 300,
       device = "png", bg = "white") 






############################### AUpr ##########################
metric1<-read.table("./MCA_MTL_Celltype/Test/test_mode_pr.csv", sep=",", head=T)
colnames(metric1)<-c("ID", "value")
metric1$type <- "MTL_Celltype"
head(metric1)

metric2<-read.table("./MCA_MTL_Cluster/Test/test_mode_pr.csv", sep=",", head=T)
colnames(metric2)<-c("ID", "value")
metric2$type= "MTL_Lineage"
head(metric2)

metric3<-read.table("./MCA_scMTLoss_Celltype/Test/test_mode_pr.csv", sep=",", head=T)
colnames(metric3)<-c("ID", "value")
metric3$type= "Loss_Celltype"
head(metric3)

metric4<-read.table("./MCA_scMTLoss_Cluster/Test/test_mode_pr.csv", sep=",", head=T)
colnames(metric4)<-c("ID", "value")
metric4$type= "Loss_Lineage"
head(metric4)

metric5<-read.table("./MCA_N128L15PaP15ReLU/Test/test_mode_pr.csv", sep=",", head=T)
colnames(metric5)<-c("ID", "value")
metric5$type= "Baseline"
head(metric5)

# rbind
metric <- rbind(metric1, metric2, metric3, metric4, metric5)
# sort factor
metric$type <- factor(metric$type, levels = c("Baseline", 
                                              "MTL_Lineage", "MTL_Celltype",
                                              "Loss_Lineage", "Loss_Celltype"))
metric <- metric[order(metric$type),]
dim(metric); head(metric)

####################################### Annotation ##########################################
Clu<-read.table("./MCA_cellatlas.annotation.txt", head=T, sep = "\t")
dim(Clu); Clu[1:5,]

rownames(Clu)<-Clu$Cell
Clu_use<-Clu[as.character(metric$ID),]
Clu_use[1:2,]

metric$Cluster<-Clu_use$Cellcluster
metric$Celltype<-Clu_use$Celltype

metric<-metric[metric$Cluster != "Unkown",]
metric<-metric[metric$Cluster != "Other",]
metric<-metric[!is.na(metric$Cluster),]
metric <- metric[grep("Testis_Guo", metric$Celltype, invert = T),]

p <- ggplot(metric, aes(x=Cluster, y=value, fill=type)) + 
  geom_boxplot(outlier.size = 0.2, outlier.alpha = 0.8) + theme_base() +
  scale_fill_brewer(palette = 'Set3') +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, size=10)) + 
  xlab("") +
  ylab("AUPR")
p

pdf("./AUPR_MCA_benchmark.pdf", width = 15, height = 6)
p
dev.off()
