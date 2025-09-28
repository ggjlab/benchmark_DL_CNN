rm(list=ls())
gc()
library(ggplot2)
library(ggpubr)  
library(harmony)
library(org.Hs.eg.db)
library(clusterProfiler)
library(enrichplot)
library(org.Mm.eg.db)

setwd('./01_benmark_data/d2_MCL')
pred <- read.table('pred3.tsv',sep = '\t',header = T)
head(pred)
pred <- pred[order(-pred$average),]

go_enrich <- enrichGO(gene = pred$X,
                      OrgDb = org.Mm.eg.db,
                      keyType = "SYMBOL",
                      ont = "ALL",  # 可选"BP"/"MF"/"CC"
                      pAdjustMethod = "BH",
                      pvalueCutoff = 0.05,
                      qvalueCutoff = 0.2,
                      readable = TRUE)  
head(go_enrich)


# 修改后的代码
P21 <- barplot(go_enrich, showCategory = 20, 
               font.size = 12 
) +
  theme_bw() + 
  theme(panel.grid = element_blank(),      
        panel.background = element_blank(),  
        panel.border = element_rect(fill = NA, colour = "black")  
  ) +
  theme(legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        axis.text.y = element_text(size = 12, hjust = 1) 
        #plot.margin = unit(c(1, 1, 1, 3), "lines") 
  ) + scale_y_discrete(labels=function(x) str_wrap(x,width = 50))+
  geom_bar(stat = "identity", width = 0.6) 
P21 + scale_y_discrete(labels=function(x) str_wrap(x,width = 60))

P21



pdf("./Figures/FigureS7.pdf", 
    width = 12, 
    height = 11,
    family = "Times", # 或 "Times"
    pointsize = 12)       # 设置基础字体大小

# 你的绘图代码
barplot(go_enrich, showCategory = 20, font.size = 16) +
  theme_bw() + 
  theme(panel.grid = element_blank(),      
        panel.background = element_blank(),  
        panel.border = element_rect(fill = NA, colour = "black"),
        legend.text = element_text(size = 16),
        axis.text.y = element_text(size = 18, hjust = 1), 
        plot.margin = unit(c(1, 1, 1, 3), "lines")) + 
  scale_y_discrete(labels = function(x) str_wrap(x, width = 60)) +
  geom_bar(stat = "identity", width = 0.6)

dev.off()






np <- read.table('./01_benmark_data/01_run/NP.csv',sep=',',header = TRUE,row.names = 1)
np2 <- data.frame(t(np))
np2$sample <- rownames(np2)
np2 = np2[c(1:2,5,7,8,10),]
np2$Sample <- c("mESCs", "mESCs.MAGIC",
                "PBMC", "PBMC.MAGIC",
                "Pancreas", "Pancreas.MAGIC")
np2
np2 <- np2 %>%
  pivot_longer(cols = c(N_0, P_1), names_to = "category", values_to = "count") %>%
  group_by(Sample) %>%
  mutate(
    proportion = count / sum(count, na.rm = TRUE),  # 计算比例
    label = sprintf("%.1f%%", proportion * 100)     # 格式化标签
  ) 

ggplot(np2, aes(x = Sample, y = proportion, fill = category)) +
  geom_col(position = "stack", width = 0.7) +  # 堆叠柱状图
  geom_text(
    aes(label = label), 
    position = position_stack(vjust = 0.5),  # 标签居中
    size = 4, color = "white"
  ) +labs(title = "Proportions of N_0 and P_1", y = NULL, x = NULL) +
  scale_fill_manual(values = c("#76afda", "#eb998b")) +  # 自定义颜色
  scale_y_continuous(labels = scales::percent) +         # y轴显示百分比
  theme_minimal() +
  theme(axis.text.x = element_text(size=12,angle=45,hjust=1),
        axis.title.y = element_text(size = 12),legend.position = "top")


ggplot(df_combined_PCC, aes(x=Platform, y=PCC, fill=Group)) + 
  geom_boxplot() + theme_base() + ylab("PCC")+ labs(x = NULL)+
  scale_fill_manual(values = brewer.pal(12,'Set3')[c(5,6)]) +
  stat_compare_means(aes(group = Group), 
                     method = "wilcox.test", 
                     label = "p.signif",
                     hide.ns = TRUE) + ylim(0.3,0.75)+
  theme(axis.text.x = element_text(angle = 45, vjust = .6, size=12),
        axis.text.y = element_text(hjust = -0.6,size=12), 
        legend.position="right",legend.title = element_blank())
dev.off()