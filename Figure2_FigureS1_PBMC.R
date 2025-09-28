############################  (四) PBMC 583cell AUROC   #######################################
rm(list=ls())
gc()
#### 4.1 baseline
setwd('./02_BIB_PBMC_tech/d3_baseline_cutoff001_583cells/')
list.files()
platforms <- c("10x-Chromium-V2", "10x-Chromium-v3", 'CEL-Seq2',"Drop-seq",'inDrops','Seq-Well','Smart-seq2')
file_paths <- paste0("./pbmc_", platforms, "/Test/Metric.csv")

datalist583_AUROC_pbmc <- lapply(file_paths, function(path) {
  data <- read.table(path, sep = ",", header = TRUE, row.names = 1)
  as.numeric(data[1, ])  
})
names(datalist583_AUROC_pbmc) <- platforms 


df_baseline583_AUROC_pbmc <- stack(datalist583_AUROC_pbmc) %>% 
  rename(Platform = ind, AUROC = values) %>% 
  mutate(Group = "baseline")
head(df_baseline583_AUROC_pbmc)


# #### 4.2 MAGIC
#### 5.3  583cell MAGIC CUTOFF = median= 0.13
setwd('/home/ggj/Galaxy/01_benmark_data/01_run/02_BIB_PBMC_tech/d5_MAGIC_cutoff03_583cells/')
list.files()
platforms <- c("MAGIC-10x-Chromium-V2", "MAGIC-10x-Chromium-v3", 'MAGIC-CEL-Seq2',"MAGIC-Drop-seq",'MAGIC-inDrops','MAGIC-Seq-Well','MAGIC-Smart-seq2')
file_paths <- paste0("./pbmc_", platforms, "/Test/Metric.csv")



datalist_magic_AUROC_cutoff03 <- lapply(file_paths, function(path) {
  data <- read.table(path, sep = ",", header = TRUE, row.names = 1)
  as.numeric(data[1, ])  
})
names(datalist_magic_AUROC_cutoff03) <- platforms 

df_magic583_AUROC <- stack(datalist_magic_AUROC_cutoff03) %>% 
  rename(Platform = ind, AUROC = values) %>% 
  mutate(Group = "MAGIC", Platform = gsub("MAGIC-", "", Platform))

df_combined_AUROC <- rbind(df_baseline583_AUROC_pbmc, df_magic583_AUROC)


head(df_combined_AUROC)
baseline_order <- df_combined_AUROC %>% filter(Group == "baseline") %>% 
  group_by(Platform) %>% 
  summarise(med = median(AUROC, na.rm = TRUE)) %>% 
  arrange(med) %>% 
  pull(Platform)

df_combined_AUROC_pbmc_cutoff03_sort <- df_combined_AUROC %>% 
  mutate(Platform = factor(Platform, levels = baseline_order))
df_combined_AUROC_pbmc_cutoff03_sort <- df_combined_AUROC_pbmc_cutoff03_sort %>%
  mutate(Platform = recode(Platform,
                           "10x-Chromium-V2" = "10X-V2",
                           "10x-Chromium-v3" = "10X-V3"))


P2 <- ggplot(df_combined_AUROC_pbmc_cutoff03_sort, aes(x=Platform, y=AUROC, fill=Group)) + 
  geom_boxplot() + theme_bw() +
  theme(panel.grid = element_blank(),       
        panel.background = element_blank(),  
        panel.border = element_rect(fill = NA, colour = "black"))+
  ylab("AUROC")+ labs(x = NULL)+
  scale_fill_manual(values = brewer.pal(12,'Set3')[c(5,6)]) +
  stat_compare_means(aes(group = Group), 
                     method = "wilcox.test", 
                     label = "p.signif",hide.ns = TRUE) + 
  scale_y_continuous(limits = c(0.6, 0.9), expand = expansion(mult = c(0, 0.1))) +
  theme(axis.text.x = element_text(angle = 45, vjust = .6, size=10),
        axis.text.y = element_text(hjust = -0.6,size=10),
        axis.text = element_text(colour = "black"),
        axis.title.y = element_text(size=10),
        legend.position="none",legend.title = element_blank())
P2

ggsave("./Figure2/Figure2_pbmc_AUROC_new.png", plot = P2, 
       width = 10, height = 7, units = "cm", dpi = 300,
       device = "png", bg = "white") 

############################  (七) PBMC 583cell PCC   #######################################
rm(list=ls())
gc()
####baseline
setwd('./02_BIB_PBMC_tech/d4_PCC_baseline_PBMC_583cells/')
list.files()

platforms <- c("10x-Chromium-V2", "10x-Chromium-v3", 'CEL-Seq2',"Drop-seq",'inDrops','Seq-Well','Smart-seq2')
file_paths <- paste0("./pbmc_", platforms, "/Test/Metric.csv")
# 批量读取数据并提取第一行（PCC值）
datalist583_PCC_pbmc <- lapply(file_paths, function(path) {
  data <- read.table(path, sep = ",", header = TRUE, row.names = 1)
  as.numeric(data[1, ])  
})
names(datalist583_PCC_pbmc) <- platforms 


df_baseline583_PCC_pbmc <- stack(datalist583_PCC_pbmc) %>% 
  rename(Platform = ind, PCC = values) %>% 
  mutate(Group = "baseline")

####MAGIC
setwd('./02_BIB_PBMC_tech/d6_PCC_MAGIC_PBMC_583cell')
list.files()
platforms <- c("MAGIC-10x-Chromium-V2", "MAGIC-10x-Chromium-v3", 'MAGIC-CEL-Seq2',"MAGIC-Drop-seq",'MAGIC-inDrops','MAGIC-Seq-Well','MAGIC-Smart-seq2')

file_paths <- paste0("./pbmc_", platforms, "/Test/Metric.csv")


datalist_magic_PCC <- lapply(file_paths, function(path) {
  data <- read.table(path, sep = ",", header = TRUE, row.names = 1)
  as.numeric(data[1, ])  
})
names(datalist_magic_PCC) <- platforms 


df_magic583_PCC <- stack(datalist_magic_PCC) %>% 
  rename(Platform = ind, PCC = values) %>% 
  mutate(Group = "MAGIC", Platform = gsub("MAGIC-", "", Platform))


df_combined_PCC_pbmc <- rbind(df_baseline583_PCC_pbmc, df_magic583_PCC)


df_combined_PCC_pbmc <- df_combined_PCC_pbmc %>%
  mutate(Platform = recode(Platform,
                           "10x-Chromium-V2" = "10X-V2",
                           "10x-Chromium-v3" = "10X-V3"))


table(df_combined_PCC_pbmc$Platform)


target_order <- levels(df_combined_AUROC_pbmc_cutoff03_sort$Platform)
df_combined_PCC_pbmc$Platform <- factor(
  df_combined_PCC_pbmc$Platform,
  levels = target_order
)




P1 <- ggplot(df_combined_PCC_pbmc, aes(x=Platform, y=PCC, fill=Group)) + 
  geom_boxplot() + theme_base() + ylab("PCC")+ labs(x = NULL)+
  scale_fill_manual(values = brewer.pal(12,'Set3')[c(5,6)]) +
  stat_compare_means(aes(group = Group), 
                     method = "wilcox.test", 
                     label = "p.signif",hide.ns = TRUE) + 
  scale_y_continuous(limits = c(0.0, 0.71), expand = expansion(mult = c(0, 0.1))) +
  theme(axis.text.x = element_text(angle = 45, vjust = .6, size=10),
        axis.text.y = element_text(hjust = -0.6,size=10),
        axis.text = element_text(colour = "black"),
        axis.title.y = element_text(size=10),
        legend.position="none",legend.title = element_blank())
P1

ggsave("./Figures/FigureS2_pbmc_PCC_new.png", plot = P1, 
       width = 10, height = 7, units = "cm", dpi = 300,
       device = "png", bg = "white") 


################################################################################
##################################################################################
np <- read.table('./01_run/NP.csv',sep=',',header = TRUE,row.names = 1)
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
    proportion = count / sum(count, na.rm = TRUE),  
    label = sprintf("%.1f%%", proportion * 100)    
  ) 


P3 <- ggplot(np2, aes(x = Sample, y = proportion, fill = category)) +
  geom_col(position = "stack", width = 0.7) + 
  geom_text(
    aes(label = label), 
    position = position_stack(vjust = 0.5),  
    size = 2, color = "black"
  ) +labs(title = NULL,  x = NULL) +
  theme_bw()  +
  theme(panel.grid = element_blank(),        
    panel.background = element_blank(),  
    panel.border = element_rect(fill = NA, colour = "black")  
    )+
  ylab("Sample ratio")+ 
  scale_fill_manual(values  = brewer.pal(12,'Set3')[c(5,6)],labels=c("N_0"="Negative sample","P_1"="Positive sample")) + 
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 45, vjust = .6, size=10),
        axis.title.y = element_text(size=12,vjust = 0.6),
        legend.position="none",legend.title = element_blank())

P3
ggsave("./Figures/FigureS2_ratio2.png", plot = P3, 
       width = 10, height = 6.3, units = "cm", dpi = 300,
       device = "png", bg = "white") 
