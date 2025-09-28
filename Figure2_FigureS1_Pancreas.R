############################  (五) Pancreas 583cell AUROC   #######################################
rm(list=ls())
gc()

#### 5.1 baseline
setwd('./03_BIB_pancreas_tech/')
list.files()

platforms <- c('celseq2',"inDrop1",'fluidigmc1','smarter','smartseq2')
file_paths <- paste0("./pancreas_", platforms, "/Test/Metric.csv")

datalist583_AUROC <- lapply(file_paths, function(path) {
  data <- read.table(path, sep = ",", header = TRUE, row.names = 1)
  as.numeric(data[1, ])  
})
names(datalist583_AUROC) <- platforms 

df_baseline583_AUROC <- stack(datalist583_AUROC) %>% 
  rename(Platform = ind, AUROC = values) %>% 
  mutate(Group = "baseline")


#### 5.3  583cell MAGIC CUTOFF = median= 0.13
setwd('./03_BIB_pancreas_tech/cutoff13/MAGIC')
list.files()

platforms <- c('MAGIC-celseq2',"MAGIC-inDrop1",'MAGIC-fluidigmc1','MAGIC-smarter','MAGIC-smartseq2')
file_paths <- paste0("./pancreas_", platforms, "/Test/Metric.csv")


datalist_magic_AUROC_cutoff13 <- lapply(file_paths, function(path) {
  data <- read.table(path, sep = ",", header = TRUE, row.names = 1)
  as.numeric(data[1, ]) 
})
names(datalist_magic_AUROC_cutoff13) <- platforms 



df_baseline583_AUROC <- stack(datalist583_AUROC) %>% 
  rename(Platform = ind, AUROC = values) %>% 
  mutate(Group = "baseline")

df_magic_AUROC_cutoff13 <- stack(datalist_magic_AUROC_cutoff13) %>% 
  rename(Platform = ind, AUROC = values) %>% 
  mutate(Group = "MAGIC", Platform = gsub("MAGIC-", "", Platform))

df_combined_AUROC_cutoff13 <- rbind(df_baseline583_AUROC, df_magic_AUROC_cutoff13)


df_combined_AUROC_cutoff13 <- df_combined_AUROC_cutoff13 %>%
  mutate(Platform = recode(Platform,
                           "inDrop1" = "inDrops",
                           "celseq2" = "CEL-seq2",
                           "smarter" = "SMARTer-seq",
                           "smartseq2" = "Smart-seq2",
                           "fluidigmc1"="Fluidigm-C1"))


# 1. 计算 baseline 中位数，用于排序
baseline_order <- df_combined_AUROC_cutoff13 %>% 
  filter(Group == "baseline") %>% 
  group_by(Platform) %>% 
  summarise(med = median(AUROC, na.rm = TRUE)) %>% 
  arrange(med) %>% 
  pull(Platform)
# 2. 把 Platform 变成有序因子
df_combined_AUROC_cutoff13_sort <- df_combined_AUROC_cutoff13 %>% 
  mutate(Platform = factor(Platform, levels = baseline_order))



P2 <- ggplot(df_combined_AUROC_cutoff13_sort, aes(x=Platform, y=AUROC, fill=Group)) + 
  geom_boxplot()+ theme_bw() +
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
        axis.title.y = element_text(size=10),
        axis.text = element_text(colour = "black"),
        legend.position="none",legend.title = element_blank())
P2

ggsave("./Figure2/Figure2_pancreas_AUROC_new.png", plot = P2, 
       width = 10, height = 7, units = "cm", dpi = 300,
       device = "png", bg = "white") 


############################  (六) Pancreas 583cell PCC   #######################################
rm(list=ls())
gc()

####baseline
setwd('./03_BIB_pancreas_tech/PCC')
list.files()
platforms <- c('celseq2',"inDrop1",'fluidigmc1','smarter','smartseq2')

file_paths <- paste0("./pancreas_", platforms, "/Test/Metric.csv")

datalist583_PCC <- lapply(file_paths, function(path) {
  data <- read.table(path, sep = ",", header = TRUE, row.names = 1)
  as.numeric(data[1, ])  
})
names(datalist583_PCC) <- platforms 



df_baseline583_PCC <- stack(datalist583_PCC) %>% 
  rename(Platform = ind, PCC = values) %>% 
  mutate(Group = "baseline")


####MAGIC
setwd('./03_BIB_pancreas_tech/PCC/MAGIC')
list.files()
platforms <- c('MAGIC-celseq2',"MAGIC-inDrop1",'MAGIC-fluidigmc1','MAGIC-smarter','MAGIC-smartseq2')
file_paths <- paste0("./pancreas_", platforms, "/Test/Metric.csv")


datalist_magic_PCC <- lapply(file_paths, function(path) {
  data <- read.table(path, sep = ",", header = TRUE, row.names = 1)
  as.numeric(data[1, ])  
})
names(datalist_magic_PCC) <- platforms 


df_magic583_PCC <- stack(datalist_magic_PCC) %>% 
  rename(Platform = ind, PCC = values) %>% 
  mutate(Group = "MAGIC", Platform = gsub("MAGIC-", "", Platform))

df_combined_PCC <- rbind(df_baseline583_PCC, df_magic583_PCC)
df_combined_PCC <- df_combined_PCC %>%
  mutate(Platform = recode(Platform,
                           "inDrop1" = "inDrops",
                           "celseq2" = "CEL-seq2",
                           "smarter" = "SMARTer-seq",
                           "smartseq2" = "Smart-seq2",
                           "fluidigmc1"="Fluidigm-C1"))



target_order <- levels(df_combined_AUROC_cutoff13_sort$Platform)

df_combined_PCC$Platform <- factor(
  df_combined_PCC$Platform,
  levels = target_order
)


P1 <- ggplot(df_combined_PCC, aes(x=Platform, y=PCC, fill=Group)) + 
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

ggsave("./FigureS2_pancrese_PCC_new.png", plot = P1, 
       width = 10, height = 7, units = "cm", dpi = 300,
       device = "png", bg = "white") 




