
rm(list=ls())
gc()
############################ （一） mESCs PCC   #######################################
setwd('./04_Benchmark_scTech_mESC_PCC_MAGIC/')
dir()
platforms <- c("MAGIC-MARSseq", 'MAGIC-MicrowellSeq',"MAGIC-DropSeq","MAGIC-10X",'MAGIC-CELseq','MAGIC-SmartSeq','MAGIC-SCRBseq','MAGIC-SmartSeq2')  
file_paths <- paste0("./mESC_", platforms, "/Test/Metric.csv")

datalist_magic_PCC <- lapply(file_paths, function(path) {
  data <- read.table(path, sep = ",", header = TRUE, row.names = 1)
  as.numeric(data[1, ]) 
})
names(datalist_magic_PCC) <- platforms 
str(datalist_magic_PCC)

#baseline
setwd('./01_Benchmark_scTech_mESC_PCC')
platforms <- c( "MARSseq", 'Microwell',"DropSeq","10X",'CELseq','SmartSeq','SCRBseq','SmartSeq2')  
file_paths <- paste0("./mESC_", platforms, "/Test/Metric.csv")

datalist_PCC <- lapply(file_paths, function(path) {
  data <- read.table(path, sep = ",", header = TRUE, row.names = 1)
  as.numeric(data[1, ]) 
})
platforms2 <- c("MARSseq", 'MicrowellSeq',"DropSeq","10X", 'CELseq','SmartSeq','SCRBseq','SmartSeq2') 
names(datalist_PCC) <- platforms2 
str(datalist_PCC)


a <-  stack(datalist_PCC)
head(a)
df_baseline_PCC <- stack(datalist_PCC) %>% 
  rename(Platform = ind, PCC = values) %>% 
  mutate(Group = "baseline")


df_magic_PCC <- stack(datalist_magic_PCC) %>% 
  rename(Platform = ind, PCC = values) %>% 
  mutate(Group = "MAGIC", Platform = gsub("MAGIC-", "", Platform))

df_combined_PCC <- rbind(df_baseline_PCC, df_magic_PCC)
head(df_combined_PCC)
rm(df_baseline_PCC,df_magic_PCC)

df_combined_PCC <- df_combined_PCC %>%
  mutate(Platform = recode(Platform,
                           "MARSseq" = "MARS-seq", 
                           'MicrowellSeq'='Microwell-seq',
                           "DropSeq"="Drop-seq",
                           'CELseq'='CEL-seq2',
                           'SmartSeq'='Smart-seq',
                           'SCRBseq'='SCRB-seq',
                           'SmartSeq2'='Smart-seq2',
                           "10X" = "10X-V2"))
table(df_combined_PCC$Platform)



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

ggsave("./FigureS2_mESCs_PCC_new.png", plot = P1, 
       width = 10, height = 7, units = "cm", dpi = 300,
       device = "png", bg = "white") 


############################  (二) mESCs AUROC   #######################################
rm(list=ls())
gc()

setwd('.//08_Benchmark_scTech_mESC_ljq_AUROC_MAGIC_cutoff_median/')
platforms <- c("MAGIC-MARSseq", 'MAGIC-MicrowellSeq',"MAGIC-DropSeq","MAGIC-10X",'MAGIC-CELseq','MAGIC-SmartSeq','MAGIC-SCRBseq','MAGIC-SmartSeq2')  # 可根据实际平台扩展
file_paths <- paste0("./mESC_", platforms, "/Test/Metric.csv")


datalist_magic_AUROC <- lapply(file_paths, function(path) {
  data <- read.table(path, sep = ",", header = TRUE, row.names = 1)
  as.numeric(data[1, ])  
})
names(datalist_magic_AUROC) <- platforms 



setwd('.//03_Benchmark_scTech_mESC_ljq_AUROC_binary')
platforms <- c("MARSseq", 'MicrowellSeq',"DropSeq","10X",'CELseq','SmartSeq','SCRBseq','SmartSeq2')  
file_paths <- paste0("./mESC_", platforms, "/Test/Metric.csv")


datalist_AUROC <- lapply(file_paths, function(path) {
  data <- read.table(path, sep = ",", header = TRUE, row.names = 1)
  as.numeric(data[1, ]) 
})
names(datalist_AUROC) <- platforms 


df_baseline_AUROC <- stack(datalist_AUROC) %>% 
  rename(Platform = ind, AUROC = values) %>% 
  mutate(Group = "baseline")

df_magic_AUROC <- stack(datalist_magic_AUROC) %>% 
  rename(Platform = ind, AUROC = values) %>% 
  mutate(Group = "MAGIC", Platform = gsub("MAGIC-", "", Platform))

df_combined_AUROC <- rbind(df_baseline_AUROC, df_magic_AUROC)
df_combined_AUROC <- df_combined_AUROC %>%
  mutate(Platform = recode(Platform,
                           "MARSseq" = "MARS-seq", 
                           'MicrowellSeq'='Microwell-seq',
                           "DropSeq"="Drop-seq",
                           'CELseq'='CEL-seq2',
                           'SmartSeq'='Smart-seq',
                           'SCRBseq'='SCRB-seq',
                           'SmartSeq2'='Smart-seq2',
                           "10X" = "10X-V2"))
rm(df_baseline_AUROC,df_magic_AUROC)

head(df_combined_AUROC)




P2 <- ggplot(df_combined_AUROC, aes(x=Platform, y=AUROC, fill=Group)) + 
  geom_boxplot() + theme_base() + theme_bw() +
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

ggsave("./Figure2/Figure2_mESCs_AUROC_new.png", plot = P2, 
       width = 10, height = 7, units = "cm", dpi = 300,
       device = "png", bg = "white") 



L2 <- ggplot(df_combined_AUROC, aes(x=Platform, y=AUROC, fill=Group)) + 
  geom_boxplot() + theme_base() + ylab("AUROC")+ labs(x = NULL)+
  scale_fill_manual(values = brewer.pal(12,'Set3')[c(5,6)]) +
  stat_compare_means(aes(group = Group), 
                     method = "wilcox.test", 
                     label = "p.signif",hide.ns = TRUE) + 
  scale_y_continuous(limits = c(0.6, 0.9), expand = expansion(mult = c(0, 0.1))) +
  theme(axis.text.x = element_text(angle = 45, vjust = .6, size=10),
        axis.text.y = element_text(hjust = -0.6,size=10),
        axis.title.y = element_text(size=10),legend.title = element_blank())
L2
ggsave("./Figures/mESCs.png", plot = L2, 
       width = 10, height = 7, units = "cm", dpi = 300,
       device = "png", bg = "white") 


