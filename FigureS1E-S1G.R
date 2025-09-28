library(dplyr)
rm(list=ls())
gc()


load("./d3_DetectedGene/PCC_detectedgene.RData")
setwd('./d3_DetectedGene')
################################### 
head(combined_data_mESCs,2)
combined_data_mESCs$group <- paste0('mESCs','_',combined_data_mESCs$Group)
combined_data_PBMC$group <- paste0('PBMC','_',combined_data_PBMC$Group)
combined_data_pancreas$group <- paste0('Pancreas','_',combined_data_pancreas$Group)
combined_data_total <- rbind(combined_data_mESCs,combined_data_PBMC,combined_data_pancreas)


combined_data_total <- combined_data_total %>%
  mutate(
    dataset = gsub("_.*", "", group),  
    treatment = ifelse(grepl("MAGIC", group), "MAGIC", "baseline") 
  )

head(combined_data_total)

dataset_colors <- c("mESCs" = "#1f77b4", "Pancreas" = "#ff7f0e", "PBMC" = "#2ca02c") 
treatment_colors <- c("baseline" = "gray30", "MAGIC" = "#F06152")  
treatment_colors <- c("baseline" = "gray60", "MAGIC" = "gray10")  

P0 <- ggplot(combined_data_total, aes(x = mean_genes, y = mean_PCC)) +
  geom_smooth(
    aes(group = dataset, fill = dataset),
    method = "lm", se = TRUE, alpha = 0.2, color = NA
  ) +
  geom_point(
    aes(fill = dataset, color = treatment), 
    shape = 21,             
    size = 2, 
    alpha = 0.8,
    stroke = 1.2             
  ) +
  geom_smooth(
    aes(
      group = interaction(dataset, treatment),
      color = treatment,
      linetype = treatment
    ),
    method = "lm", se = FALSE, linewidth = 1
  ) +
  scale_fill_manual(   
    values = dataset_colors, 
    name = "Dataset"
  ) +
  scale_color_manual( 
    values = treatment_colors, 
    name = "Treatment"
  ) +
  scale_linetype_manual(
    values = c("baseline" = "solid", "MAGIC" = "dashed"),
    name = "Treatment"
  ) +
  labs(
    x = "Detected Genes per Platform",
    y = "PCC per Platform",
    #title = "Gene Detection vs Model Performance by Dataset"
  ) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        axis.title = element_text(size=10),
        legend.text = element_text(size = 8),    

P0
ggsave("./Figures/DETECTED_GENE2.png", plot = P0, 
       width = 10, height = 7.1, units = "cm", dpi = 300,
       device = "png", bg = "white") 

theme(axis.text.x = element_text(angle = 45, vjust = .6, size=10),
      axis.text.y = element_text(hjust = -0.6,size=10),
      axis.title.y = element_text(size=10),
      legend.position="none",legend.title = element_blank())
###################################### (一) ######################################
df_combined_PCC_mESCs <- read.table("./d3_DetectedGene/df_combined_PCC_mESCs.csv",header = TRUE,sep=',')
t1 <- read.table("./d3_DetectedGene/annotation_mESCs.tsv",header = TRUE,sep=',')
t2 <- read.table("./d3_DetectedGene/annotation_mESCs_magic_median.tsv",header = TRUE,sep=',')

#t1$Platform <- t1$Cluster
t1 <- t1[-c(1,2),] %>% 
  rename(Platform = Cluster) %>% 
  mutate(Group = "baseline")
t2 <- t2 %>% 
  rename(Platform = Cluster) %>% 
  mutate(Group = "MAGIC", Platform = gsub("MAGIC-", "", Platform))
t <- rbind(t1,t2)
head(t)
t <- t[,4:7]
head(df_combined_PCC_mESCs)
table(df_combined_PCC_mESCs$Platform)
table(t$Platform)
t <- t %>%
  mutate(Platform = recode(Platform,
                           "MARSseq" = "MARS-seq", 
                           'MicrowellSeq'='Microwell-seq',
                           "DropSeq"="Drop-seq",
                           'CELseq'='CEL-seq2',
                           'SmartSeq'='Smart-seq',
                           'SCRBseq'='SCRB-seq',
                           'SmartSeq2'='Smart-seq2',
                           "10X" = "10X-V2"))

# (一) mESCs

t_summary <- t %>%
  group_by(Platform, Group) %>%
  summarise(mean_genes = mean(gene_detected_PCC, na.rm = TRUE))


df_summary <- df_combined_PCC_mESCs %>%
  group_by(Platform, Group) %>%
  summarise(mean_PCC = mean(PCC, na.rm = TRUE))


combined_data_mESCs <- inner_join(t_summary, df_summary, by = c("Platform", "Group"))
head(combined_data_mESCs)


cor_baseline <- cor.test(
  combined_data_mESCs %>% filter(Group == "baseline") %>% pull(mean_genes),
  combined_data_mESCs %>% filter(Group == "baseline") %>% pull(mean_PCC),
  method = "pearson"
)

cor_MAGIC <- cor.test(
  combined_data_mESCs %>% filter(Group == "MAGIC") %>% pull(mean_genes),
  combined_data_mESCs %>% filter(Group == "MAGIC") %>% pull(mean_PCC),
  method = "pearson"
)


print(paste("Baseline组: ρ =", round(cor_baseline$estimate, 3), "p =", cor_baseline$p.value))
print(paste("MAGIC组: ρ =", round(cor_MAGIC$estimate, 3), "p =", cor_MAGIC$p.value))

P1 <- ggplot(combined_data_mESCs, aes(x = mean_genes, y = mean_PCC)) +
  geom_point(aes(color = Group, shape = Platform), size = 3) +
  geom_smooth(
    aes(group = Group, color = Group, fill = Group),
    method = "lm", 
    se = TRUE, 
    linetype = "dashed", 
    alpha = 0.2  
  ) +
  scale_color_manual(
    values = c("baseline" = "#1f77b4", "MAGIC" = "#ff7f0e")  
  ) +
  scale_fill_manual(
    values = c("baseline" = "#a6c6e3", "MAGIC" = "#ffd699") 
  ) +
  scale_shape_manual(values = c(16, 17, 18, 15, 3,7,10,11)) +  
  labs(
    x = "Detected Genes per Platform",
    y = "PCC per Platform",
    title = "mESCs",
    subtitle = paste(
      "Baseline: r =", round(cor_baseline$estimate, 3), 
      "(p =", format(cor_baseline$p.value, scientific = TRUE, digits = 2), ") \n ",
      "MAGIC: r =", round(cor_MAGIC$estimate, 3),
      "(p =", format(cor_MAGIC$p.value, scientific = TRUE, digits = 2), ")"
    )
  ) +
  theme_bw() +
  theme(
    legend.position = "right",
    #legend.position.inside = c(0.85, 0.15),
    legend.box = "vertical", 
    legend.title = element_blank()  
  )

P1
ggsave("./Figures/Detectedgene_mESCs_PCC.png", plot = P1, 
       width = 10, height = 9, units = "cm", dpi = 300,
       device = "png", bg = "white") 



###################################(二) PBMC ######################################
## PBMCs
library(dplyr)
rm(list=ls())
gc()

df_combined_PCC_PBMC <- read.table("./d3_DetectedGene/df_combined_PCC_PBMC.csv",header = TRUE,sep=',')
head(df_combined_PCC_PBMC)
t1 <- read.table("./d3_DetectedGene/annotation_PBMC.tsv",header = TRUE,sep=',')
t2 <- read.table("./d3_DetectedGene/annotation_PBMC_magic.tsv",header = TRUE,sep=',')

#t1$Platform <- t1$Cluster
t1 <- t1 %>% 
  rename(Platform = Cluster) %>% 
  mutate(Group = "baseline")
t2 <- t2 %>% 
  rename(Platform = Cluster) %>% 
  mutate(Group = "MAGIC", Platform = gsub("MAGIC-", "", Platform))
t <- rbind(t1,t2)
head(t)
t <- t[,4:7]

t_summary <- t %>%
  group_by(Platform, Group) %>%
  summarise(mean_genes = mean(gene_detected_PCC, na.rm = TRUE))

head(df_combined_PCC_PBMC)
df_summary <- df_combined_PCC_PBMC %>%
  group_by(Platform, Group) %>%
  summarise(mean_PCC = mean(PCC, na.rm = TRUE))

df_summary <- df_combined_PCC_PBMC %>%
  group_by(Platform, Group) %>%
  summarise(mean_PCC = mean(PCC, na.rm = TRUE))

combined_data_PBMC <- inner_join(t_summary, df_summary, by = c("Platform", "Group"))
head(combined_data_PBMC)


cor_baseline_PBMC <- cor.test(
  combined_data_PBMC %>% filter(Group == "baseline") %>% pull(mean_genes),
  combined_data_PBMC %>% filter(Group == "baseline") %>% pull(mean_PCC),
  method = "pearson"
)

cor_MAGIC_PBMC <- cor.test(
  combined_data_PBMC %>% filter(Group == "MAGIC") %>% pull(mean_genes),
  combined_data_PBMC %>% filter(Group == "MAGIC") %>% pull(mean_PCC),
  method = "pearson"
)

print(paste("Baseline组: r =", round(cor_baseline_PBMC$estimate, 3), "p =", cor_baseline_PBMC$p.value))
print(paste("MAGIC组: r =", round(cor_MAGIC_PBMC$estimate, 3), "p =", cor_MAGIC_PBMC$p.value))

combined_data_PBMC <- combined_data_PBMC %>%
  mutate(Platform = recode(Platform,
                           "10x-Chromium-V2" = "10X-V2",
                           "10x-Chromium-v3" = "10X-V3"))
P2 <- ggplot(combined_data_PBMC, aes(x = mean_genes, y = mean_PCC)) +
  geom_point(aes(color = Group, shape = Platform), size = 3) +
  geom_smooth(
    aes(group = Group, color = Group, fill = Group),
    method = "lm", 
    se = TRUE, 
    linetype = "dashed", 
    alpha = 0.2  
  ) +
  scale_color_manual(
    values = c("baseline" = "#1f77b4", "MAGIC" = "#ff7f0e") 
  ) +
  scale_fill_manual(
    values = c("baseline" = "#a6c6e3", "MAGIC" = "#ffd699") 
  ) +
  scale_shape_manual(values = c(16, 15,17, 18,3,10,11)) +  
  labs(
    x = "Detected Genes per Platform",
    y = "PCC per Platform",
    title = "PBMCs",
    subtitle = paste(
      "Baseline: r =", round(cor_baseline_PBMC$estimate, 3), 
      "(p =", format(cor_baseline_PBMC$p.value, scientific = TRUE, digits = 2), ") \n ",
      "MAGIC: r =", round(cor_MAGIC_PBMC$estimate, 3),
      "(p =", format(cor_MAGIC_PBMC$p.value, scientific = TRUE, digits = 2), ")"
    )
  ) +
  theme_bw() +
  theme(
    legend.position = "right",
    #legend.position.inside = c(0.85, 0.15),
    legend.box = "vertical",  
    legend.title = element_blank() 
  )
P2
ggsave("./Figures/Detectedgene_pbmc_PCC.png", plot = P2, 
       width = 10, height = 9, units = "cm", dpi = 300,
       device = "png", bg = "white") 

###########################################(三)### pancreas ######################################
### pancreas
df_combined_PCC_pancreas <- read.table("./d3_DetectedGene/df_combined_PCC_pancreas.csv",header = TRUE,sep=',')

t1 <- read.table("./d3_DetectedGene/annotation_Pancreas.tsv",header = TRUE,sep=',')
t2 <- read.table("./d3_DetectedGene/annotation_Pancreas_magic.tsv",header = TRUE,sep=',')

#t1$Platform <- t1$Cluster
t1 <- t1 %>% 
  rename(Platform = Cluster) %>% 
  mutate(Group = "baseline")
t2 <- t2 %>% 
  rename(Platform = Cluster) %>% 
  mutate(Group = "MAGIC", Platform = gsub("MAGIC-", "", Platform))
t <- rbind(t1,t2)
t <- t[,4:7]
head(t)
head(df_combined_PCC_pancreas)

t_summary <- t %>%
  group_by(Platform, Group) %>%
  summarise(mean_genes = mean(gene_detected_PCC, na.rm = TRUE))

table(t_summary$Platform)
table(df_combined_PCC_pancreas$Platform)

t_summary <- t_summary %>%
  mutate(Platform = recode(Platform,
                           "inDrop" = "inDrops",
                           "celseq" = "CEL-seq2",
                           "smarter" = "SMARTer-seq",
                           "smartseq2" = "Smart-seq2",
                           "fluidigmc1"="Fluidigm-C1"))


df_summary <- df_combined_PCC_pancreas %>%
  group_by(Platform, Group) %>%
  summarise(mean_PCC = mean(PCC, na.rm = TRUE))


combined_data_pancreas <- inner_join(t_summary, df_summary, by = c("Platform", "Group"))

cor_baseline_pancreas <- cor.test(
  combined_data_pancreas %>% filter(Group == "baseline") %>% pull(mean_genes),
  combined_data_pancreas %>% filter(Group == "baseline") %>% pull(mean_PCC),
  method = "pearson"
)

cor_MAGIC_pancreas <- cor.test(
  combined_data_pancreas %>% filter(Group == "MAGIC") %>% pull(mean_genes),
  combined_data_pancreas %>% filter(Group == "MAGIC") %>% pull(mean_PCC),
  method = "pearson"
)

print(paste("Baseline组: ρ =", round(cor_baseline_pancreas$estimate, 3), "p =", cor_baseline_pancreas$p.value))
print(paste("MAGIC组: ρ =", round(cor_MAGIC_pancreas$estimate, 3), "p =", cor_MAGIC_pancreas$p.value))
head(combined_data_pancreas,3)


P3 <- ggplot(combined_data_pancreas, aes(x = mean_genes, y = mean_PCC)) +
  geom_point(aes(color = Group, shape = Platform), size = 3) +
  geom_smooth(
    aes(group = Group, color = Group, fill = Group),
    method = "lm", 
    se = TRUE, 
    linetype = "dashed", 
    alpha = 0.2 
  ) +
  scale_color_manual(
    values = c("baseline" = "#1f77b4", "MAGIC" = "#ff7f0e")  
  ) +
  scale_fill_manual(
    values = c("baseline" = "#a6c6e3", "MAGIC" = "#ffd699")  
  ) +
  scale_shape_manual(values = c(17, 16, 3, 11, 18)) + 
  labs(
    x = "Detected Genes per Platform",
    y = "PCC per Platform",
    title = "Pancreas",
    subtitle = paste(
      "Baseline: r =", round(cor_baseline_pancreas$estimate, 3), 
      "(p =", format(cor_baseline_pancreas$p.value, scientific = TRUE, digits = 2), ") \n ",
      "MAGIC: r =", round(cor_MAGIC_pancreas$estimate, 3),
      "(p =", format(cor_MAGIC_pancreas$p.value, scientific = TRUE, digits = 2), ")"
    )
  ) +
  theme_bw() +
  theme(
    legend.position = "right",
    #legend.position.inside = c(0.85, 0.15),
    legend.box = "vertical",  
    legend.title = element_blank()  
  )
P3
ggsave("./Figures/Detectedgene_pancreas_PCC.png", plot = P3, 
       width = 10, height = 9, units = "cm", dpi = 300,
       device = "png", bg = "white") 

