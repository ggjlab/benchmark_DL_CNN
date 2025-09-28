rm(list=ls())
gc()
setwd("./scATAC_cnn_parameters_100cells")
library(ggthemes)
library(ggplot2)
library(ggpubr)

############## Group ##############
# Baseline
level_order <- c('scATAC_N128L15PaP15ReLU')
# Filter Number (N)
level_order <- c('scATAC_N8L15PaP15ReLU', 'scATAC_N32L15PaP15ReLU','scATAC_N512L15PaP15ReLU')
# Filter Length (L)
level_order <- c('scATAC_N128L5PaP15ReLU', 'scATAC_N128L25PaP15ReLU')
# Pooling layer (Pa/Pm)
level_order <- c('scATAC_N128L15PmP15ReLU')
# Pooling size (P)
level_order <- c('scATAC_N128L15PaP5ReLU','scATAC_N128L15PaP25ReLU')
# Activation Function
level_order <- c('scATAC_N128L15PaP15Sigmoid','scATAC_N128L15PaP15Exp','scATAC_N128L15PaP15Tanh')
# Batch Normalization
level_order <- c('scATAC_N128L15PaP15ReLUBN','scATAC_N128L15PaP15SigmoidBN','scATAC_N128L15PaP15ExpBN','scATAC_N128L15PaP15TanhBN')

###########################################
if(T){
  # Hyper-parameter: Pa/Pm, N, L, P, Activation Function, Batch Normalization(8)
  # Metric1
  # 1 Pooling layer (Pa/Pm)
  level_order <- c('scATAC_N128L15PaP15ReLU','scATAC_N128L15PmP15ReLU')
  metric1.fpaths <- paste0(level_order, "/Test/Metric.csv")
  metric1.fpaths <- metric1.fpaths[file.exists(metric1.fpaths)]
  metric1.list <- lapply(metric1.fpaths, function(metric1.fname){
    print(metric1.fname)
    metric1 <- as.data.frame(t(read.csv(metric1.fname, header=T, row.names = 1)))
    metric1$type <- strsplit(metric1.fname, '/')[[1]][1]
    return(metric1)
  })
  metric1 <- do.call(rbind, metric1.list)
  
  # sort factor
  metric1$type <- factor(metric1$type, levels = level_order)
  metric1 <- metric1[order(metric1$type),]
  metric1$Group <- 'Pooling Layer'
  
  tmp <- gsub('scATAC_N128L15','',metric1$type)
  metric1$Type <- gsub('P15ReLU','',tmp)
  dim(metric1);table(metric1$Type); head(metric1)
  
  # metric2
  # 2Filter Number (N)
  level_order <- c('scATAC_N8L15PaP15ReLU','scATAC_N32L15PaP15ReLU','scATAC_N512L15PaP15ReLU','scATAC_N128L15PaP15ReLU')
  
  metric2.fpaths <- paste0(level_order, "/Test/Metric.csv")
  metric2.fpaths <- metric2.fpaths[file.exists(metric2.fpaths)]
  metric2.list <- lapply(metric2.fpaths, function(metric2.fname){
    print(metric2.fname)
    metric2 <- as.data.frame(t(read.csv(metric2.fname, header=T, row.names = 1)))
    metric2$type <- strsplit(metric2.fname, '/')[[1]][1]
    return(metric2)
  })
  metric2 <- do.call(rbind, metric2.list)
  
  # sort factor
  metric2$type <- factor(metric2$type, levels = level_order)
  metric2 <- metric2[order(metric2$type),]
  metric2$Group <- 'Filter Number'
  tmp <- gsub('scATAC_','',metric2$type)
  metric2$Type <- gsub('L15PaP15ReLU','',tmp)
  dim(metric2);table(metric2$Type); head(metric2)
  
  # metric3
  # 3 Filter Length (L)
  level_order <- c( 'scATAC_N128L25PaP15ReLU','scATAC_N128L15PaP15ReLU','scATAC_N128L5PaP15ReLU')
  
  metric3.fpaths <- paste0(level_order, "/Test/Metric.csv")
  metric3.fpaths <- metric3.fpaths[file.exists(metric3.fpaths)]
  metric3.list <- lapply(metric3.fpaths, function(metric3.fname){
    print(metric3.fname)
    metric3 <- as.data.frame(t(read.csv(metric3.fname, header=T, row.names = 1)))
    metric3$type <- strsplit(metric3.fname, '/')[[1]][1]
    return(metric3)
  })
  metric3 <- do.call(rbind, metric3.list)
  
  # sort factor
  metric3$type <- factor(metric3$type, levels = level_order)
  
  metric3 <- metric3[order(metric3$type),]
  
  metric3$Group <- 'Filter Length'
  
  tmp <- gsub('scATAC_N128','',metric3$type)
  metric3$Type <- gsub('PaP15ReLU','',tmp)
  dim(metric3);table(metric3$Type); head(metric3)
  
  
  # metric4
  # Pooling size (P)
  level_order <- c('scATAC_N128L15PaP5ReLU','scATAC_N128L15PaP15ReLU','scATAC_N128L15PaP25ReLU')
  
  metric4.fpaths <- paste0(level_order, "/Test/Metric.csv")
  metric4.fpaths <- metric4.fpaths[file.exists(metric4.fpaths)]
  metric4.list <- lapply(metric4.fpaths, function(metric4.fname){
    print(metric4.fname)
    metric4 <- as.data.frame(t(read.csv(metric4.fname, header=T, row.names = 1)))
    metric4$type <- strsplit(metric4.fname, '/')[[1]][1]
    return(metric4)
  })
  metric4 <- do.call(rbind, metric4.list)
  
  # sort factor
  metric4$type <- factor(metric4$type, levels = level_order)
  
  metric4 <- metric4[order(metric4$type),]
  
  metric4$Group <- 'Pooling Size'
  tmp <- gsub('scATAC_N128L15Pa','',metric4$type)
  metric4$Type <- gsub('ReLU','',tmp)
  dim(metric4);table(metric4$Type); head(metric4)
  
  # metric5
  # Activation Function
  level_order <- c('scATAC_N128L15PaP15Sigmoid','scATAC_N128L15PaP15Tanh','scATAC_N128L15PaP15Exp','scATAC_N128L15PaP15ReLU')
  
  metric5.fpaths <- paste0(level_order, "/Test/Metric.csv")
  metric5.fpaths <- metric5.fpaths[file.exists(metric5.fpaths)]
  metric5.list <- lapply(metric5.fpaths, function(metric5.fname){
    print(metric5.fname)
    metric5 <- as.data.frame(t(read.csv(metric5.fname, header=T, row.names = 1)))
    metric5$type <- strsplit(metric5.fname, '/')[[1]][1]
    return(metric5)
  })
  metric5 <- do.call(rbind, metric5.list)
  
  # sort factor
  metric5$type <- factor(metric5$type, levels = level_order)
  
  metric5 <- metric5[order(metric5$type),]
  
  metric5$Group <- 'Activation Function'
  metric5$Type <- gsub('scATAC_N128L15PaP15','',metric5$type)
  dim(metric5);table(metric5$Type); head(metric5)
  
  #  # metric6
  # Batch Normalization
  level_order <- c('scATAC_N128L15PaP15SigmoidBN',
                   'scATAC_N128L15PaP15TanhBN',
                   'scATAC_N128L15PaP15ExpBN',
                   'scATAC_N128L15PaP15ReLUBN')
  
  metric6.fpaths <- paste0(level_order, "/Test/Metric.csv")
  metric6.fpaths <- metric6.fpaths[file.exists(metric6.fpaths)]
  metric6.list <- lapply(metric6.fpaths, function(metric6.fname){
    print(metric6.fname)
    metric6 <- as.data.frame(t(read.csv(metric6.fname, header=T, row.names = 1)))
    metric6$type <- strsplit(metric6.fname, '/')[[1]][1]
    return(metric6)
  })
  metric6 <- do.call(rbind, metric6.list)
  
  # sort factor
  metric6$type <- factor(metric6$type, levels = level_order)
  metric6 <- metric6[order(metric6$type),]
  
  metric6$Group <- 'Batch Normalization'
  metric6$Type <- gsub('scATAC_N128L15PaP15','',metric6$type)
  dim(metric6);table(metric6$Type) ;head(metric6)
}

################# merge ############################
metric_total <- rbind(metric2, metric3, metric1, metric4, metric5, metric6)
#metric_total <- rbind(metric2, metric3, metric1, metric4, metric5)
table(metric_total$type); table(metric_total$Group);table(metric_total$Type); 
level_order_g <- c('Filter Number', 'Filter Length', 'Pooling Layer','Pooling Size',
                   'Activation Function','Batch Normalization')
metric_total$Group <- factor(metric_total$Group, levels = level_order_g)
metric_total <- metric_total[order(metric_total$Group),]
head(metric_total)

# 按Group分组后，根据auroc中位数对Type重新排序
metric_total2 <- metric_total %>%
  group_by(Group) %>%
  mutate(Type = fct_reorder(Type, auroc, .fun = median, .na_rm = TRUE)) %>%
  ungroup()
head(metric_total2)


P1 <- ggplot(metric_total2, aes(x=Type, y=auroc, fill=Group)) + 
  geom_boxplot(outlier.size = 0.2, outlier.alpha = 0.8) + 
  theme_bw()+ 
  theme(panel.grid = element_blank(),      
    panel.background = element_blank(),  
    panel.border = element_rect(fill = NA, colour = "black")  
  )+
  facet_grid(cols  = vars(Group), scales = 'free_x', space = 'free_x') + #分panel画
  theme(axis.text.x = element_text(angle = 90, vjust = .5, size=10,hjust = 1,colour = "black"),
        axis.text.y = element_text(size=9,colour = "black"), 
        axis.title.y = element_text(size=10),
        strip.text = element_text(size = 1,colour = "white"),
        legend.position="none") +
  ylab("AUROC") + labs(x = NULL) +
  scale_fill_brewer(palette = 'Set3') +
  stat_compare_means(aes(group = Group), method="wilcox.test",label = "p.signif", label.y = 0.7) + theme(
  strip.background = element_rect(fill = "white", color = "white"))  # 背景白色，边框黑色

P1

ggsave("./Figure3_atac_new.png", plot = P1, 
       width = 10.5, height = 8, units = "cm", dpi = 300,
       device = "png", bg = "white") 



###########################################
################# IC1
setwd("./atac_from_fjl/scATAC_cnn_parameters_100cells")

if(T){
  # IC1 Pooling layer
  level_order <- c('scATAC_N128L15PaP15ReLU','scATAC_N128L15PmP15ReLU')
  
  IC1.fpaths <- paste0(level_order, "/Motif/meme_IC.csv")
  IC1.fpaths <- IC1.fpaths[file.exists(IC1.fpaths)]
  
  IC1.list <- lapply(IC1.fpaths, function(IC1.fname){
    print(IC1.fname)
    IC1 <- read.csv(IC1.fname)
    if (dim(IC1)[-1] == 4){
      IC1 <- IC1[,1:3]
    }
    IC1$type <- strsplit(IC1.fname, '/')[[1]][1]
    return(IC1)
  })
  IC1 <- do.call(rbind, IC1.list)
  
  # sort factor
  IC1$type <- factor(IC1$type, levels = level_order)
  IC1 <- IC1[order(IC1$type),]
  IC1$Group <- 'Pooling Layer'
  # IC$IC <- ifelse(IC$IC > 8, 8, IC$IC)
  # IC$IC <- ifelse(IC$IC < 6, 6, IC$IC)
  tmp <- gsub('scATAC_N128L15','',IC1$type)
  IC1$Type <- gsub('P15ReLU','',tmp)
  
  dim(IC1); head(IC1)
  table(IC1$Type)
  ################# IC2
  # Filter Number (N)
  level_order <- c('scATAC_N32L15PaP15ReLU','scATAC_N128L15PaP15ReLU', 'scATAC_N512L15PaP15ReLU','scATAC_N8L15PaP15ReLU')
  
  IC2.fpaths <- paste0(level_order, "/Motif/meme_IC.csv")
  IC2.fpaths <- IC2.fpaths[file.exists(IC2.fpaths)]
  
  IC2.list <- lapply(IC2.fpaths, function(IC2.fname){
    print(IC2.fname)
    IC2 <- read.csv(IC2.fname)
    if (dim(IC2)[-1] == 4){
      IC2 <- IC2[,1:3]
    }
    IC2$type <- strsplit(IC2.fname, '/')[[1]][1]
    return(IC2)
  })
  IC2 <- do.call(rbind, IC2.list)
  
  # sort factor
  IC2$type <- factor(IC2$type, levels = level_order)
  IC2 <- IC2[order(IC2$type),]
  IC2$Group <- 'Filter Number'
  # IC$IC <- ifelse(IC$IC > 8, 8, IC$IC)
  # IC$IC <- ifelse(IC$IC < 6, 6, IC$IC)
  tmp <- gsub('scATAC_','',IC2$type)
  IC2$Type <- gsub('L15PaP15ReLU','',tmp)
  dim(IC2); head(IC2)
  
  ################# IC3
  # Filter Length (L)
  level_order <- c('scATAC_N128L5PaP15ReLU', 'scATAC_N128L25PaP15ReLU', 'scATAC_N128L15PaP15ReLU')
  
  IC3.fpaths <- paste0(level_order, "/Motif/meme_IC.csv")
  IC3.fpaths <- IC3.fpaths[file.exists(IC3.fpaths)]
  
  IC3.list <- lapply(IC3.fpaths, function(IC3.fname){
    print(IC3.fname)
    IC3 <- read.csv(IC3.fname)
    if (dim(IC3)[-1] == 4){
      IC3 <- IC3[,1:3]
    }
    IC3$type <- strsplit(IC3.fname, '/')[[1]][1]
    return(IC3)
  })
  IC3 <- do.call(rbind, IC3.list)
  
  # sort factor
  IC3$type <- factor(IC3$type, levels = level_order)
  IC3 <- IC3[order(IC3$type),]
  IC3$Group <- 'Filter Length'
  # IC$IC <- ifelse(IC$IC > 8, 8, IC$IC)
  # IC$IC <- ifelse(IC$IC < 6, 6, IC$IC)
  tmp <- gsub('scATAC_N128','',IC3$type)
  IC3$Type <- gsub('PaP15ReLU','',tmp)
  dim(IC3); head(IC3)
  
  
  ################# IC4
  # Pooling size (P)
  level_order <- c('scATAC_N128L15PaP5ReLU','scATAC_N128L15PaP15ReLU','scATAC_N128L15PaP25ReLU')
  
  IC4.fpaths <- paste0(level_order, "/Motif/meme_IC.csv")
  IC4.fpaths <- IC4.fpaths[file.exists(IC4.fpaths)]
  
  IC4.list <- lapply(IC4.fpaths, function(IC4.fname){
    print(IC4.fname)
    IC4 <- read.csv(IC4.fname)
    if (dim(IC4)[-1] == 4){
      IC4 <- IC4[,1:3]
    }
    IC4$type <- strsplit(IC4.fname, '/')[[1]][1]
    return(IC4)
  })
  IC4 <- do.call(rbind, IC4.list)
  
  # sort factor
  IC4$type <- factor(IC4$type, levels = level_order)
  IC4 <- IC4[order(IC4$type),]
  IC4$Group <- 'Pooling Size'
  # IC$IC <- ifelse(IC$IC > 8, 8, IC$IC)
  # IC$IC <- ifelse(IC$IC < 6, 6, IC$IC)
  tmp <- gsub('scATAC_N128L15Pa','',IC4$type)
  IC4$Type <- gsub('ReLU','',tmp)
  dim(IC4); head(IC4)
  
  ################# IC5
  # Activation Function
  level_order <- c('scATAC_N128L15PaP15ReLU', 'scATAC_N128L15PaP15Sigmoid','scATAC_N128L15PaP15Tanh','scATAC_N128L15PaP15Exp')
  
  IC5.fpaths <- paste0(level_order, "/Motif/meme_IC.csv")
  IC5.fpaths <- IC5.fpaths[file.exists(IC5.fpaths)]
  
  IC5.list <- lapply(IC5.fpaths, function(IC5.fname){
    print(IC5.fname)
    IC5 <- read.csv(IC5.fname)
    if (dim(IC5)[-1] == 4){
      IC5 <- IC5[,1:3]
    }
    IC5$type <- strsplit(IC5.fname, '/')[[1]][1]
    return(IC5)
  })
  IC5 <- do.call(rbind, IC5.list)
  
  # sort factor
  IC5$type <- factor(IC5$type, levels = level_order)
  IC5 <- IC5[order(IC5$type),]
  IC5$Group <- 'Activation Function'
  # IC$IC <- ifelse(IC$IC > 8, 8, IC$IC)
  # IC$IC <- ifelse(IC$IC < 6, 6, IC$IC)
  IC5$Group <- 'Activation Function'
  IC5$Type <- gsub('scATAC_N128L15PaP15','',IC5$type)
  dim(IC5); head(IC5)
  
  ################# IC6
  # Batch Normalization
  level_order <- c('scATAC_N128L15PaP15ReLUBN',
                   'scATAC_N128L15PaP15SigmoidBN',
                   'scATAC_N128L15PaP15TanhBN',
                   'scATAC_N128L15PaP15ExpBN')
  IC6.fpaths <- paste0(level_order, "/Motif/meme_IC.csv")
  IC6.fpaths <- IC6.fpaths[file.exists(IC6.fpaths)]
  
  IC6.list <- lapply(IC6.fpaths, function(IC6.fname){
    print(IC6.fname)
    IC6 <- read.csv(IC6.fname)
    if (dim(IC6)[-1] == 4){
      IC6 <- IC6[,1:3]
    }
    IC6$type <- strsplit(IC6.fname, '/')[[1]][1]
    return(IC6)
  })
  IC6 <- do.call(rbind, IC6.list)
  
  # sort factor
  IC6$type <- factor(IC6$type, levels = level_order)
  IC6 <- IC6[order(IC6$type),]
  IC6$Group <- 'Batch Normalization'
  # IC$IC <- ifelse(IC$IC > 8, 8, IC$IC)
  # IC$IC <- ifelse(IC$IC < 6, 6, IC$IC)
  IC6$Type <- gsub('scATAC_N128L15PaP15','',IC6$type)
  dim(IC6); head(IC6)
}

################# merge ############################
IC_total <- rbind(IC1, IC2, IC3, IC4, IC5, IC6)
table(IC_total$type);table(IC_total$Group)
level_order_g <- c('Filter Number','Filter Length','Pooling Layer', 'Pooling Size',
                   'Activation Function','Batch Normalization')
IC_total$Group <- factor(IC_total$Group, levels = level_order_g)
IC_total <- IC_total[order(IC_total$Group),]

head(IC_total,3)

# 获取metric_total2中Type列的唯一值及其顺序
type_levels <- levels(factor(metric_total2$Type))
# 将IC_total的Type列转换为因子，并指定相同的水平顺序
IC_total$Type <- factor(IC_total$Type, levels = type_levels)
# 按Type列排序（可选，若需重新排序数据框）
IC_total2 <- IC_total[order(IC_total$Type), ]





ggplot(IC_total2, aes(x=Type, y=IC, fill=Group)) + 
  geom_boxplot(varwidth = FALSE,outlier.size = 0.2, outlier.alpha = 0.8) + theme_base() + 
  facet_grid(cols  = vars(Group), scales = 'free_x', space = 'free_x') +
  theme(axis.text.x = element_text(angle = 45, vjust = .6, size=12),
        axis.text.y = element_text(hjust = -0.6,size=12), 
        legend.position="none")  + ylim(c(5,16)) + 
  #scale_y_continuous(limits = c(8,16)) +
  ylab("Information Content") +labs(x = NULL)+
  #scale_fill_viridis(discrete = TRUE, alpha = 0.8)
  scale_fill_brewer(palette = 'Set3') 
## stat_compare_means(aes(group = type), method="wilcox.test", label = "p.format", label.y = 1) 
#dev.off()


P2 <- ggplot(IC_total2, aes(x=Type, y=IC, fill=Group)) + 
  geom_boxplot(varwidth = FALSE,outlier.size = 0.2, outlier.alpha = 0.8) + 
  theme_bw()+ 
  theme(panel.grid = element_blank(),      
        panel.background = element_blank(),  
        panel.border = element_rect(fill = NA, colour = "black")  
  ) + ylim(c(5,16)) +
  facet_grid(cols  = vars(Group), scales = 'free_x', space = 'free_x') + #分panel画
  theme(axis.text.x = element_text(angle = 90, vjust = .5, size=10,hjust = 1,colour = "black"),
        axis.text.y = element_text(size=9,colour = "black"), 
        axis.title.y = element_text(size=10),
        strip.text = element_text(size = 1,colour = "white"),
        legend.position="none") +
  ylab("Information Content") + labs(x = NULL) +
  scale_fill_brewer(palette = 'Set3') + theme(
    strip.background = element_rect(fill = "white", color = "white"))  
#stat_compare_means(aes(group = Group), method="wilcox.test",label = "p.signif", label.y = 0.7) 

P2

ggsave("./Figure3_atac_IC_new.png", plot = P2, 
       width = 10.5, height = 8, units = "cm", dpi = 300,
       device = "png", bg = "white") 
