library(compareGroups)
library(table1)
library(readxl)
library(dplyr)
library(openxlsx)
library(foreign)
library(haven)
library(tidyverse)
library(ggseg)
library(ggplot2)
library(ppcor)
library(mice)

# 清空环境
rm(list = ls())

# 设置路径
setwd("..")

# load set
# 数据预处理与变量构建
alps_clinic_age <- read_xlsx("alps_clinic_age.xlsx")
str(alps_clinic_age)
colnames(alps_clinic_age)
#alps_clinic_age$Memory<-alps_clinic_age$AVLT_N4+alps_clinic_age$AVLT_N5
#alps_clinic_age$Language<-(alps_clinic_age$BNT+alps_clinic_age$VFT)/2
#alps_clinic_age$Executive<-(alps_clinic_age$TMTB+alps_clinic_age$STROOP)/2
#alps_clinic_age$Attention<-(alps_clinic_age$SDMT+alps_clinic_age$DST)/2
#alps_clinic_age$Visuospatial<-(alps_clinic_age$CDT+alps_clinic_age$ROCF)/2

alps_clinic_age$MMSE <- alps_clinic_age$MMSE
alps_clinic_age$Memory <- alps_clinic_age$AVLT_N13 + alps_clinic_age$AVLT_N4 + alps_clinic_age$AVLT_N5 + alps_clinic_age$AVLT_N6
alps_clinic_age$Executive <- 1/alps_clinic_age$TMTB + alps_clinic_age$STROOP
alps_clinic_age$Attention <- alps_clinic_age$SDMT + alps_clinic_age$DST
alps_clinic_age$Language <- alps_clinic_age$BNT + alps_clinic_age$VFT
alps_clinic_age$Visuospatial <- alps_clinic_age$CDT + alps_clinic_age$ROCF


#####---------------------------------离群值处理————————————————————————————###############
# 2. 使用 Tukey 方法检测离群值
# 定义函数来识别和处理离群值
handle_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 3 * IQR
  upper_bound <- Q3 + 3 * IQR
  
  # 替换超出1.5倍IQR的值为Q1或Q3
  x[x < lower_bound] <- Q1
  x[x > upper_bound] <- Q3
  return(x)
}

# 需要处理离群值的特定变量列表
outlier_vars <- c("Executive","Visuospatial")

# 提取ptau217_group == Positive 的数据集
HCYdata_positive_cleaned <- alps_clinic_age %>% filter(Status == "prodromal AD")

# 对HCYdatan_egative_im进行离群值检查和替换
HCYdata_positive_cleaned[outlier_vars] <- lapply(HCYdata_positive_cleaned[outlier_vars], handle_outliers)

# 提取ptau217_group == Negative 的数据集
HCYdata_negative_cleaned <- alps_clinic_age %>% filter(Status == "NC")

# 对HCYdatan_egative_im进行离群值检查和替换
HCYdata_negative_cleaned[outlier_vars] <- lapply(HCYdata_negative_cleaned[outlier_vars], handle_outliers)

# 合并正负数据集
alps_clinic_age <- rbind(HCYdata_positive_cleaned, HCYdata_negative_cleaned)

# 查看数据的维度和列名
dim(alps_clinic_age)
colnames(alps_clinic_age)

# 检查合并后的数据是否存在缺失值
colSums(is.na(alps_clinic_age))
# 加载必要的包
library(Hmisc)

####-------------------------heatmap all participants---------------------------
#选择要绘制热图的变量
alps_variables <- c( "alps_L", "alps_R", "cBAG")
cog_variables <-c("MMSE","Memory","Language","Executive","Attention","Visuospatial")

# 选择特定变量
alps_data <- alps_clinic_age[, alps_variables]
cog_data<-alps_clinic_age[,cog_variables]

# 创建新的数据框
alps_data <- as.data.frame(alps_data)
cog_data<-as.data.frame(cog_data)

# 将有序分类变量转为数值型(因为偏相关分析，协变量无法识别分类变量)，
# 理由：因子变量有明确的逻辑顺序或等级关系，那么将其转化为数值型变量是合理的。
# 这样的转换不仅可以保留因子变量的排序关系，还能使其适用于偏相关分析或其他数值型的统计分析。

table(alps_clinic_age$Sex)
alps_clinic_age$Sex <- factor(alps_clinic_age$Sex, levels = c("Male", "Female"))
alps_clinic_age$Sex <- as.numeric(alps_clinic_age$Sex)


table(alps_clinic_age$APOEe4)
alps_clinic_age$APOEe4 <- factor(alps_clinic_age$APOEe4, levels = c("Carrier", "non-Carrier"))
alps_clinic_age$APOEe4 <- as.numeric(alps_clinic_age$APOEe4)

#table(alps_clinic_age$CognitiveStatus)
#alps_clinic_age$CognitiveStatus <- factor(HCYdata_imtu$CognitiveStatus, levels = c("CognitivelyUnimpaired", "MCI", "Dementia"))
#alps_clinic_age$CognitiveStatus <- as.numeric(HCYdata_imtu$CognitiveStatus)

# 定义协变量
control_variables <- c(
  "Sex",
  "Age",
  "Education",
  "APOEe4",
  "PSMD"
  #,"PSQI"
  #"CognitiveStatus"
)

colSums(is.na(alps_clinic_age))


# 构建相关关系矩阵
library(psych)
data.corr <- corr.test(alps_data, cog_data, method = "pearson", adjust="fdr")
data.r <- data.corr$r  # 相关系数
data.p <- data.corr$p  # p值


library(ggplot2)
library(reshape2)

# 显著性标记函数
getSig <- function(p_value) {
  if (p_value < 0.001) return("***")
  else if (p_value < 0.01) return("**")
  else if (p_value < 0.05) return("*")
  else return("")
}

# ================= 热图 A: All participants =================
data.corr <- corr.test(alps_data, cog_data, method = "pearson", adjust="fdr")
data.r <- data.corr$r
data.p <- data.corr$p

data.r.melt <- melt(data.r)
data.p.melt <- melt(data.p)
data.r.melt$Sig <- mapply(getSig, data.p.melt$value)
colnames(data.r.melt) <- c("Plasma", "CSF", "Correlation", "Significance")

p1 <- ggplot(data.r.melt, aes(x = Plasma, y = CSF, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#0000FF", mid = "#FFFFFF", high = "#FF0000", midpoint = 0, limits = c(-1, 1)) +
  geom_text(aes(label = Significance), color = "black", size = 2.5, fontface = "bold", family = "Arial") +
  theme_minimal(base_size = 8, base_family = "Arial") +
  theme(
    axis.title = element_blank(),       
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8, face = "bold"),  
    axis.text.y = element_text(size = 8, face = "bold"),
    legend.position = "none",  # 去掉图例，后续在AI里加共享colorbar
    plot.margin = margin(2, 2, 2, 2)
  ) +
  scale_x_discrete(labels = c("Left ALPS", "Right ALPS", "cBAG")) +
  scale_y_discrete(labels = c("Global Cognition","Memory","Language","Executive","Attention","Visuospatial"))

print(p1)
ggsave("figure3_A_heatmap_all.pdf", p1, width = 2.4, height = 2.4, device = cairo_pdf)

# ================= 热图 B: pAD participants =================
positive_plasma <- subset(alps_clinic_age, Status == "prodromal AD")
alps_data_p <- as.data.frame(positive_plasma[, alps_variables])
cog_data_p <- as.data.frame(positive_plasma[, cog_variables])

data.corr <- corr.test(alps_data_p, cog_data_p, method = "pearson", adjust="fdr")
data.r <- data.corr$r; data.p <- data.corr$p
data.r.melt <- melt(data.r); data.p.melt <- melt(data.p)
data.r.melt$Sig <- mapply(getSig, data.p.melt$value)
colnames(data.r.melt) <- c("Plasma", "CSF", "Correlation", "Significance")

p2 <- ggplot(data.r.melt, aes(x = Plasma, y = CSF, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#0000FF", mid = "#FFFFFF", high = "#FF0000", midpoint = 0, limits = c(-1, 1)) +
  geom_text(aes(label = Significance), color = "black", size = 2.5, fontface = "bold", family = "Arial") +
  theme_minimal(base_size = 8, base_family = "Arial") +
  theme(
    axis.title = element_blank(),       
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8, face = "bold"),  
    axis.text.y = element_text(size = 8, face = "bold"),
    legend.position = "none", 
    plot.margin = margin(2, 2, 2, 2)
  ) +
  scale_x_discrete(labels = c("Left ALPS", "Right ALPS", "cBAG")) +
  scale_y_discrete(labels = c("Global Cognition","Memory","Language","Executive","Attention","Visuospatial"))
print(p2)
ggsave("figure3_B_heatmap_pAD.pdf", p2, width = 2.4, height = 2.4, device = cairo_pdf)

# ================= 热图 C: NC participants =================
Negative_plasma <- subset(alps_clinic_age, Status == "NC")
alps_data_n <- as.data.frame(Negative_plasma[, alps_variables])
cog_data_n <- as.data.frame(Negative_plasma[, cog_variables])

data.corr <- corr.test(alps_data_n, cog_data_n, method = "pearson", adjust="fdr")
data.r <- data.corr$r; data.p <- data.corr$p
data.r.melt <- melt(data.r); data.p.melt <- melt(data.p)
data.r.melt$Sig <- mapply(getSig, data.p.melt$value)
colnames(data.r.melt) <- c("Plasma", "CSF", "Correlation", "Significance")

p3 <- ggplot(data.r.melt, aes(x = Plasma, y = CSF, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#0000FF", mid = "#FFFFFF", high = "#FF0000", midpoint = 0, limits = c(-1, 1)) +
  geom_text(aes(label = Significance), color = "black", size = 2.5, fontface = "bold", family = "Arial") +
  theme_minimal(base_size = 8, base_family = "Arial") +
  theme(
    axis.title = element_blank(),       
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8, face = "bold"),  
    axis.text.y = element_text(size = 8, face = "bold"),
    legend.position = "none", 
    plot.margin = margin(2, 2, 2, 2)
  ) +
  scale_x_discrete(labels = c("Left ALPS", "Right ALPS", "cBAG")) +
  scale_y_discrete(labels = c("Global Cognition","Memory","Language","Executive","Attention","Visuospatial"))
print(p3)
ggsave("figure3_C_heatmap_NC.pdf", p3, width = 2.4, height = 2.4, device = cairo_pdf)


# ============================================================
# Figure 3: 认知域与ALPS/cBAG的偏相关散点图（标注 r 和 q）
# ============================================================

library(dplyr)
library(ggplot2)

# 控制变量
control_variables <- c("Sex", "Age", "Education", "APOEe4")

# ---------- 辅助函数：从 lm 模型提取偏相关 r ----------
get_partial_r <- function(model, var_name) {
  coef_tab <- summary(model)$coefficients
  if (!var_name %in% rownames(coef_tab)) return(NA)
  t_val <- coef_tab[var_name, "t value"]
  df <- model$df.residual
  if (is.na(t_val) || df <= 0) return(NA)
  sign(t_val) * sqrt(t_val^2 / (t_val^2 + df))
}

# ---------- 辅助函数：格式化 q 值 ----------
format_p <- function(p_val) {
  if (is.na(p_val)) return("NA")
  if (p_val < 0.001) return("< 0.001")
  return(sprintf("%.3f", p_val))
}

# ============================================================
# 图D: Left ALPS ~ Memory
# ============================================================
lm_formula <- as.formula(paste("Memory ~ alps_L +", paste(control_variables, collapse = " + ")))
lm_all <- lm(lm_formula, data = alps_clinic_age)
lm_nc  <- lm(lm_formula, data = subset(alps_clinic_age, Status == "NC"))
lm_pad <- lm(lm_formula, data = subset(alps_clinic_age, Status == "prodromal AD"))

r_all <- get_partial_r(lm_all, "alps_L")
r_nc  <- get_partial_r(lm_nc,  "alps_L")
r_pad <- get_partial_r(lm_pad, "alps_L")

p_all <- summary(lm_all)$coefficients["alps_L", "Pr(>|t|)"]
p_nc  <- summary(lm_nc)$coefficients["alps_L", "Pr(>|t|)"]
p_pad <- summary(lm_pad)$coefficients["alps_L", "Pr(>|t|)"]
q_values <- p.adjust(c(p_all, p_nc, p_pad), method = "fdr")
q_all <- q_values[1]; q_nc <- q_values[2]; q_pad <- q_values[3]

x_max <- max(alps_clinic_age$alps_L, na.rm = TRUE)
y_max <- max(alps_clinic_age$Memory, na.rm = TRUE)

gg_D <- ggplot(alps_clinic_age, aes(x = alps_L, y = Memory, color = factor(Status), shape = factor(Status))) +
  geom_point(na.rm = TRUE, size = 1.2, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, aes(group = 1), linetype = "dashed", color = "#305292", linewidth = 0.5) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.5) +
  labs(x = "Left ALPS", y = "Memory") +
  theme_minimal(base_size = 8, base_family = "Arial") +
  theme(
    panel.background = element_rect(fill = "white"),
    axis.text.x  = element_text(size = 8, face = "bold"),
    axis.text.y  = element_text(size = 8, face = "bold"),
    axis.title   = element_text(size = 9, face = "bold"),
    legend.position = "none",
    plot.margin = margin(2, 2, 2, 2)
  ) +
  scale_shape_manual(values = c(16, 17), guide = "none") +
  geom_text(x = x_max, y = y_max * 0.98, hjust = 1, vjust = 1,
            label = paste0("All: r = ", round(r_all, 3), ", q ", format_p(q_all)),
            color = "#305292", size = 2.8, family = "Arial") +
  geom_text(x = x_max, y = y_max * 0.92, hjust = 1, vjust = 1,
            label = paste0("pAD: r = ", round(r_pad, 3), ", q = ", format_p(q_pad)),
            color = "#F36688", size = 2.8, family = "Arial") +
  geom_text(x = x_max, y = y_max * 0.86, hjust = 1, vjust = 1,
            label = paste0("NC: r = ", round(r_nc, 3), ", q = ", format_p(q_nc)),
            color = "#AAE3C4", size = 2.8, family = "Arial") +
  scale_color_manual(values = c("#AAE3C4", "#F36688"))

print(gg_D)
ggsave("figure3_D_alps_L_memory_FDR.pdf", gg_D, width = 2.4, height = 2.4, device = cairo_pdf)

# ============================================================
# 图E: Right ALPS ~ Memory
# ============================================================
lm_formula <- as.formula(paste("Memory ~ alps_R +", paste(control_variables, collapse = " + ")))
lm_all <- lm(lm_formula, data = alps_clinic_age)
lm_nc  <- lm(lm_formula, data = subset(alps_clinic_age, Status == "NC"))
lm_pad <- lm(lm_formula, data = subset(alps_clinic_age, Status == "prodromal AD"))

r_all <- get_partial_r(lm_all, "alps_R")
r_nc  <- get_partial_r(lm_nc,  "alps_R")
r_pad <- get_partial_r(lm_pad, "alps_R")

p_all <- summary(lm_all)$coefficients["alps_R", "Pr(>|t|)"]
p_nc  <- summary(lm_nc)$coefficients["alps_R", "Pr(>|t|)"]
p_pad <- summary(lm_pad)$coefficients["alps_R", "Pr(>|t|)"]
q_values <- p.adjust(c(p_all, p_nc, p_pad), method = "fdr")
q_all <- q_values[1]; q_nc <- q_values[2]; q_pad <- q_values[3]

x_max <- max(alps_clinic_age$alps_R, na.rm = TRUE)
y_max <- max(alps_clinic_age$Memory, na.rm = TRUE)

gg_E <- ggplot(alps_clinic_age, aes(x = alps_R, y = Memory, color = factor(Status), shape = factor(Status))) +
  geom_point(na.rm = TRUE, size = 1.2) +
  geom_smooth(method = "lm", se = TRUE, aes(group = 1), linetype = "dashed", color = "#305292", linewidth = 0.5) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.5) +
  labs(x = "Right ALPS", y = "Memory") +
  theme_minimal(base_size = 8, base_family = "Arial") +
  theme(
    panel.background = element_rect(fill = "white"),
    axis.text.x  = element_text(size = 8, face = "bold"),
    axis.text.y  = element_text(size = 8, face = "bold"),
    axis.title   = element_text(size = 9, face = "bold"),
    legend.position = "none",
    plot.margin = margin(2, 2, 2, 2)
  ) +
  scale_shape_manual(values = c(16, 17), guide = "none") +
  geom_text(x = x_max, y = y_max * 0.98, hjust = 1, vjust = 1,
            label = paste0("All: r = ", round(r_all, 3), ", q ", format_p(q_all)),
            color = "#305292", size = 2.8, family = "Arial") +
  geom_text(x = x_max, y = y_max * 0.92, hjust = 1, vjust = 1,
            label = paste0("pAD: r = ", round(r_pad, 3), ", q = ", format_p(q_pad)),
            color = "#F36688", size = 2.8, family = "Arial") +
  geom_text(x = x_max, y = y_max * 0.86, hjust = 1, vjust = 1,
            label = paste0("NC: r = ", round(r_nc, 3), ", q = ", format_p(q_nc)),
            color = "#AAE3C4", size = 2.8, family = "Arial") +
  scale_color_manual(values = c("#AAE3C4", "#F36688"))

print(gg_E)
ggsave("figure3_E_alps_R_memory_FDR.pdf", gg_E, width = 2.4, height = 2.4, device = cairo_pdf)

# ============================================================
# 图F: cBAG ~ Memory
# ============================================================
lm_formula <- as.formula(paste("Memory ~ cBAG +", paste(control_variables, collapse = " + ")))
lm_all <- lm(lm_formula, data = alps_clinic_age)
lm_nc  <- lm(lm_formula, data = subset(alps_clinic_age, Status == "NC"))
lm_pad <- lm(lm_formula, data = subset(alps_clinic_age, Status == "prodromal AD"))

r_all <- get_partial_r(lm_all, "cBAG")
r_nc  <- get_partial_r(lm_nc,  "cBAG")
r_pad <- get_partial_r(lm_pad, "cBAG")

p_all <- summary(lm_all)$coefficients["cBAG", "Pr(>|t|)"]
p_nc  <- summary(lm_nc)$coefficients["cBAG", "Pr(>|t|)"]
p_pad <- summary(lm_pad)$coefficients["cBAG", "Pr(>|t|)"]
q_values <- p.adjust(c(p_all, p_nc, p_pad), method = "fdr")
q_all <- q_values[1]; q_nc <- q_values[2]; q_pad <- q_values[3]

x_max <- max(alps_clinic_age$cBAG, na.rm = TRUE)
y_max <- max(alps_clinic_age$Memory, na.rm = TRUE)

gg_F <- ggplot(alps_clinic_age, aes(x = cBAG, y = Memory, color = factor(Status), shape = factor(Status))) +
  geom_point(na.rm = TRUE, size = 1.2, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, aes(group = 1), linetype = "dashed", color = "#305292", linewidth = 0.5) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.5) +
  labs(x = "cBAG", y = "Memory") +
  theme_minimal(base_size = 8, base_family = "Arial") +
  theme(
    panel.background = element_rect(fill = "white"),
    axis.text.x  = element_text(size = 8, face = "bold"),
    axis.text.y  = element_text(size = 8, face = "bold"),
    axis.title   = element_text(size = 9, face = "bold"),
    legend.position = "none",
    plot.margin = margin(2, 2, 2, 2)
  ) +
  scale_shape_manual(values = c(16, 17), guide = "none") +
  geom_text(x = x_max, y = y_max * 0.98, hjust = 1, vjust = 1,
            label = paste0("All: r = ", round(r_all, 3), ", q ", format_p(q_all)),
            color = "#305292", size = 2.8, family = "Arial") +
  geom_text(x = x_max, y = y_max * 0.92, hjust = 1, vjust = 1,
            label = paste0("pAD: r = ", round(r_pad, 3), ", q = ", format_p(q_pad)),
            color = "#F36688", size = 2.8, family = "Arial") +
  geom_text(x = x_max, y = y_max * 0.86, hjust = 1, vjust = 1,
            label = paste0("NC: r = ", round(r_nc, 3), ", q = ", format_p(q_nc)),
            color = "#AAE3C4", size = 2.8, family = "Arial") +
  scale_color_manual(values = c("#AAE3C4", "#F36688"))

print(gg_F)
ggsave("figure3_F_cBAG_memory_FDR.pdf", gg_F, width = 2.4, height = 2.4, device = cairo_pdf)

# ============================================================
# 图G: Left ALPS ~ Executive
# ============================================================
lm_formula <- as.formula(paste("Executive ~ alps_L +", paste(control_variables, collapse = " + ")))
lm_all <- lm(lm_formula, data = alps_clinic_age)
lm_nc  <- lm(lm_formula, data = subset(alps_clinic_age, Status == "NC"))
lm_pad <- lm(lm_formula, data = subset(alps_clinic_age, Status == "prodromal AD"))

r_all <- get_partial_r(lm_all, "alps_L")
r_nc  <- get_partial_r(lm_nc,  "alps_L")
r_pad <- get_partial_r(lm_pad, "alps_L")

p_all <- summary(lm_all)$coefficients["alps_L", "Pr(>|t|)"]
p_nc  <- summary(lm_nc)$coefficients["alps_L", "Pr(>|t|)"]
p_pad <- summary(lm_pad)$coefficients["alps_L", "Pr(>|t|)"]
q_values <- p.adjust(c(p_all, p_nc, p_pad), method = "fdr")
q_all <- q_values[1]; q_nc <- q_values[2]; q_pad <- q_values[3]

x_max <- max(alps_clinic_age$alps_L, na.rm = TRUE)
y_max <- max(alps_clinic_age$Executive, na.rm = TRUE)

gg_G <- ggplot(alps_clinic_age, aes(x = alps_L, y = Executive, color = factor(Status), shape = factor(Status))) +
  geom_point(na.rm = TRUE, size = 1.2, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, aes(group = 1), linetype = "dashed", color = "#305292", linewidth = 0.5) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.5) +
  labs(x = "Left ALPS", y = "Executive") +
  theme_minimal(base_size = 8, base_family = "Arial") +
  theme(
    panel.background = element_rect(fill = "white"),
    axis.text.x  = element_text(size = 8, face = "bold"),
    axis.text.y  = element_text(size = 8, face = "bold"),
    axis.title   = element_text(size = 9, face = "bold"),
    legend.position = "none",
    plot.margin = margin(2, 2, 2, 2)
  ) +
  scale_shape_manual(values = c(16, 17), guide = "none") +
  geom_text(x = x_max, y = y_max * 0.98 - 30, hjust = 1, vjust = 1,
            label = paste0("All: r = ", round(r_all, 3), ", q ", format_p(q_all)),
            color = "#305292", size = 2.8, family = "Arial") +
  geom_text(x = x_max, y = y_max * 0.92 - 30, hjust = 1, vjust = 1,
            label = paste0("pAD: r = ", round(r_pad, 3), ", q = ", format_p(q_pad)),
            color = "#F36688", size = 2.8, family = "Arial") +
  geom_text(x = x_max, y = y_max * 0.86 - 30, hjust = 1, vjust = 1,
            label = paste0("NC: r = ", round(r_nc, 3), ", q = ", format_p(q_nc)),
            color = "#AAE3C4", size = 2.8, family = "Arial") +
  scale_color_manual(values = c("#AAE3C4", "#F36688"))

print(gg_G)
ggsave("figure3_G_alps_L_executive_FDR.pdf", gg_G, width = 2.4, height = 2.4, device = cairo_pdf)

# ============================================================
# 图H: Right ALPS ~ Executive
# ============================================================
lm_formula <- as.formula(paste("Executive ~ alps_R +", paste(control_variables, collapse = " + ")))
lm_all <- lm(lm_formula, data = alps_clinic_age)
lm_nc  <- lm(lm_formula, data = subset(alps_clinic_age, Status == "NC"))
lm_pad <- lm(lm_formula, data = subset(alps_clinic_age, Status == "prodromal AD"))

r_all <- get_partial_r(lm_all, "alps_R")
r_nc  <- get_partial_r(lm_nc,  "alps_R")
r_pad <- get_partial_r(lm_pad, "alps_R")

p_all <- summary(lm_all)$coefficients["alps_R", "Pr(>|t|)"]
p_nc  <- summary(lm_nc)$coefficients["alps_R", "Pr(>|t|)"]
p_pad <- summary(lm_pad)$coefficients["alps_R", "Pr(>|t|)"]
q_values <- p.adjust(c(p_all, p_nc, p_pad), method = "fdr")
q_all <- q_values[1]; q_nc <- q_values[2]; q_pad <- q_values[3]

x_max <- max(alps_clinic_age$alps_R, na.rm = TRUE)
y_max <- max(alps_clinic_age$Executive, na.rm = TRUE)

gg_H <- ggplot(alps_clinic_age, aes(x = alps_R, y = Executive, color = factor(Status), shape = factor(Status))) +
  geom_point(na.rm = TRUE, size = 1.2, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, aes(group = 1), linetype = "dashed", color = "#305292", linewidth = 0.5) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.5) +
  labs(x = "Right ALPS", y = "Executive") +
  theme_minimal(base_size = 8, base_family = "Arial") +
  theme(
    panel.background = element_rect(fill = "white"),
    axis.text.x  = element_text(size = 8, face = "bold"),
    axis.text.y  = element_text(size = 8, face = "bold"),
    axis.title   = element_text(size = 9, face = "bold"),
    legend.position = "none",
    plot.margin = margin(2, 2, 2, 2)
  ) +
  scale_shape_manual(values = c(16, 17), guide = "none") +
  geom_text(x = x_max, y = y_max * 0.98 - 30, hjust = 1, vjust = 1,
            label = paste0("All: r = ", round(r_all, 3), ", q = ", format_p(q_all)),
            color = "#305292", size = 2.8, family = "Arial") +
  geom_text(x = x_max, y = y_max * 0.92 - 30, hjust = 1, vjust = 1,
            label = paste0("pAD: r = ", round(r_pad, 3), ", q = ", format_p(q_pad)),
            color = "#F36688", size = 2.8, family = "Arial") +
  geom_text(x = x_max, y = y_max * 0.86 - 30, hjust = 1, vjust = 1,
            label = paste0("NC: r = ", round(r_nc, 3), ", q = ", format_p(q_nc)),
            color = "#AAE3C4", size = 2.8, family = "Arial") +
  scale_color_manual(values = c("#AAE3C4", "#F36688"))

print(gg_H)
ggsave("figure3_H_alps_R_executive_FDR.pdf", gg_H, width = 2.4, height = 2.4, device = cairo_pdf)

# ============================================================
# 图I: cBAG ~ Executive
# ============================================================
lm_formula <- as.formula(paste("Executive ~ cBAG +", paste(control_variables, collapse = " + ")))
lm_all <- lm(lm_formula, data = alps_clinic_age)
lm_nc  <- lm(lm_formula, data = subset(alps_clinic_age, Status == "NC"))
lm_pad <- lm(lm_formula, data = subset(alps_clinic_age, Status == "prodromal AD"))

r_all <- get_partial_r(lm_all, "cBAG")
r_nc  <- get_partial_r(lm_nc,  "cBAG")
r_pad <- get_partial_r(lm_pad, "cBAG")

p_all <- summary(lm_all)$coefficients["cBAG", "Pr(>|t|)"]
p_nc  <- summary(lm_nc)$coefficients["cBAG", "Pr(>|t|)"]
p_pad <- summary(lm_pad)$coefficients["cBAG", "Pr(>|t|)"]
q_values <- p.adjust(c(p_all, p_nc, p_pad), method = "fdr")
q_all <- q_values[1]; q_nc <- q_values[2]; q_pad <- q_values[3]

x_max <- max(alps_clinic_age$cBAG, na.rm = TRUE)
y_max <- max(alps_clinic_age$Executive, na.rm = TRUE)

gg_I <- ggplot(alps_clinic_age, aes(x = cBAG, y = Executive, color = factor(Status), shape = factor(Status))) +
  geom_point(na.rm = TRUE, size = 1.2, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, aes(group = 1), linetype = "dashed", color = "#305292", linewidth = 0.5) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.5) +
  labs(x = "cBAG", y = "Executive") +
  theme_minimal(base_size = 8, base_family = "Arial") +
  theme(
    panel.background = element_rect(fill = "white"),
    axis.text.x  = element_text(size = 8, face = "bold"),
    axis.text.y  = element_text(size = 8, face = "bold"),
    axis.title   = element_text(size = 9, face = "bold"),
    legend.position = "none",
    plot.margin = margin(2, 2, 2, 2)
  ) +
  scale_shape_manual(values = c(16, 17), guide = "none") +
  geom_text(x = x_max, y = y_max * 0.98 - 30, hjust = 1, vjust = 1,
            label = paste0("All: r = ", round(r_all, 3), ", q ", format_p(q_all)),
            color = "#305292", size = 2.8, family = "Arial") +
  geom_text(x = x_max, y = y_max * 0.92 - 30, hjust = 1, vjust = 1,
            label = paste0("pAD: r = ", round(r_pad, 3), ", q = ", format_p(q_pad)),
            color = "#F36688", size = 2.8, family = "Arial") +
  geom_text(x = x_max, y = y_max * 0.86 - 30, hjust = 1, vjust = 1,
            label = paste0("NC: r = ", round(r_nc, 3), ", q = ", format_p(q_nc)),
            color = "#AAE3C4", size = 2.8, family = "Arial") +
  scale_color_manual(values = c("#AAE3C4", "#F36688"))

print(gg_I)
ggsave("figure3_I_cBAG_executive_FDR.pdf", gg_I, width = 2.4, height = 2.4, device = cairo_pdf)

# ============================================================
# 图J: Left ALPS ~ Language
# ============================================================
lm_formula <- as.formula(paste("Language ~ alps_L +", paste(control_variables, collapse = " + ")))
lm_all <- lm(lm_formula, data = alps_clinic_age)
lm_nc  <- lm(lm_formula, data = subset(alps_clinic_age, Status == "NC"))
lm_pad <- lm(lm_formula, data = subset(alps_clinic_age, Status == "prodromal AD"))

r_all <- get_partial_r(lm_all, "alps_L")
r_nc  <- get_partial_r(lm_nc,  "alps_L")
r_pad <- get_partial_r(lm_pad, "alps_L")

p_all <- summary(lm_all)$coefficients["alps_L", "Pr(>|t|)"]
p_nc  <- summary(lm_nc)$coefficients["alps_L", "Pr(>|t|)"]
p_pad <- summary(lm_pad)$coefficients["alps_L", "Pr(>|t|)"]
q_values <- p.adjust(c(p_all, p_nc, p_pad), method = "fdr")
q_all <- q_values[1]; q_nc <- q_values[2]; q_pad <- q_values[3]

x_max <- max(alps_clinic_age$alps_L, na.rm = TRUE)
y_max <- max(alps_clinic_age$Language, na.rm = TRUE)

gg_J <- ggplot(alps_clinic_age, aes(x = alps_L, y = Language, color = factor(Status), shape = factor(Status))) +
  geom_point(na.rm = TRUE, size = 1.2, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, aes(group = 1), linetype = "dashed", color = "#305292", linewidth = 0.5) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.5) +
  labs(x = "Left ALPS", y = "Language") +
  theme_minimal(base_size = 8, base_family = "Arial") +
  theme(
    panel.background = element_rect(fill = "white"),
    axis.text.x  = element_text(size = 8, face = "bold"),
    axis.text.y  = element_text(size = 8, face = "bold"),
    axis.title   = element_text(size = 9, face = "bold"),
    legend.position = "none",
    plot.margin = margin(2, 2, 2, 2)
  ) +
  scale_shape_manual(values = c(16, 17), guide = "none") +
  geom_text(x = x_max, y = y_max * 0.98 - 28, hjust = 1, vjust = 1,
            label = paste0("All: r = ", round(r_all, 3), ", q = ", format_p(q_all)),
            color = "#305292", size = 2.8, family = "Arial") +
  geom_text(x = x_max, y = y_max * 0.94 - 28, hjust = 1, vjust = 1,
            label = paste0("pAD: r = ", round(r_pad, 3), ", q = ", format_p(q_pad)),
            color = "#F36688", size = 2.8, family = "Arial") +
  geom_text(x = x_max, y = y_max * 0.90 - 28, hjust = 1, vjust = 1,
            label = paste0("NC: r = ", round(r_nc, 3), ", q = ", format_p(q_nc)),
            color = "#AAE3C4", size = 2.8, family = "Arial") +
  scale_color_manual(values = c("#AAE3C4", "#F36688"))

print(gg_J)
ggsave("figure3_J_alps_L_language_FDR.pdf", gg_J, width = 2.4, height = 2.4, device = cairo_pdf)

# ============================================================
# 图K: Right ALPS ~ Language
# ============================================================
lm_formula <- as.formula(paste("Language ~ alps_R +", paste(control_variables, collapse = " + ")))
lm_all <- lm(lm_formula, data = alps_clinic_age)
lm_nc  <- lm(lm_formula, data = subset(alps_clinic_age, Status == "NC"))
lm_pad <- lm(lm_formula, data = subset(alps_clinic_age, Status == "prodromal AD"))

r_all <- get_partial_r(lm_all, "alps_R")
r_nc  <- get_partial_r(lm_nc,  "alps_R")
r_pad <- get_partial_r(lm_pad, "alps_R")

p_all <- summary(lm_all)$coefficients["alps_R", "Pr(>|t|)"]
p_nc  <- summary(lm_nc)$coefficients["alps_R", "Pr(>|t|)"]
p_pad <- summary(lm_pad)$coefficients["alps_R", "Pr(>|t|)"]
q_values <- p.adjust(c(p_all, p_nc, p_pad), method = "fdr")
q_all <- q_values[1]; q_nc <- q_values[2]; q_pad <- q_values[3]

x_max <- max(alps_clinic_age$alps_R, na.rm = TRUE)
y_max <- max(alps_clinic_age$Language, na.rm = TRUE)

gg_K <- ggplot(alps_clinic_age, aes(x = alps_R, y = Language, color = factor(Status), shape = factor(Status))) +
  geom_point(na.rm = TRUE, size = 1.2, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, aes(group = 1), linetype = "dashed", color = "#305292", linewidth = 0.5) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.5) +
  labs(x = "Right ALPS", y = "Language") +
  theme_minimal(base_size = 8, base_family = "Arial") +
  theme(
    panel.background = element_rect(fill = "white"),
    axis.text.x  = element_text(size = 8, face = "bold"),
    axis.text.y  = element_text(size = 8, face = "bold"),
    axis.title   = element_text(size = 9, face = "bold"),
    legend.position = "none",
    plot.margin = margin(2, 2, 2, 2)
  ) +
  scale_shape_manual(values = c(16, 17), guide = "none") +
  geom_text(x = x_max, y = y_max * 0.98 - 28, hjust = 1, vjust = 1,
            label = paste0("All: r = ", round(r_all, 3), ", q ", format_p(q_all)),
            color = "#305292", size = 2.8, family = "Arial") +
  geom_text(x = x_max, y = y_max * 0.94 - 28, hjust = 1, vjust = 1,
            label = paste0("pAD: r = ", round(r_pad, 3), ", q = ", format_p(q_pad)),
            color = "#F36688", size = 2.8, family = "Arial") +
  geom_text(x = x_max, y = y_max * 0.90 - 28, hjust = 1, vjust = 1,
            label = paste0("NC: r = ", round(r_nc, 3), ", q = ", format_p(q_nc)),
            color = "#AAE3C4", size = 2.8, family = "Arial") +
  scale_color_manual(values = c("#AAE3C4", "#F36688"))

print(gg_K)
ggsave("figure3_K_alps_R_language_FDR.pdf", gg_K, width = 2.4, height = 2.4, device = cairo_pdf)

# ============================================================
# 图L: Right ALPS ~ Language
# ============================================================
lm_formula <- as.formula(paste("MMSE ~ alps_R +", paste(control_variables, collapse = " + ")))
lm_all <- lm(lm_formula, data = alps_clinic_age)
lm_nc  <- lm(lm_formula, data = subset(alps_clinic_age, Status == "NC"))
lm_pad <- lm(lm_formula, data = subset(alps_clinic_age, Status == "prodromal AD"))

r_all <- get_partial_r(lm_all, "alps_R")
r_nc  <- get_partial_r(lm_nc,  "alps_R")
r_pad <- get_partial_r(lm_pad, "alps_R")

p_all <- summary(lm_all)$coefficients["alps_R", "Pr(>|t|)"]
p_nc  <- summary(lm_nc)$coefficients["alps_R", "Pr(>|t|)"]
p_pad <- summary(lm_pad)$coefficients["alps_R", "Pr(>|t|)"]
q_values <- p.adjust(c(p_all, p_nc, p_pad), method = "fdr")
q_all <- q_values[1]; q_nc <- q_values[2]; q_pad <- q_values[3]

x_max <- max(alps_clinic_age$alps_R, na.rm = TRUE)
y_max <- max(alps_clinic_age$MMSE, na.rm = TRUE)

gg_L <- ggplot(alps_clinic_age, aes(x = alps_R, y = MMSE, color = factor(Status), shape = factor(Status))) +
  geom_point(na.rm = TRUE, size = 1.2, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, aes(group = 1), linetype = "dashed", color = "#305292", linewidth = 0.5) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.5) +
  labs(x = "Right ALPS", y = "Global Cognition") +
  theme_minimal(base_size = 8, base_family = "Arial") +
  theme(
    panel.background = element_rect(fill = "white"),
    axis.text.x  = element_text(size = 8, face = "bold"),
    axis.text.y  = element_text(size = 8, face = "bold"),
    axis.title   = element_text(size = 9, face = "bold"),
    legend.position = "none",
    plot.margin = margin(2, 2, 2, 2)
  ) +
  scale_shape_manual(values = c(16, 17), guide = "none") +
  geom_text(x = x_max, y = y_max * 0.98-14 , hjust = 1, vjust = 1,
            label = paste0("All: r = ", round(r_all, 3), ", q ", format_p(q_all)),
            color = "#305292", size = 2.8, family = "Arial") +
  geom_text(x = x_max, y = y_max * 0.94-14 , hjust = 1, vjust = 1,
            label = paste0("pAD: r = ", round(r_pad, 3), ", q = ", format_p(q_pad)),
            color = "#F36688", size = 2.8, family = "Arial") +
  geom_text(x = x_max, y = y_max * 0.90-14 , hjust = 1, vjust = 1,
            label = paste0("NC: r = ", round(r_nc, 3), ", q = ", format_p(q_nc)),
            color = "#AAE3C4", size = 2.8, family = "Arial") +
  scale_color_manual(values = c("#AAE3C4", "#F36688"))

print(gg_L)
ggsave("figure3_L_alps_R_MMSE_FDR.pdf", gg_L, width = 2.4, height = 2.4, device = cairo_pdf)


