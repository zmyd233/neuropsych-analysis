
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
alps_clinic_age <- read_xlsx("alps_clinic_age.xlsx")
str(alps_clinic_age)
colnames(alps_clinic_age)
#alps_clinic_age<-subset(alps_clinic_age,Status =="prodromal AD")
# 加载必要的包
library(Hmisc)

####-------------------------heatmap all participants---------------------------
#选择要绘制热图的变量
alps_variables <- c( "Ptau217", "Aβ42", "alps_L", "alps_R", "cBAG")


# 选择特定变量
alps_data <- alps_clinic_age[, alps_variables]


# 创建新的数据框
alps_data <- as.data.frame(alps_data)


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
  #"HIS"
  #,"eTIV"
  #,"PSQI"
  #"CognitiveStatus"
)

colSums(is.na(alps_clinic_age))


# 构建相关关系矩阵
library(psych)
data.corr <- corr.test(alps_data, alps_data, method = "pearson", adjust="fdr")
data.r <- data.corr$r  # 相关系数
data.p <- data.corr$p  # p值


library(ggplot2)
library(reshape2)

# 显著性标记函数
getSig <- function(p_value) {
  if (p_value < 0.001) {
    return("***")
  } else if (p_value < 0.01) {
    return("**")
  } else if (p_value < 0.05) {
    return("*")
  } else {
    return("")
  }
}

# 将相关系数和显著性标记矩阵转换为数据框格式
data.r.melt <- melt(data.r)  # 相关系数矩阵展开为长格式
data.p.melt <- melt(data.p)  # p 值矩阵展开为长格式

# 添加显著性标记列
data.r.melt$Sig <- mapply(getSig, data.p.melt$value)

# 设置变量名称
colnames(data.r.melt) <- c("Plasma", "CSF", "Correlation", "Significance")

# 绘制热图
p1 <- ggplot(data.r.melt, aes(x = Plasma, y = CSF, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#0000FF", mid = "#FFFFFF", high = "#FF0000", 
                       midpoint = 0, limits = c(-1, 1), name = "R") +
  geom_text(aes(label = Significance), color = "black", size = 2.5, family = "Arial") +
  theme_minimal(base_size = 8, base_family = "Arial") + 
  theme(
    axis.title = element_blank(),       
    # 把45度改成90度，避免标签打架，字号可以略微提升到8
    axis.text.x = element_text( angle = 45, hjust = 1, size = 8, face = "bold"),
    axis.text.y = element_text(size = 8, face = "bold"),
    legend.position = "none",  # ★★★ 修复：必须明确设为 "none"
    plot.margin = margin(2, 2, 2, 2) # 减少一点边缘留白
  ) +
  # ★★★ 建议：使用简写标签，给格子内部留出更多空间
  scale_x_discrete(labels = c("p-tau217", "Aβ42", "Left ALPS", "Right ALPS", "cBAG")) +
  scale_y_discrete(labels = c("p-tau217", "Aβ42", "Left ALPS", "Right ALPS", "cBAG"))

# 打印图像
print(p1)

# 保存图形（修改为 .pdf 后缀）
ggsave("figure2_heatmap_all.pdf", p1, width = 2.4, height = 2.4, device = cairo_pdf)


#######----------------------heatmap of  Positive--------------------------------
colnames(alps_clinic_age)

table(alps_clinic_age$Status)
# 筛选 Positive 数据

positive_plasma <- subset(alps_clinic_age, Status == "prodromal AD")

# 选择特定变量
plasma_data <- positive_plasma[, alps_variables]

# 创建新的数据框
plasma_data <- as.data.frame(plasma_data)


# 构建相关关系矩阵
library(psych)
data.corr <- corr.test(plasma_data, plasma_data, method = "pearson", adjust="fdr")#
data.r <- data.corr$r  # 相关系数
data.p <- data.corr$p  # p值


library(ggplot2)
library(reshape2)

# 显著性标记函数
getSig <- function(p_value) {
  if (p_value < 0.001) {
    return("***")
  } else if (p_value < 0.01) {
    return("**")
  } else if (p_value < 0.05) {
    return("*")
  } else {
    return("")
  }
}

# 将相关系数和显著性标记矩阵转换为数据框格式
data.r.melt <- melt(data.r)  # 相关系数矩阵展开为长格式
data.p.melt <- melt(data.p)  # p 值矩阵展开为长格式

# 添加显著性标记列
data.r.melt$Sig <- mapply(getSig, data.p.melt$value)

# 设置变量名称
colnames(data.r.melt) <- c("Plasma", "CSF", "Correlation", "Significance")

# 绘制热图
p2 <- ggplot(data.r.melt, aes(x = Plasma, y = CSF, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#0000FF", mid = "#FFFFFF", high = "#FF0000", 
                       midpoint = 0, limits = c(-1, 1), name = "R") +
  geom_text(aes(label = Significance), color = "black", size = 2.5, family = "Arial") +
  theme_minimal(base_size = 8, base_family = "Arial") + 
  theme(
    axis.title = element_blank(),       
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8, face = "bold"),
    axis.text.y = element_text(size = 8, face = "bold"),
    legend.position = "none",  # ★★★ 关键：在这里去掉图例！
    plot.margin = margin(2, 2, 2, 2)
  ) +
  scale_x_discrete(labels = c("p-tau217", "Aβ42", "Left ALPS", "Right ALPS", "cBAG")) +  # 修改X轴标签
  scale_y_discrete(labels = c("p-tau217", "Aβ42", "Left ALPS", "Right ALPS", "cBAG"))    # 修改Y轴标签

# 打印图像
print(p2)


# 保存图形
ggsave("figure2_heatmap_pAD.pdf", p2, width = 2.4, height = 2.4, device = cairo_pdf)



#######----------------------heatmap of  Negative--------------------------------
table(alps_clinic_age$Status)
# 筛选 Negative 数据
Negative_plasma <- subset(alps_clinic_age, Status == "NC")

# 选择特定变量
plasma_data <- Negative_plasma[, alps_variables]

# 创建新的数据框
plasma_data <- as.data.frame(plasma_data)


# 构建相关关系矩阵
library(psych)
data.corr <- corr.test(plasma_data, plasma_data, method = "pearson", adjust="fdr")
data.r <- data.corr$r  # 相关系数
data.p <- data.corr$p  # p值


library(ggplot2)
library(reshape2)

# 显著性标记函数
getSig <- function(p_value) {
  if (p_value < 0.001) {
    return("***")
  } else if (p_value < 0.01) {
    return("**")
  } else if (p_value < 0.05) {
    return("*")
  } else {
    return("")
  }
}

# 将相关系数和显著性标记矩阵转换为数据框格式
data.r.melt <- melt(data.r)  # 相关系数矩阵展开为长格式
data.p.melt <- melt(data.p)  # p 值矩阵展开为长格式

# 添加显著性标记列
data.r.melt$Sig <- mapply(getSig, data.p.melt$value)

# 设置变量名称
colnames(data.r.melt) <- c("Plasma", "CSF", "Correlation", "Significance")

# 绘制热图
p3 <- ggplot(data.r.melt, aes(x = Plasma, y = CSF, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#0000FF", mid = "#FFFFFF", high = "#FF0000", 
                       midpoint = 0, limits = c(-1, 1), name = "R") +
  geom_text(aes(label = Significance), color = "black", size = 2.5, family = "Arial") +
  theme_minimal(base_size = 8, base_family = "Arial") + 
  theme(
    axis.title = element_blank(),       
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8, face = "bold"),
    axis.text.y = element_text(size = 8, face = "bold"),
    legend.position = "none",  # ★★★ 关键：在这里去掉图例！
    plot.margin = margin(1, 1, 1, 1)
  ) +
  scale_x_discrete(labels = c("p-tau217", "Aβ42", "Left ALPS", "Right ALPS", "cBAG")) +  # 修改X轴标签
  scale_y_discrete(labels = c("p-tau217", "Aβ42", "Left ALPS", "Right ALPS", "cBAG"))    # 修改Y轴标签

# 打印图像
print(p3)

# 保存图形
ggsave("figure2_heatmap_NC.pdf", p3, width = 2.4, height = 2.4, device = cairo_pdf)

library(cowplot)

# 1. 创建一个带图例的临时图（参数必须和你的热图完全一致）
df_dummy <- data.frame(x = 1, y = 1, z = 0)
p_temp <- ggplot(df_dummy, aes(x = x, y = y, fill = z)) +
  geom_point(shape = NA) + 
  scale_fill_gradient2(low = "#0000FF", mid = "#FFFFFF", high = "#FF0000", 
                       midpoint = 0, limits = c(-1, 1), name = "r",
                       breaks = c(-1, -0.5, 0, 0.5, 1),
                       labels = c("-1.0", "-0.5", "0.0", "0.5", "1.0"),
                       guide = guide_colorbar(
                         barwidth = unit(0.25, "cm"),   # 色带宽度
                         barheight = unit(2.4, "cm"),   # 色带高度（与热图等高）
                         title.position = "top",
                         title.hjust = 0.5,
                         frame.colour = "black",
                         ticks.colour = "black"
                       )) +
  theme_minimal(base_family = "Arial") +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 7),
    plot.margin = margin(0, 0, 0, 0)
  )

# 2. 提取图例
colorbar_only <- cowplot::get_legend(p_temp)
plot(colorbar_only)
# 3. 导出为 PDF（尺寸按最终在 AI 里占用的空间设，宽0.6，高2.4英寸）
ggsave("colorbar_R.pdf", colorbar_only,
       width = 0.6, height = 2.4, device = cairo_pdf)
# ============================================================
# 每个图内对 All/NC/pAD 三个 p 值单独做 FDR 校正（n=3）
# ============================================================

# 辅助函数：格式化 q 值显示
format_p <- function(p_val) {
  if (is.na(p_val)) return("NA")
  if (p_val < 0.001) return("< 0.001")
  return(sprintf("%.3f", p_val))
}

# 辅助函数：从 lm 模型提取偏相关 r
get_partial_r <- function(model, var_name) {
  coef_tab <- summary(model)$coefficients
  if (!var_name %in% rownames(coef_tab)) return(NA)
  t_val <- coef_tab[var_name, "t value"]
  df <- model$df.residual
  if (is.na(t_val) || df <= 0) return(NA)
  sign(t_val) * sqrt(t_val^2 / (t_val^2 + df))
}

# ============================================================
# 图D: alps_L ~ cBAG
# ============================================================
lm_formula_D <- as.formula(paste("cBAG ~ alps_L +", paste(control_variables, collapse = " + ")))
lm_D_all <- lm(lm_formula_D, data = alps_clinic_age)
lm_D_nc  <- lm(lm_formula_D, data = subset(alps_clinic_age, Status == "NC"))
lm_D_pad <- lm(lm_formula_D, data = subset(alps_clinic_age, Status == "prodromal AD"))

r_D_all <- get_partial_r(lm_D_all, "alps_L")
r_D_nc  <- get_partial_r(lm_D_nc,  "alps_L")
r_D_pad <- get_partial_r(lm_D_pad, "alps_L")

p_D_all <- summary(lm_D_all)$coefficients["alps_L", "Pr(>|t|)"]
p_D_nc  <- summary(lm_D_nc)$coefficients["alps_L", "Pr(>|t|)"]
p_D_pad <- summary(lm_D_pad)$coefficients["alps_L", "Pr(>|t|)"]

# ★ 图D 内独立 FDR 校正（n=3）
q_D <- p.adjust(c(p_D_all, p_D_nc, p_D_pad), method = "fdr")
q_D_all <- q_D[1]; q_D_nc <- q_D[2]; q_D_pad <- q_D[3]

cat("\n========== 图D FDR 校正结果（n=3） ==========\n")
print(data.frame(
  Group = c("All", "NC", "pAD"),
  raw_p = round(c(p_D_all, p_D_nc, p_D_pad), 5),
  q_value = round(q_D, 5)
))

x_max <- max(alps_clinic_age$alps_L, na.rm = TRUE)
y_max <- max(alps_clinic_age$cBAG,   na.rm = TRUE)

gg_D <- ggplot(alps_clinic_age, aes(x = alps_L, y = cBAG, color = factor(Status), shape = factor(Status))) +
  geom_point(na.rm = TRUE, size = 1.5, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, aes(group = 1), linetype = "dashed", color = "#305292", linewidth = 0.5) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.5) +
  labs(x = "Left ALPS", y = "cBAG") +
  theme_minimal(base_size = 8, base_family = "Arial") +
  theme(
    panel.background = element_rect(fill = "white"),
    axis.text.x = element_text(size = 8, face = "bold"),
    axis.text.y = element_text(size = 8, face = "bold"),
    axis.title = element_text(size = 9, face = "bold"),
    legend.position = "none",
    plot.margin = margin(2, 2, 2, 2)
  ) +
  scale_shape_manual(values = c(16, 17), guide = "none") +
  geom_text(x = x_max, y = y_max * 0.98,
            label = paste0("All: r = ", round(r_D_all, 3), ", q ", format_p(q_D_all)),
            color = "#305292", size = 2.8, hjust = 1, vjust = 1) +
  geom_text(x = x_max, y = y_max * 0.88,
            label = paste0("pAD: r = ", round(r_D_pad, 3), ", q ", format_p(q_D_pad)),
            color = "#F36688", size = 2.8, hjust = 1, vjust = 1) +
  geom_text(x = x_max, y = y_max * 0.78,
            label = paste0("NC: r = ", round(r_D_nc, 3), ", q ", format_p(q_D_nc)),
            color = "#AAE3C4", size = 2.8, hjust = 1, vjust = 1) +
  scale_color_manual(values = c("#AAE3C4", "#F36688"))

print(gg_D)
ggsave("figure2_D_alps_L_cBAG_FDR.pdf", gg_D, width = 3.6, height = 3.0, device = cairo_pdf)
ggsave("figure2_D_alps_L_cBAG_FDR.tiff", gg_D, width = 3.6, height = 3.0, dpi = 600, compression = "lzw", bg = "white")

# ============================================================
# 图E: alps_R ~ cBAG
# ============================================================
lm_formula_E <- as.formula(paste("cBAG ~ alps_R +", paste(control_variables, collapse = " + ")))
lm_E_all <- lm(lm_formula_E, data = alps_clinic_age)
lm_E_nc  <- lm(lm_formula_E, data = subset(alps_clinic_age, Status == "NC"))
lm_E_pad <- lm(lm_formula_E, data = subset(alps_clinic_age, Status == "prodromal AD"))

r_E_all <- get_partial_r(lm_E_all, "alps_R")
r_E_nc  <- get_partial_r(lm_E_nc,  "alps_R")
r_E_pad <- get_partial_r(lm_E_pad, "alps_R")

p_E_all <- summary(lm_E_all)$coefficients["alps_R", "Pr(>|t|)"]
p_E_nc  <- summary(lm_E_nc)$coefficients["alps_R", "Pr(>|t|)"]
p_E_pad <- summary(lm_E_pad)$coefficients["alps_R", "Pr(>|t|)"]

# ★ 图E 内独立 FDR 校正（n=3）
q_E <- p.adjust(c(p_E_all, p_E_nc, p_E_pad), method = "fdr")
q_E_all <- q_E[1]; q_E_nc <- q_E[2]; q_E_pad <- q_E[3]

cat("\n========== 图E FDR 校正结果（n=3） ==========\n")
print(data.frame(
  Group = c("All", "NC", "pAD"),
  raw_p = round(c(p_E_all, p_E_nc, p_E_pad), 5),
  q_value = round(q_E, 5)
))

x_max_E <- max(alps_clinic_age$alps_R, na.rm = TRUE)
y_max_E <- max(alps_clinic_age$cBAG,   na.rm = TRUE)

gg_E <- ggplot(alps_clinic_age, aes(x = alps_R, y = cBAG, color = factor(Status), shape = factor(Status))) +
  geom_point(na.rm = TRUE, size = 1.5, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, aes(group = 1), linetype = "dashed", color = "#305292", linewidth = 0.5) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.5) +
  labs(x = "Right ALPS", y = "cBAG") +
  theme_minimal(base_size = 8, base_family = "Arial") +
  theme(
    panel.background = element_rect(fill = "white"),
    axis.text.x = element_text(size = 8, face = "bold"),
    axis.text.y = element_text(size = 8, face = "bold"),
    axis.title = element_text(size = 9, face = "bold"),
    legend.position = "none",
    plot.margin = margin(2, 2, 2, 2)
  ) +
  scale_shape_manual(values = c(16, 17), guide = "none") +
  geom_text(x = x_max_E, y = y_max_E * 0.98,
            label = paste0("All: r = ", round(r_E_all, 3), ", q ", format_p(q_E_all)),
            color = "#305292", size = 2.8, hjust = 1, vjust = 1) +
  geom_text(x = x_max_E, y = y_max_E * 0.88,
            label = paste0("pAD: r = ", round(r_E_pad, 3), ", q = ", format_p(q_E_pad)),
            color = "#F36688", size = 2.8, hjust = 1, vjust = 1) +
  geom_text(x = x_max_E, y = y_max_E * 0.78,
            label = paste0("NC: r = ", round(r_E_nc, 3), ", q ", format_p(q_E_nc)),
            color = "#AAE3C4", size = 2.8, hjust = 1, vjust = 1) +
  scale_color_manual(values = c("#AAE3C4", "#F36688"))

print(gg_E)
ggsave("figure2_E_alps_R_cBAG_FDR.pdf", gg_E, width = 3.6, height = 3.0, device = cairo_pdf)
ggsave("figure2_E_alps_R_cBAG_FDR.tiff", gg_E, width = 3.6, height = 3.0, dpi = 600, compression = "lzw", bg = "white")

# ============================================================
# 图F: alps_L ~ alps_R
# ============================================================
lm_formula_F <- as.formula(paste("alps_R ~ alps_L +", paste(control_variables, collapse = " + ")))
lm_F_all <- lm(lm_formula_F, data = alps_clinic_age)
lm_F_nc  <- lm(lm_formula_F, data = subset(alps_clinic_age, Status == "NC"))
lm_F_pad <- lm(lm_formula_F, data = subset(alps_clinic_age, Status == "prodromal AD"))

r_F_all <- get_partial_r(lm_F_all, "alps_L")
r_F_nc  <- get_partial_r(lm_F_nc,  "alps_L")
r_F_pad <- get_partial_r(lm_F_pad, "alps_L")

p_F_all <- summary(lm_F_all)$coefficients["alps_L", "Pr(>|t|)"]
p_F_nc  <- summary(lm_F_nc)$coefficients["alps_L", "Pr(>|t|)"]
p_F_pad <- summary(lm_F_pad)$coefficients["alps_L", "Pr(>|t|)"]

# ★ 图F 内独立 FDR 校正（n=3）
q_F <- p.adjust(c(p_F_all, p_F_nc, p_F_pad), method = "fdr")
q_F_all <- q_F[1]; q_F_nc <- q_F[2]; q_F_pad <- q_F[3]

cat("\n========== 图F FDR 校正结果（n=3） ==========\n")
print(data.frame(
  Group = c("All", "NC", "pAD"),
  raw_p = round(c(p_F_all, p_F_nc, p_F_pad), 5),
  q_value = round(q_F, 5)
))

x_max_F <- max(alps_clinic_age$alps_L, na.rm = TRUE)
y_max_F <- max(alps_clinic_age$alps_R, na.rm = TRUE)

gg_F <- ggplot(alps_clinic_age, aes(x = alps_L, y = alps_R, color = factor(Status), shape = factor(Status))) +
  geom_point(na.rm = TRUE, size = 1.5, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, aes(group = 1), linetype = "dashed", color = "#305292", linewidth = 0.5) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.5) +
  labs(x = "Left ALPS", y = "Right ALPS") +
  theme_minimal(base_size = 8, base_family = "Arial") +
  theme(
    panel.background = element_rect(fill = "white"),
    axis.text.x = element_text(size = 8, face = "bold"),
    axis.text.y = element_text(size = 8, face = "bold"),
    axis.title = element_text(size = 9, face = "bold"),
    legend.position = "none",
    plot.margin = margin(2, 2, 2, 2)
  ) +
  scale_shape_manual(values = c(16, 17), guide = "none") +
  geom_text(x = x_max_F, y = y_max_F * 0.98 - 0.6,
            label = paste0("All: r = ", round(r_F_all, 3), ", q ", format_p(q_F_all)),
            color = "#305292", size = 2.8, hjust = 1, vjust = 1) +
  geom_text(x = x_max_F, y = y_max_F * 0.96 - 0.6,
            label = paste0("pAD: r = ", round(r_F_pad, 3), ", q ", format_p(q_F_pad)),
            color = "#F36688", size = 2.8, hjust = 1, vjust = 1) +
  geom_text(x = x_max_F, y = y_max_F * 0.94 - 0.6,
            label = paste0("NC: r = ", round(r_F_nc, 3), ", q ", format_p(q_F_nc)),
            color = "#AAE3C4", size = 2.8, hjust = 1, vjust = 1) +
  scale_color_manual(values = c("#AAE3C4", "#F36688"))

print(gg_F)
ggsave("figure2_F_alps_L_alps_R_FDR.pdf", gg_F, width = 3.6, height = 3.0, device = cairo_pdf)
ggsave("figure2_F_alps_L_alps_R_FDR.tiff", gg_F, width = 3.6, height = 3.0, dpi = 600, compression = "lzw", bg = "white")

# ============================================================
# 图G: Ptau217 ~ Aβ42
# ============================================================
lm_formula_G <- as.formula(paste("Aβ42 ~ Ptau217 +", paste(control_variables, collapse = " + ")))
lm_G_all <- lm(lm_formula_G, data = alps_clinic_age)
lm_G_nc  <- lm(lm_formula_G, data = subset(alps_clinic_age, Status == "NC"))
lm_G_pad <- lm(lm_formula_G, data = subset(alps_clinic_age, Status == "prodromal AD"))

r_G_all <- get_partial_r(lm_G_all, "Ptau217")
r_G_nc  <- get_partial_r(lm_G_nc,  "Ptau217")
r_G_pad <- get_partial_r(lm_G_pad, "Ptau217")

p_G_all <- summary(lm_G_all)$coefficients["Ptau217", "Pr(>|t|)"]
p_G_nc  <- summary(lm_G_nc)$coefficients["Ptau217", "Pr(>|t|)"]
p_G_pad <- summary(lm_G_pad)$coefficients["Ptau217", "Pr(>|t|)"]

# ★ 图G 内独立 FDR 校正（n=3）
q_G <- p.adjust(c(p_G_all, p_G_nc, p_G_pad), method = "fdr")
q_G_all <- q_G[1]; q_G_nc <- q_G[2]; q_G_pad <- q_G[3]

cat("\n========== 图G FDR 校正结果（n=3） ==========\n")
print(data.frame(
  Group = c("All", "NC", "pAD"),
  raw_p = round(c(p_G_all, p_G_nc, p_G_pad), 5),
  q_value = round(q_G, 5)
))

x_max_G <- max(alps_clinic_age$Ptau217, na.rm = TRUE)
y_max_G <- max(alps_clinic_age$Aβ42,   na.rm = TRUE)

gg_G <- ggplot(alps_clinic_age, aes(x = Ptau217, y = Aβ42, color = factor(Status), shape = factor(Status))) +
  geom_point(na.rm = TRUE, size = 1.5, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, aes(group = 1), linetype = "dashed", color = "#305292", linewidth = 0.5) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.5) +
  labs(x = "p-tau217", y = "Aβ42") +
  theme_minimal(base_size = 8, base_family = "Arial") +
  theme(
    panel.background = element_rect(fill = "white"),
    axis.text.x = element_text(size = 8, face = "bold"),
    axis.text.y = element_text(size = 8, face = "bold"),
    axis.title = element_text(size = 9, face = "bold"),
    legend.position = "none",
    plot.margin = margin(2, 2, 2, 2)
  ) +
  scale_shape_manual(values = c(16, 17), guide = "none") +
  geom_text(x = x_max_G, y = y_max_G * 0.98,
            label = paste0("All: r = ", round(r_G_all, 3), ", q ", format_p(q_G_all)),
            color = "#305292", size = 2.8, hjust = 1, vjust = 1) +
  geom_text(x = x_max_G, y = y_max_G * 0.94,
            label = paste0("pAD: r = ", round(r_G_pad, 3), ", q = ", format_p(q_G_pad)),
            color = "#F36688", size = 2.8, hjust = 1, vjust = 1) +
  geom_text(x = x_max_G, y = y_max_G * 0.90,
            label = paste0("NC: r = ", round(r_G_nc, 3), ", q = ", format_p(q_G_nc)),
            color = "#AAE3C4", size = 2.8, hjust = 1, vjust = 1) +
  scale_color_manual(values = c("#AAE3C4", "#F36688"))

print(gg_G)
ggsave("figure2_G_Ptau217_Aβ42_FDR.pdf", gg_G, width = 3.6, height = 3.0, device = cairo_pdf)
ggsave("figure2_G_Ptau217_Aβ42_FDR.tiff", gg_G, width = 3.6, height = 3.0, dpi = 600, compression = "lzw", bg = "white")

# ============================================================
# 共享图例（保持不变）
# ============================================================
library(cowplot)
p_temp_legend <- ggplot(alps_clinic_age, aes(x = alps_L, y = cBAG, color = factor(Status), shape = factor(Status))) +
  geom_point(size = 1.5, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, aes(group = 1), linetype = "dashed", color = "#305292") +
  geom_smooth(method = "lm", se = TRUE) +
  scale_color_manual(values = c("#AAE3C4", "#F36688"), labels = c("NC", "pAD")) +
  scale_shape_manual(values = c(16, 17), labels = c("NC", "pAD")) +
  theme_minimal(base_family = "Arial") +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    legend.key = element_rect(fill = "white")
  ) +
  guides(color = guide_legend(override.aes = list(size = 3)),
         shape = guide_legend(override.aes = list(size = 3)))

shared_legend <- cowplot::get_legend(p_temp_legend)
ggsave("shared_legend.pdf", shared_legend, width = 1.2, height = 1.5, device = cairo_pdf)
