# ============================================================
# T1皮层厚度与ALPS/cBAG的相关分析
# 6个AD signature ROI × 3个预测变量 × 3组（全样本/NC/pAD）
# ============================================================
library(table1)
library(readxl)
library(dplyr)
library(openxlsx)
library(ppcor)
library(ggplot2)
library(reshape2)
library(pheatmap)
# 安装

# 加载
library(pheatmap)
rm(list = ls())
setwd("..")  # parent folder contains alps_clinic_age.xlsx
# 读取数据（假设您的数据已合并ALPS、cBAG、认知、分组等信息）
df <- read_xlsx("alps_clinic_age.xlsx")
df$PSMD<-df$PSMD*10000
# 1. 计算6个ROI的左右平均厚度
df$entorhinal <- (df$lh_entorhinal_thickness + df$rh_entorhinal_thickness) / 2
df$inferiortemporal <- (df$lh_inferiortemporal_thickness + df$rh_inferiortemporal_thickness) / 2
df$middletemporal <- (df$lh_middletemporal_thickness + df$rh_middletemporal_thickness) / 2
df$fusiform <- (df$lh_fusiform_thickness + df$rh_fusiform_thickness) / 2
df$precuneus <- (df$lh_precuneus_thickness + df$rh_precuneus_thickness) / 2
df$posteriorcingulate <- (df$lh_posteriorcingulate_thickness + df$rh_posteriorcingulate_thickness) / 2

df$Sex <- ifelse(df$Sex == "Male",1, 0)
#alps_clinic_age$Status<-ifelse(alps_clinic_age$Status=="NC",0,1)
df$APOEe4 <- ifelse(df$APOEe4 == "Carrier",1, 0)
# 2. 定义变量
roi_names <- c("entorhinal", "inferiortemporal", "middletemporal", 
               "fusiform", "precuneus", "posteriorcingulate")
roi_labels <- c("Entorhinal", "Inferior temporal", "Middle temporal",
                "Fusiform", "Precuneus", "Posterior cingulate")

predictors <- c("alps_L", "alps_R", "cBAG")
predictor_labels <- c("Left ALPS", "Right ALPS", "cBAG")

covars <- c("Age", "Sex", "Education", "APOEe4","PSMD","eTIV.y")

numeric_cols <- c("alps_L", "alps_R", "cBAG", 
                  "age", "sex", "education", "APOEe4","PSMD","eTIV.y",
                  roi_names)

for (col in numeric_cols) {
  if (col %in% colnames(df)) {
    df[[col]] <- as.numeric(as.character(df[[col]]))
  }
}
# 3. 偏相关函数
run_pcor <- function(x, y, cov_list, data) {
  vars <- data[, c(x, y, cov_list)]
  vars <- na.omit(vars)
  
  # 检查变量是否存在
  if (!all(c(x, y, cov_list) %in% colnames(vars))) {
    return(c(r = NA, p = NA))
  }
  
  res <- pcor(vars)
  idx_x <- which(colnames(vars) == x)
  idx_y <- which(colnames(vars) == y)
  
  r <- res$estimate[idx_x, idx_y]
  p <- res$p.value[idx_x, idx_y]
  
  return(c(r = r, p = p))
}

# 4. 运行分析的函数
run_analysis <- function(data, group_name) {
  results <- data.frame()
  
  for (i in 1:length(roi_names)) {
    roi <- roi_names[i]
    for (j in 1:length(predictors)) {
      pred <- predictors[j]
      
      res <- run_pcor(pred, roi, covars, data)
      
      results <- rbind(results, data.frame(
        Group = group_name,
        ROI = roi_labels[i],
        Predictor = predictor_labels[j],
        r = res["r"],
        p = res["p"]
      ))
    }
  }
  
  # FDR校正（每组内18个检验）
  results$p_fdr <- p.adjust(results$p, method = "fdr")
  
  return(results)
}

# 5. 三组分析
results_all <- run_analysis(df, "All")
results_nc <- run_analysis(df[df$Status == "NC", ], "NC")
results_pad <- run_analysis(df[df$Status == "prodromal AD", ], "pAD")

# 合并
results_combined <- rbind(results_all, results_nc, results_pad)

# 6. 输出Table S9
write.csv(results_combined, "Table_S9_cortical_thickness_correlations.csv", row.names = FALSE)

# 打印预览
print(results_combined[, c("Group", "ROI", "Predictor", "r", "p", "p_fdr")])

library(pheatmap)
library(grid)
library(gridExtra)
library(reshape2)

library(ggplot2)
library(reshape2)
library(gridExtra)
library(cowplot)

# 统一颜色范围
max_abs <- max(abs(c(results_all$r, results_nc$r, results_pad$r)), na.rm = TRUE)

# ggplot2热图函数
plot_heatmap_gg <- function(results_subset, title, show_legend = FALSE) {
  # 长格式
  df_r <- melt(dcast(results_subset, ROI ~ Predictor, value.var = "r"), 
               id.vars = "ROI", variable.name = "Predictor", value.name = "r")
  df_p <- melt(dcast(results_subset, ROI ~ Predictor, value.var = "p_fdr"),
               id.vars = "ROI", variable.name = "Predictor", value.name = "p_fdr")
  
  df_plot <- merge(df_r, df_p, by = c("ROI", "Predictor"))
  
  # 星号标注
  df_plot$star <- ""
  df_plot$star[df_plot$p_fdr < 0.05] <- "*"
  df_plot$star[df_plot$p_fdr < 0.01] <- "**"
  df_plot$star[df_plot$p_fdr < 0.001] <- "***"
  
  # 设置ROI和Predictor顺序
  df_plot$ROI <- factor(df_plot$ROI, levels = rev(roi_labels))
  df_plot$Predictor <- factor(df_plot$Predictor, levels = predictor_labels)
  
  p <- ggplot(df_plot, aes(x = Predictor, y = ROI, fill = r)) +
    geom_tile(color = "white", size = 0.3) +
    geom_text(aes(label = star), color = "black", size = 5) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                         midpoint = 0, limits = c(-max_abs, max_abs),
                         name = "Partial r") +
    labs(title = title, x = NULL, y = NULL) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 11, color = "black"),
      axis.text.y = element_text(size = 11, color = "black"),
      plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
      panel.grid = element_blank(),
      legend.position = ifelse(show_legend, "right", "none")
    )
  
  return(p)
}

# 生成三张图
p1 <- plot_heatmap_gg(results_all, "A  All participants", show_legend = FALSE)
p2 <- plot_heatmap_gg(results_nc, "B  NC group", show_legend = FALSE)
p3 <- plot_heatmap_gg(results_pad, "C  pAD group", show_legend = FALSE)
p4 <- plot_heatmap_gg(results_all, "", show_legend = TRUE)  # 仅用于提取色阶

# 提取色阶
legend <- get_legend(p4)

# 组图
combined <- plot_grid(
  plot_grid(p1, p2, p3, ncol = 3, rel_widths = c(1, 1, 1)),
  legend,
  ncol = 2,
  rel_widths = c(10, 1)
)
print(combined)
# 添加总标题
#title_gg <- ggdraw() + 
  #draw_label("Figure S3. Associations of ALPS indices and cBAG with cortical thickness in AD-signature regions",
            # fontface = "bold", size = 14)

#final_plot <- plot_grid(title_gg, combined, ncol = 1, rel_heights = c(0.1, 1))
#print(final_plot)
# 保存
ggsave("Figure_S3_combined.tiff", combined, width = 14, height = 5, dpi = 600,compression = "lzw", bg = "white")
ggsave("Figure_S3_combined.pdf", combined, width = 14, height = 5)
