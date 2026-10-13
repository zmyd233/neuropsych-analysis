#完整的小样本Cole矫正流程（含矫正前后对比+显著性星号）
rm(list = ls())
setwd("..")  # parent folder contains alps_clinic_age.xlsx
library(readxl)
library(ggplot2)
library(dplyr)
library(patchwork)
library(gridExtra)
library(ggpubr)
library(readxl)# 添加显著性星号
library(openxlsx)
# 数据录入
alps_clinic_age <- read.xlsx("alps_clinic_age.xlsx")
hc_data <- subset(alps_clinic_age, Status == "NC")
ad_data <- subset(alps_clinic_age, Status == "prodromal AD")

# ============================================================
# 1. 计算原始BAG（矫正前）
# ============================================================
hc_bag_raw <- hc_data$predicted_age - hc_data$Age
pAD_bag_raw <- ad_data$predicted_age - ad_data$Age

# ============================================================
# 2. 留一法交叉验证矫正（HC组）
# ============================================================
n_hc <- nrow(hc_data)
hc_corrected_loo <- numeric(n_hc)

for (i in 1:n_hc) {
  hc_train <- hc_data[-i, ]
  hc_valid <- hc_data[i, ]
  cole_model <- lm(predicted_age ~ Age, data = hc_train)
  alpha <- coef(cole_model)["Age"]
  beta <- coef(cole_model)["(Intercept)"]
  hc_corrected_loo[i] <- (hc_valid$predicted_age - beta) / alpha
}

# 计算HC组矫正后BAG
hc_bag_corrected <- hc_corrected_loo - hc_data$Age

# ============================================================
# 3. pAD组矫正（使用全部HC数据）
# ============================================================
cole_model_full <- lm(predicted_age ~ Age, data = hc_data)
alpha_full <- coef(cole_model_full)["Age"]
beta_full <- coef(cole_model_full)["(Intercept)"]
pAD_corrected <- (ad_data$predicted_age - beta_full) / alpha_full
pAD_bag_corrected <- pAD_corrected - ad_data$Age

# 新建一列
#alps_clinic_age$cBAG1 <- NA

# NC组
#alps_clinic_age$cBAG1[alps_clinic_age$Status == "NC"] <- hc_bag_corrected

# prodromal AD组
#alps_clinic_age$cBAG1[alps_clinic_age$Status == "prodromal AD"] <- pAD_bag_corrected

#write.xlsx(alps_clinic_age,"alps_clinic_age.xlsx")
alps_clinic_age %>%
  group_by(Status) %>%
  summarise(
    n = n(),
    mean = mean(cBAG, na.rm = TRUE),
    sd = sd(Aβ42, na.rm = TRUE)
  )
t.test(BAG ~ Status,data=alps_clinic_age)
# ============================================================
# 4. 矫正前后对比数据准备
# ============================================================

# 构建长格式数据（用于箱线图对比）
comparison_long <- data.frame(
  BAG = c(hc_bag_raw, hc_bag_corrected, pAD_bag_raw, pAD_bag_corrected),
  Group = factor(c(rep("NC", length(hc_bag_raw)*2), 
                   rep("pAD", length(pAD_bag_raw)*2))),
  Correction = factor(c(rep("Raw", length(hc_bag_raw)), 
                        rep("Cole-corrected", length(hc_bag_corrected)),
                        rep("Raw", length(pAD_bag_raw)), 
                        rep("Cole-corrected", length(pAD_bag_corrected))),
                      levels = c("Raw", "Cole-corrected"))
)

# 构建配对数据（用于个体水平展示）
comparison_paired <- rbind(
  data.frame(ID = 1:nrow(hc_data), Group = "NC", 
             Type = "Raw", BAG = hc_bag_raw),
  data.frame(ID = 1:nrow(hc_data), Group = "NC", 
             Type = "Cole-corrected", BAG = hc_bag_corrected),
  data.frame(ID = 1:nrow(ad_data), Group = "pAD", 
             Type = "Raw", BAG = pAD_bag_raw),
  data.frame(ID = 1:nrow(ad_data), Group = "pAD", 
             Type = "Cole-corrected", BAG = pAD_bag_corrected)
)

# 构建年龄-BAG关系数据
age_bag_data <- rbind(
  data.frame(Age = hc_data$Age, BAG = hc_bag_raw, 
             Group = "NC", Correction = "Raw"),
  data.frame(Age = hc_data$Age, BAG = hc_bag_corrected, 
             Group = "NC", Correction = "Cole-corrected"),
  data.frame(Age = ad_data$Age, BAG = pAD_bag_raw, 
             Group = "pAD", Correction = "Raw"),
  data.frame(Age = ad_data$Age, BAG = pAD_bag_corrected, 
             Group = "pAD", Correction = "Cole-corrected")
)

# ============================================================
# 5. 统计指标对比
# ============================================================

# 计算各组统计指标
stats_summary <- data.frame(
  Group = rep(c("NC", "pAD"), each = 2),
  Correction = rep(c("Raw", "Cole-corrected"), 2),
  N = c(length(hc_bag_raw), length(hc_bag_corrected),
        length(pAD_bag_raw), length(pAD_bag_corrected)),
  Mean = c(mean(hc_bag_raw), mean(hc_bag_corrected),
           mean(pAD_bag_raw), mean(pAD_bag_corrected)),
  SD = c(sd(hc_bag_raw), sd(hc_bag_corrected),
         sd(pAD_bag_raw), sd(pAD_bag_corrected)),
  Median = c(median(hc_bag_raw), median(hc_bag_corrected),
             median(pAD_bag_raw), median(pAD_bag_corrected)),
  Q1 = c(quantile(hc_bag_raw, 0.25), quantile(hc_bag_corrected, 0.25),
         quantile(pAD_bag_raw, 0.25), quantile(pAD_bag_corrected, 0.25)),
  Q3 = c(quantile(hc_bag_raw, 0.75), quantile(hc_bag_corrected, 0.75),
         quantile(pAD_bag_raw, 0.75), quantile(pAD_bag_corrected, 0.75)),
  Min = c(min(hc_bag_raw), min(hc_bag_corrected),
          min(pAD_bag_raw), min(pAD_bag_corrected)),
  Max = c(max(hc_bag_raw), max(hc_bag_corrected),
          max(pAD_bag_raw), max(pAD_bag_corrected))
)

# 四舍五入保留两位小数
stats_summary[, c("Mean", "SD", "Median", "Q1", "Q3", "Min", "Max")] <- 
  round(stats_summary[, c("Mean", "SD", "Median", "Q1", "Q3", "Min", "Max")], 2)

print("=== 矫正前后统计指标对比 ===")
print(stats_summary)

# 保存统计表格
write.csv(stats_summary, "BAG_correction_stats.csv", row.names = FALSE)

# ============================================================
# 6. 验证矫正效果（NC组应该均值为0，与年龄不相关）
# ============================================================

cat("\n=== 验证矫正效果（NC组）===\n")

# NC组矫正前后与0的比较
t_raw_nc <- t.test(hc_bag_raw)
t_corrected_nc <- t.test(hc_bag_corrected)

cat("\n矫正前 NC组 BAG vs 0:\n")
cat(sprintf("Mean = %.2f, 95%% CI [%.2f, %.2f], t = %.2f, p = %.4f\n", 
            t_raw_nc$estimate, t_raw_nc$conf.int[1], t_raw_nc$conf.int[2],
            t_raw_nc$statistic, t_raw_nc$p.value))

cat("\nCole矫正后 NC组 BAG vs 0:\n")
cat(sprintf("Mean = %.2f, 95%% CI [%.2f, %.2f], t = %.2f, p = %.4f\n", 
            t_corrected_nc$estimate, t_corrected_nc$conf.int[1], t_corrected_nc$conf.int[2],
            t_corrected_nc$statistic, t_corrected_nc$p.value))

# 年龄相关性检验
cor_raw_nc <- cor.test(hc_data$Age, hc_bag_raw)
cor_corrected_nc <- cor.test(hc_data$Age, hc_bag_corrected)

cat("\n年龄相关性检验:\n")
cat(sprintf("矫正前: r = %.3f, p = %.4f\n", cor_raw_nc$estimate, cor_raw_nc$p.value))
cat(sprintf("Cole矫正后: r = %.3f, p = %.4f\n", cor_corrected_nc$estimate, cor_corrected_nc$p.value))

# ============================================================
# 7. 组间比较（矫正前后分别比较）
# ============================================================

# 矫正前组间比较
t_test_raw <- t.test(hc_bag_raw, pAD_bag_raw)
wilcox_raw <- wilcox.test(hc_bag_raw, pAD_bag_raw)

# 矫正后组间比较
t_test_corrected <- t.test(hc_bag_corrected, pAD_bag_corrected)
wilcox_corrected <- wilcox.test(hc_bag_corrected, pAD_bag_corrected)

cat("\n=== 组间比较结果 ===\n")
cat("\n矫正前 NC vs pAD:\n")
cat(sprintf("t-test: t = %.2f, p = %.4f\n", t_test_raw$statistic, t_test_raw$p.value))
cat(sprintf("Mann-Whitney U: W = %.0f, p = %.4f\n", wilcox_raw$statistic, wilcox_raw$p.value))

cat("\nCole矫正后 NC vs pAD:\n")
cat(sprintf("t-test: t = %.2f, p = %.4f\n", t_test_corrected$statistic, t_test_corrected$p.value))
cat(sprintf("Mann-Whitney U: W = %.0f, p = %.4f\n", wilcox_corrected$statistic, wilcox_corrected$p.value))

# ============================================================
# 8. 计算显著性星号函数
# ============================================================

get_significance_stars <- function(p_value) {
  if (is.na(p_value)) return("ns")
  if (p_value < 0.001) return("***")
  if (p_value < 0.01) return("**")
  if (p_value < 0.05) return("*")
  return("ns")
}

# 获取各组比较的p值和星号
raw_p <- t_test_raw$p.value
corrected_p <- t_test_corrected$p.value
raw_stars <- get_significance_stars(raw_p)
corrected_stars <- get_significance_stars(corrected_p)

# 计算箱线图的y轴范围用于放置星号
max_raw <- max(c(hc_bag_raw, pAD_bag_raw), na.rm = TRUE)
max_corrected <- max(c(hc_bag_corrected, pAD_bag_corrected), na.rm = TRUE)
min_raw <- min(c(hc_bag_raw, pAD_bag_raw), na.rm = TRUE)
min_corrected <- min(c(hc_bag_corrected, pAD_bag_corrected), na.rm = TRUE)

# 计算星号位置（在箱线图上方）
y_pos_raw <- max_raw + (max_raw - min_raw) * 0.1
y_pos_corrected <- max_corrected + (max_corrected - min_corrected) * 0.1

# ============================================================
# 9. 论文用可视化（组合图，带显著性星号）
# ============================================================

# ---------- Panel A: 矫正前后箱线图对比（带显著性星号）----------
plot_a <- ggplot(comparison_long, aes(x = Correction, y = BAG, fill = Group)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 16, outlier.size = 1.2, width = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.5) +
  annotate("text", x = 1, y = y_pos_raw, 
           label = raw_stars, size = 6, fontface = "bold") +
  annotate("text", x = 2, y = y_pos_corrected, 
           label = corrected_stars, size = 6, fontface = "bold") +
  annotate("segment", x = 0.8, xend = 1.2, 
           y = y_pos_raw - 0.5, yend = y_pos_raw - 0.5, 
           color = "black", linewidth = 0.5) +
  annotate("segment", x = 1.8, xend = 2.2, 
           y = y_pos_corrected - 0.5, yend = y_pos_corrected - 0.5, 
           color = "black", linewidth = 0.5) +
  labs(title = "A", 
       y = "Brain Age Gap (years)", 
       x = "",
       caption = paste0("* p < 0.05, ** p < 0.01, *** p < 0.001\n",
                        "Raw: p = ", format(raw_p, scientific = TRUE, digits = 3),
                        " | corrected: p = ", format(corrected_p, scientific = TRUE, digits = 3))) +
                        #"\nNote: Predominantly negative Raw BAG reflects the known regression-to-the-mean\n",
                        #"effect when the model is applied to this age range, and motivates the age correction.")) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 11),
        plot.caption = element_text(size = 8, color = "gray50", hjust = 0)) +
  scale_fill_manual(values = c("NC" = "#2E86AB", "pAD" = "#A23B72")) +
  scale_x_discrete(labels = c("Raw" = "Raw", "Cole-corrected" = "Corrected"))
print(plot_a)

# ---------- Panel B: 配对连线图（全部 NC + 全部 pAD，分面）----------
set.seed(123)

plot_b_data <- rbind(
  data.frame(ID = paste0("NC_", 1:nrow(hc_data)), Group = "NC",
             Type = "Raw", BAG = hc_bag_raw),
  data.frame(ID = paste0("NC_", 1:nrow(hc_data)), Group = "NC",
             Type = "Cole-corrected", BAG = hc_bag_corrected),
  data.frame(ID = paste0("pAD_", 1:nrow(ad_data)), Group = "pAD",
             Type = "Raw", BAG = pAD_bag_raw),
  data.frame(ID = paste0("pAD_", 1:nrow(ad_data)), Group = "pAD",
             Type = "Cole-corrected", BAG = pAD_bag_corrected)
)

plot_b_data$Type <- factor(plot_b_data$Type, levels = c("Raw", "Cole-corrected"))

# 生成水平 jitter 偏移（稍微减小抖动范围，让X轴更稳定）
plot_b_data$x_jit <- as.numeric(plot_b_data$Type) +
  runif(nrow(plot_b_data), -0.06, 0.06)

plot_b <- ggplot(plot_b_data, aes(x = x_jit, y = BAG, group = ID)) +
  # 1. 线条调淡，但保留趋势感
  geom_line(alpha = 0.15, linewidth = 0.3, color = "gray30") +
  # 2. 点放大，透明度提高，改用空心圆 (shape = 1) 增强辨识度
  geom_point(aes(color = Group), alpha = 0.5, size = 1.5, shape = 16, stroke = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  facet_wrap(~ Group, ncol = 2,
             labeller = labeller(Group = c("NC" = "NC (n=101)",
                                           "pAD" = "pAD (n=95)"))) +
  scale_x_continuous(breaks = c(1, 2),
                     labels = c("Raw", "Corrected"),
                     limits = c(0.7, 2.3)) +
  labs(title = "B",
       y = "Brain Age Gap (years)",
       x = "") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 11),
        strip.text = element_text(size = 11, face = "bold")) +
  scale_color_manual(values = c("NC" = "#2E86AB", "pAD" = "#A23B72"))

print(plot_b)

# ---------- Panel C: 【校正前】原始BAG与年龄的回归图 (Comment 2.3a) ----------
age_bag_raw_data <- subset(age_bag_data, Correction == "Raw")

plot_c <- ggplot(age_bag_raw_data, aes(x = Age, y = BAG, color = Group)) +
  geom_point(alpha = 0.6, size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2, level = 0.95) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  stat_cor(aes(color = Group), method = "pearson",
           label.x.npc = "left", label.y.npc = "top",
           size = 3.5, show.legend = FALSE) +
  labs(title = "C",
       y = "Raw BAG (years)",
       x = "Chronological Age (years)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 11)) +
  scale_color_manual(values = c("NC" = "#2E86AB", "pAD" = "#A23B72"))
print(plot_c)

# ---------- Panel D: 【校正后】BAG与年龄的回归图 ----------
plot_d <- ggplot(subset(age_bag_data, Correction == "Cole-corrected"),
                 aes(x = Age, y = BAG, color = Group)) +
  geom_point(alpha = 0.6, size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2, level = 0.95) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  stat_cor(aes(color = Group), method = "pearson",
           label.x.npc = "left", label.y.npc = "top",
           size = 3.5, show.legend = FALSE) +
  labs(title = "D",
       y = "Corrected BAG (years)",
       x = "Chronological Age (years)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 11)) +
  scale_color_manual(values = c("NC" = "#2E86AB", "pAD" = "#A23B72"))
print(plot_d)

# ---------- 组合四个图：A | B 上排，C | D 下排 ----------
combined_plot <- (plot_a | plot_b) / (plot_c | plot_d) +
  plot_layout(heights = c(1, 1))

print(combined_plot)

# 保存高分辨率图片
ggsave("Figure_S2_BAG_correction_with_stars.png", combined_plot,
       width = 14, height = 11, dpi = 600, bg = "white")
ggsave("Figure_S2_BAG_correction_with_stars.pdf", combined_plot,
       width = 14, height = 11)
ggsave("Figure_S2_BAG_correction_with_stars.tiff", combined_plot,
       width = 14, height = 11, dpi = 600,compression = "lzw", bg = "white")
