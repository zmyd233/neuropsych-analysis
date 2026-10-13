# ============================================================
library(readxl)
library(dplyr)
library(ppcor)
library(ggplot2)
library(cowplot)

rm(list = ls())

# ---------- 0. 工作目录 ----------
setwd("..")  # parent folder contains alps_clinic_age.xlsx

# ---------- 1. 读取数据 ----------
df <- read_xlsx("alps_clinic_age.xlsx")

# ---------- 2. 协变量编码（Sex / APOEe4 转为 0/1）----------
df$Sex    <- ifelse(df$Sex == "Male", 1, 0)
df$APOEe4 <- ifelse(df$APOEe4 == "Carrier", 1, 0)

# ---------- 4. 变量定义 ----------
y_vars   <- c("alps_L", "alps_R", "cBAG")
y_labels <- c("Left ALPS", "Right ALPS", "cBAG")
covars   <- c("Age", "Sex", "Education", "APOEe4")

for (col in c("PSMD", y_vars, covars)) {
  df[[col]] <- as.numeric(as.character(df[[col]]))
}

# ---------- 5. 偏相关函数（ppcor::pcor）----------
run_pcor <- function(x, y, cov_list, data) {
  vars <- data[, c(x, y, cov_list)]
  vars <- na.omit(vars)
  if (!all(c(x, y, cov_list) %in% colnames(vars))) return(c(r = NA, p = NA))
  res   <- ppcor::pcor(vars)
  idx_x <- which(colnames(vars) == x)
  idx_y <- which(colnames(vars) == y)
  return(c(r = res$estimate[idx_x, idx_y], p = res$p.value[idx_x, idx_y]))
}

# ---------- 6. 分组偏相关 + FDR 校正 ----------
run_analysis <- function(data, group_name) {
  results <- data.frame()
  for (j in seq_along(y_vars)) {
    res <- run_pcor("PSMD", y_vars[j], covars, data)
    results <- rbind(results, data.frame(
      Group    = group_name,
      Variable = y_labels[j],
      r        = as.numeric(res["r"]),
      p        = as.numeric(res["p"])
    ))
  }
  results$p_fdr <- p.adjust(results$p, method = "fdr")
  return(results)
}

results_all <- run_analysis(df, "All")
results_nc  <- run_analysis(df[df$Status == "NC", ], "NC")
results_pad <- run_analysis(df[df$Status == "prodromal AD", ], "pAD")

results_combined <- rbind(results_all, results_nc, results_pad)
write.csv(results_combined, "Table_S11_PSMD_correlations_by_group.csv", row.names = FALSE)
cat("\n========== 分组偏相关结果预览 ==========\n")
print(results_combined[, c("Group", "Variable", "r", "p", "p_fdr")])

# ============================================================
# 准备绘图数据：PSMD 放大 10^4 倍（仅影响坐标轴显示，不影响偏相关）
# ============================================================
alps_clinic_age <- df
alps_clinic_age$PSMD <- alps_clinic_age$PSMD * 10^4

# ============================================================
# 每个图内对 All/NC/pAD 三个 p 值单独做 FDR（n=3）
# results_combined 已经包含每个组的偏相关 r 和 p
# 对每个变量（Left ALPS / Right ALPS / cBAG），对 3 个组的 p 值做 FDR
# ============================================================
q_lookup <- list()
for (var_label in c("Left ALPS", "Right ALPS", "cBAG")) {
  idx <- results_combined$Variable == var_label
  q_vals <- p.adjust(results_combined$p[idx], method = "fdr")
  for (i in seq_len(sum(idx))) {
    group <- results_combined$Group[which(idx)[i]]
    q_lookup[[paste(var_label, group, sep = "_")]] <- q_vals[i]
  }
}

# 查看每个图内的 FDR 校正结果
cat("\n========== 每个图内 FDR 校正结果（n=3） ==========\n")
for (var_label in c("Left ALPS", "Right ALPS", "cBAG")) {
  idx <- results_combined$Variable == var_label
  cat("\n---", var_label, "---\n")
  print(data.frame(
    Group   = results_combined$Group[idx],
    raw_p   = round(results_combined$p[idx], 5),
    q_value = round(p.adjust(results_combined$p[idx], method = "fdr"), 5)
  ))
}

# ============================================================
# 辅助函数：格式化 q 值显示
# ============================================================
format_q <- function(q_val) {
  if (is.na(q_val)) return("NA")
  if (q_val < 0.001) return("< 0.001")
  return(sprintf("%.3f", q_val))
}

# ============================================================
# 辅助函数：从 results_combined 中提取 r 和 q
# ============================================================
get_stats <- function(group, var_label) {
  row <- results_combined[results_combined$Group == group & results_combined$Variable == var_label, ]
  if (nrow(row) == 0) return(list(r = NA, q = NA))
  q_val <- q_lookup[[paste(var_label, group, sep = "_")]]
  list(r = round(row$r, 3), q = q_val)
}

# ============================================================
# 图A: PSMD vs Left ALPS
# ============================================================
stats_A_all <- get_stats("All", "Left ALPS")
stats_A_nc  <- get_stats("NC",  "Left ALPS")
stats_A_pad <- get_stats("pAD", "Left ALPS")

x_max_A <- max(alps_clinic_age$PSMD,  na.rm = TRUE)
y_max_A <- max(alps_clinic_age$alps_L, na.rm = TRUE)

gg_A <- ggplot(alps_clinic_age, aes(x = PSMD, y = alps_L, color = factor(Status), shape = factor(Status))) +
  geom_point(na.rm = TRUE, size = 1.5, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, aes(group = 1), linetype = "dashed", color = "#305292", linewidth = 0.5) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.5) +
  labs(x = "PSMD (x10^-4)", y = "Left ALPS") +
  theme_minimal(base_size = 8, base_family = "Arial") +
  theme(
    panel.background = element_rect(fill = "white"),
    axis.text.x = element_text(size = 8, face = "bold"),
    axis.text.y = element_text(size = 8, face = "bold"),
    axis.title = element_text(size = 9, face = "bold"),
    legend.position = "none"
  ) +
  scale_shape_manual(values = c(16, 17), guide = "none") +
  geom_text(x = x_max_A, y = y_max_A * 0.98,
            label = paste0("All: r = ", stats_A_all$r, ", q = ", format_q(stats_A_all$q)),
            color = "#305292", size = 2.8, hjust = 1, vjust = 1) +
  geom_text(x = x_max_A, y = y_max_A * 0.94,
            label = paste0("pAD: r = ", stats_A_pad$r, ", q = ", format_q(stats_A_pad$q)),
            color = "#F36688", size = 2.8, hjust = 1, vjust = 1) +
  geom_text(x = x_max_A, y = y_max_A * 0.90,
            label = paste0("NC: r = ", stats_A_nc$r, ", q = ", format_q(stats_A_nc$q)),
            color = "#AAE3C4", size = 2.8, hjust = 1, vjust = 1) +
  scale_color_manual(values = c("#AAE3C4", "#F36688"))

print(gg_A)
ggsave("Figure_S4_A_alps_L_PSMD_FDR.pdf",  gg_A, width = 3.6, height = 3.0, device = cairo_pdf)
ggsave("Figure_S4_A_alps_L_PSMD_FDR.tiff", gg_A, width = 3.6, height = 3.0, dpi = 600, compression = "lzw", bg = "white")

# ============================================================
# 图B: PSMD vs Right ALPS
# ============================================================
stats_B_all <- get_stats("All", "Right ALPS")
stats_B_nc  <- get_stats("NC",  "Right ALPS")
stats_B_pad <- get_stats("pAD", "Right ALPS")

x_max_B <- max(alps_clinic_age$PSMD,  na.rm = TRUE)
y_max_B <- max(alps_clinic_age$alps_R, na.rm = TRUE)

gg_B <- ggplot(alps_clinic_age, aes(x = PSMD, y = alps_R, color = factor(Status), shape = factor(Status))) +
  geom_point(na.rm = TRUE, size = 1.5, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, aes(group = 1), linetype = "dashed", color = "#305292", linewidth = 0.5) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.5) +
  labs(x = "PSMD (x10^-4)", y = "Right ALPS") +
  theme_minimal(base_size = 8, base_family = "Arial") +
  theme(
    panel.background = element_rect(fill = "white"),
    axis.text.x = element_text(size = 8, face = "bold"),
    axis.text.y = element_text(size = 8, face = "bold"),
    axis.title = element_text(size = 9, face = "bold"),
    legend.position = "none"
  ) +
  scale_shape_manual(values = c(16, 17), guide = "none") +
  geom_text(x = x_max_B, y = y_max_B * 0.98,
            label = paste0("All: r = ", stats_B_all$r, ", q = ", format_q(stats_B_all$q)),
            color = "#305292", size = 2.8, hjust = 1, vjust = 1) +
  geom_text(x = x_max_B, y = y_max_B * 0.94,
            label = paste0("pAD: r = ", stats_B_pad$r, ", q = ", format_q(stats_B_pad$q)),
            color = "#F36688", size = 2.8, hjust = 1, vjust = 1) +
  geom_text(x = x_max_B, y = y_max_B * 0.90,
            label = paste0("NC: r = ", stats_B_nc$r, ", q = ", format_q(stats_B_nc$q)),
            color = "#AAE3C4", size = 2.8, hjust = 1, vjust = 1) +
  scale_color_manual(values = c("#AAE3C4", "#F36688"))

print(gg_B)
ggsave("Figure_S4_B_alps_R_PSMD_FDR.pdf",  gg_B, width = 3.6, height = 3.0, device = cairo_pdf)
ggsave("Figure_S4_B_alps_R_PSMD_FDR.tiff", gg_B, width = 3.6, height = 3.0, dpi = 600, compression = "lzw", bg = "white")

# ============================================================
# 图C: PSMD vs cBAG
# ============================================================
stats_C_all <- get_stats("All", "cBAG")
stats_C_nc  <- get_stats("NC",  "cBAG")
stats_C_pad <- get_stats("pAD", "cBAG")

x_max_C <- max(alps_clinic_age$PSMD, na.rm = TRUE)
y_max_C <- max(alps_clinic_age$cBAG, na.rm = TRUE)

gg_C <- ggplot(alps_clinic_age, aes(x = PSMD, y = cBAG, color = factor(Status), shape = factor(Status))) +
  geom_point(na.rm = TRUE, size = 1.5, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, aes(group = 1), linetype = "dashed", color = "#305292", linewidth = 0.5) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.5) +
  labs(x = "PSMD (x10^-4)", y = "cBAG") +
  theme_minimal(base_size = 8, base_family = "Arial") +
  theme(
    panel.background = element_rect(fill = "white"),
    axis.text.x = element_text(size = 8, face = "bold"),
    axis.text.y = element_text(size = 8, face = "bold"),
    axis.title = element_text(size = 9, face = "bold"),
    legend.position = "none"
  ) +
  scale_shape_manual(values = c(16, 17), guide = "none") +
  geom_text(x = x_max_C, y = y_max_C * 0.98-40,
            label = paste0("All: r = ", stats_C_all$r, ", q ", format_q(stats_C_all$q)),
            color = "#305292", size = 2.8, hjust = 1, vjust = 1) +
  geom_text(x = x_max_C, y = y_max_C * 0.88-40,
            label = paste0("pAD: r = ", stats_C_pad$r, ", q = ", format_q(stats_C_pad$q)),
            color = "#F36688", size = 2.8, hjust = 1, vjust = 1) +
  geom_text(x = x_max_C, y = y_max_C * 0.78-40,
            label = paste0("NC: r = ", stats_C_nc$r, ", q ", format_q(stats_C_nc$q)),
            color = "#AAE3C4", size = 2.8, hjust = 1, vjust = 1) +
  scale_color_manual(values = c("#AAE3C4", "#F36688"))

print(gg_C)
ggsave("Figure_S4_C_cBAG_PSMD_FDR.pdf",  gg_C, width = 3.6, height = 3.0, device = cairo_pdf)
ggsave("Figure_S4_C_cBAG_PSMD_FDR.tiff", gg_C, width = 3.6, height = 3.0, dpi = 600, compression = "lzw", bg = "white")

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
