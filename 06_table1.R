# ============================================================
# Table 1. Demographic and clinical characteristics (NC vs pAD)
# Generates descriptive statistics with gtsummary and exports to Word.
# Input:  alps_clinic_age.xlsx (analysis dataset, n = 196)
# Output: table1.docx
# ============================================================
library(readxl)
library(dplyr)
library(gtsummary)
library(flextable)
library(broom)

rm(list = ls())
setwd("..")   # parent folder contains alps_clinic_age.xlsx

alps_clinic_age <- read_xlsx("alps_clinic_age.xlsx")

# Variables reported in Table 1
myVars <- c("Status", "Sex", "Age", "Education",
            "MMSE", "ADL", "HIS", "CDR-SB",
            "HBP", "Cardiology", "Diabetes_mellitus", "Dyslipidemia",
            "APOEe4", "A\u03b242", "Ptau217",
            "alps_L", "alps_R", "predicted_age", "BAG", "cBAG",
            "AVLT_N13", "AVLT_N4", "AVLT_N5", "AVLT_N6",
            "BNT", "VFT", "TMTB", "STROOP", "SDMT", "DST", "CDT", "ROCF",
            "PSMD")

selected_set <- alps_clinic_age[, myVars]

# Categorical vs continuous classification
categorical_vars <- c("Status", "Sex", "HBP", "Cardiology",
                      "Diabetes_mellitus", "Dyslipidemia", "APOEe4")
selected_set[categorical_vars] <- lapply(selected_set[categorical_vars], factor)

selected_set <- selected_set %>%
  mutate(ADL = as.numeric(as.character(ADL)),
         HIS = as.numeric(as.character(HIS)),
         CDT = as.numeric(as.character(CDT)))

# Base summary table
tab_base <- selected_set %>%
  tbl_summary(
    by = Status,
    type = list(
      c("ADL", "HIS", "CDT") ~ "continuous",
      c("Sex", "HBP", "Cardiology", "Diabetes_mellitus",
        "Dyslipidemia", "APOEe4") ~ "categorical"
    ),
    statistic = all_continuous() ~ "{mean} \u00b1 {sd}",
    digits = list(all_continuous() ~ 2, all_categorical() ~ 2, "PSMD" ~ 6),
    missing = "no"
  )

# Compute test statistic and p-value for each variable (NC vs pAD)
compute_stats <- function(var_name, group_vec, values) {
  if (is.numeric(values)) {
    res <- broom::tidy(t.test(values ~ group_vec))
    stat <- round(res$statistic[1], 2)
    p <- round(res$p.value[1], 3)
  } else {
    tab <- table(values, group_vec)
    suppressWarnings(chi <- chisq.test(tab, correct = FALSE))
    stat <- round(chi$statistic, 2)
    p <- round(chi$p.value, 3)
  }
  return(c(stat, p))
}

vars <- setdiff(names(selected_set), "Status")
stats_df <- data.frame(variable = vars, statistic = NA_real_, p.value = NA_real_)
for (v in vars) {
  res <- compute_stats(v, selected_set$Status, selected_set[[v]])
  stats_df[stats_df$variable == v, "statistic"] <- res[1]
  stats_df[stats_df$variable == v, "p.value"]   <- res[2]
}

# Merge statistics and export to Word
tab_with_stats <- tab_base %>%
  modify_table_body(~ left_join(.x, stats_df, by = "variable")) %>%
  modify_header(statistic ~ "**statistic (t/\u03c7\u00b2)**",
                p.value   ~ "**P value**")

print(tab_with_stats)

tab_with_stats %>%
  as_flex_table() %>%
  save_as_docx(path = "table1.docx")
