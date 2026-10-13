# ============================================================
# 补：性别分层的 ALPS-cBAG 偏相关 + SEM 结果表 + 交互检验表
# ============================================================
suppressMessages({library(readxl); library(dplyr); library(ppcor); library(lavaan)})
rm(list = ls())
setwd("..")   # parent folder contains alps_clinic_age.xlsx

df <- read_xlsx("alps_clinic_age.xlsx")
df$Cognition    <- df$MMSE
df$Memory       <- df$AVLT_N13 + df$AVLT_N4 + df$AVLT_N5 + df$AVLT_N6
df$Executive    <- 1/df$TMTB + df$STROOP
df$Attention    <- df$SDMT + df$DST
df$Language     <- df$BNT + df$VFT
df$Visuospatial <- df$CDT + df$ROCF
df$Sex    <- ifelse(df$Sex == "Male", 1, 0)
df$APOEe4 <- ifelse(df$APOEe4 == "Carrier", 1, 0)
numvars <- c("alps_L","alps_R","cBAG","Cognition","Memory","Executive",
             "Attention","Language","Visuospatial","Age","Education","APOEe4","PSMD")
for (v in numvars) df[[v]] <- as.numeric(as.character(df[[v]]))
covars <- c("Age","Education","APOEe4","PSMD")

# ============================================================
# Part A: ALPS-cBAG 偏相关（分性别）
# ============================================================
cat("=== Part A: ALPS-cBAG 偏相关（分性别）===\n")
pcor_ab <- data.frame()
for (sex in c("Male","Female")) {
  d <- df[df$Sex == ifelse(sex=="Male",1,0), ]
  for (x in c("alps_L","alps_R")) {
    v <- na.omit(d[, c(x,"cBAG",covars)])
    pc <- ppcor::pcor(v)
    pcor_ab <- rbind(pcor_ab, data.frame(Sex=sex, Pair=paste0(x," vs cBAG"),
                                         r=pc$estimate[x,"cBAG"], p=pc$p.value[x,"cBAG"]))
    cat(sprintf("%s: %s vs cBAG  r=%.3f  p=%.4f\n", sex, x,
                pc$estimate[x,"cBAG"], pc$p.value[x,"cBAG"]))
  }
}
pcor_ab$p_fdr <- p.adjust(pcor_ab$p, method="fdr")
write.csv(pcor_ab, "Table_S12_ALPS_cBAG_by_sex.csv", row.names=FALSE)

# ============================================================
# Part B: 性别分层 SEM 结果表
# ============================================================
std_vars <- c("alps_L","alps_R","cBAG","Cognition","Memory","Executive",
              "Attention","Language","Visuospatial","Age","Education","APOEe4","PSMD")
df_std <- df; df_std[std_vars] <- scale(df[std_vars])

sem_model <- '
  Glymphatic =~ alps_L + alps_R
  CognitionFactor =~ Cognition + Memory + Executive + Attention + Language + Visuospatial
  cBAG ~ a*Glymphatic + Age + Education + APOEe4 + PSMD
  CognitionFactor ~ c*Glymphatic + b*cBAG + Age + Education + APOEe4 + PSMD
  indirect := a*b
  direct := c
  total := c + a*b
'

sem_tab <- data.frame()
for (sex in c("Male","Female")) {
  d <- df_std[df$Sex == ifelse(sex=="Male",1,0), ]
  fit <- sem(sem_model, data = d)
  pe  <- parameterEstimates(fit, standardized = TRUE)
  g <- function(l) as.numeric(pe[pe$label == l, c("std.all","pvalue")])
  a <- g("a"); b <- g("b"); cc <- g("c"); ind <- g("indirect"); tot <- g("total")
  set.seed(123)
  fitb <- sem(sem_model, data = d, se = "bootstrap", bootstrap = 5000)
  peb <- parameterEstimates(fitb, standardized = TRUE)
  indci <- as.numeric(peb[peb$label == "indirect", c("ci.lower","ci.upper")])
  sem_tab <- rbind(sem_tab, data.frame(
    Sex = sex, n = nrow(d),
    a_path = a[1], a_p = a[2],
    b_path = b[1], b_p = b[2],
    c_direct = cc[1], c_p = cc[2],
    indirect = ind[1], ind_low = indci[1], ind_high = indci[2], ind_p = ind[2],
    total = tot[1], total_p = tot[2]))
}
write.csv(sem_tab, "Table_S13_sex_stratified_SEM.csv", row.names = FALSE)
cat("\n=== Part B: 性别分层 SEM（标准化系数）===\n")
print(sem_tab, row.names = FALSE, digits = 3)

# ============================================================
# Part C: 性别 × ALPS 交互检验表（alps_L 和 alps_R）
# ============================================================
cat("\n=== Part C: 性别 × ALPS 交互（全体）===\n")
inter_tab <- data.frame()
for (xv in c("alps_L","alps_R")) {
  for (o in c("cBAG","Cognition","Memory","Executive","Language","Attention","Visuospatial")) {
    m <- lm(as.formula(paste(o, "~", xv, "*Sex + Age + Education + APOEe4 + PSMD")), data = df_std)
    s <- summary(m)$coefficients
    r <- s[grep(paste0(xv, ":Sex"), rownames(s)), , drop = FALSE]
    if (nrow(r) > 0) {
      inter_tab <- rbind(inter_tab, data.frame(
        ALPS = xv, Outcome = o, beta = r[1,"Estimate"], p = r[1,"Pr(>|t|)"]))
      cat(sprintf("%s × Sex -> %-12s beta=%.3f  p=%.4f\n", xv, o, r[1,"Estimate"], r[1,"Pr(>|t|)"]))
    }
  }
}
write.csv(inter_tab, "Table_S14_sex_interaction.csv", row.names = FALSE)
cat("\n已保存: Table_S12_ALPS_cBAG_by_sex.csv, Table_S13_sex_stratified_SEM.csv, Table_S14_sex_interaction.csv\n")
