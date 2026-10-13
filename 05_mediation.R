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
library(lavaan)
library(DiagrammeR)
# 清空环境
rm(list = ls())

# 设置路径
setwd("..")

# 导入数据
alps_clinic_age <- read_xlsx("alps_clinic_age.xlsx")
colnames(alps_clinic_age)


str(alps_clinic_age)
colnames(alps_clinic_age)
table(alps_clinic_age$Status)
dim(alps_clinic_age)
colSums(is.na(alps_clinic_age))

####--------------------------------------------结构模型方程-----------------------------------------------------
library(lavaan)

# 数据预处理与变量构建
alps_clinic_age$Cognition <- alps_clinic_age$MMSE
alps_clinic_age$Memory <- alps_clinic_age$AVLT_N13 + alps_clinic_age$AVLT_N4 + alps_clinic_age$AVLT_N5 + alps_clinic_age$AVLT_N6
alps_clinic_age$Executive <- 1/alps_clinic_age$TMTB + alps_clinic_age$STROOP
alps_clinic_age$Attention <- alps_clinic_age$SDMT + alps_clinic_age$DST
alps_clinic_age$Language <- alps_clinic_age$BNT + alps_clinic_age$VFT
alps_clinic_age$Visuospatial <- alps_clinic_age$CDT + alps_clinic_age$ROCF

#alps_clinic_age$Cognition<-alps_clinic_age$MMSE
#alps_clinic_age$Memory<-alps_clinic_age$AVLT_N4+alps_clinic_age$AVLT_N5
#alps_clinic_age$Language<-(alps_clinic_age$BNT+alps_clinic_age$VFT)/2
#alps_clinic_age$Executive<-(alps_clinic_age$TMTB+alps_clinic_age$STROOP)/2
#alps_clinic_age$Attention<-(alps_clinic_age$SDMT+alps_clinic_age$DST)/2
#alps_clinic_age$Visuospatial<-(alps_clinic_age$CDT+alps_clinic_age$ROCF)/2


# 选择需要标准化的变量
variables_to_standardize <- c("Aβ42", "Ptau217", 
                              "alps_L", "alps_R", 
                              "BAG", "cBAG", "HIS", "HBP","Cardiology",
                              "Cognition", "Memory", "Executive", 
                              "Attention", "Language", "Visuospatial",
                              "Sex", "Age", "Education","PSMD")

# 首先确保分类变量已正确编码为数值型
alps_clinic_age$Sex <- ifelse(alps_clinic_age$Sex == "Male",1, 0)
#alps_clinic_age$Status<-ifelse(alps_clinic_age$Status=="NC",0,1)
alps_clinic_age$APOEe4 <- ifelse(alps_clinic_age$APOEe4 == "Carrier",1, 0)
alps_clinic_age$HBP <- ifelse(alps_clinic_age$HBP == "with HBP",1, 0)
alps_clinic_age$Cardiology <- ifelse(alps_clinic_age$Cardiology == "with Cardiology",1, 0)
#alps_clinic_age<-subset(alps_clinic_age, HBP == 0)
#alps_clinic_age<-subset(alps_clinic_age, Cardiology == 0)
# 创建Glymdata_imtu_scaled数据框（复制原始数据框）
alps_clinic_age_scaled <- alps_clinic_age

# 对选定的变量进行标准化
alps_clinic_age_scaled[variables_to_standardize] <- scale(alps_clinic_age[variables_to_standardize])

# 验证标准化结果
summary(alps_clinic_age_scaled[variables_to_standardize])

#alps_clinic_age_scaled<-alps_clinic_age_scaled[alps_clinic_age_scaled$Status=="prodromal AD",]

######----------------------------（1）结构模型方程（总体），添加协变量----------------------
colSums(is.na(alps_clinic_age_scaled))

#定义模型
mediation_model <- '
  # 定义血浆标志物潜变量(X)
  BrainIndices =~ alps_L+alps_R
  
  # 定义脑部指标潜变量(M)
  PlasmaMarkers =~ cBAG
                 
  
  # 定义认知功能潜变量(Y)
  CognitionFactor =~ Cognition + Memory +Executive + Attention + Language + Visuospatial
  
  # 结构模型路径
  PlasmaMarkers ~ a*BrainIndices + Age + Sex + Education + APOEe4 + PSMD 
  CognitionFactor ~ c*BrainIndices + b*PlasmaMarkers + Age + Sex + Education + APOEe4 + PSMD 
  
  # 计算间接效应、直接效应和总效应
  indirect := a*b
  direct := c
  total := c + (a*b)
'

# 模型拟合
fit <- sem(mediation_model, data = alps_clinic_age_scaled)

set.seed(123)
fit_boot <- sem(fit, data = alps_clinic_age_scaled, 
                se = "bootstrap", bootstrap = 5000)
parameterEstimates(fit_boot, standardized = TRUE)
pe <- parameterEstimates(fit_boot, standardized = TRUE)
ind <- pe$std.all[pe$label == "indirect"]
tot <- pe$std.all[pe$label == "total"]
ind / tot


# 模型评估
summary(fit, standardized = TRUE, fit.measures = TRUE)
varTable(fit)
modindices(fit)


# 可视化
library(lavaanPlot)
library(DiagrammeR)

# 创建双中介模型图，不显示协变量
p <- lavaanPlot(
  model = fit, 
  node_options = list(shape = "box", fontname = "SimHei"),
  edge_options = list(color = "grey"),
  coefs = TRUE,      # 包括路径系数
  stand = TRUE,     # 不显示标准化系数
  covs = TRUE,      # 不显示协方差
  stars = TRUE       # 显示显著性星号
)

# 打印模型图
print(p)

ggsave("SEM_dual_mediation_model2.2.png", p, width = 14, height = 12, dpi = 300)


