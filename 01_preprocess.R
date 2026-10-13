# Load required libraries

library(compareGroups)
library(table1)
library(readxl)
library(dplyr)
library(openxlsx)
library(foreign)
library(haven)
library(tidyverse)
library(mice)

# Clear environment to remove previous objects
rm(list = ls())
alps_clinic_age<-read.xlsx("CDR-BS.xlsx")
# 设置工作目录
setwd("..")

#####------------------------------------------- STEP 1: 导入课题组数据，成立成clinic数据 ------------------------------------------------
# Load SPSS data files and select common variables for each file
# Cases 381-2076
data1 <- read.xlsx("库1.xlsx")
data1$NUM  # Display "NUM" column for reference

# Cases 2367- 2731  includes "a150"
data2 <- read.xlsx("库2.xlsx")   # data2 为babri数据，变量缺失且名称不一致
data2$NUM  # Display "NUM" column for reference

# data2数据缺失及AVLT命名与其他data命名不一致，需更正
# 确保在 data2 中将小写列名重命名为 myVars 中使用的大写列名
names(data2)[names(data2) == "N1right"] <- "N1RIGHT"
names(data2)[names(data2) == "N2right"] <- "N2RIGHT"
names(data2)[names(data2) == "N3right"] <- "N3RIGHT"
names(data2)[names(data2) == "N4right"] <- "N4RIGHT"
names(data2)[names(data2) == "N5right"] <- "N5RIGHT"

# 确保 data2 拥有 myVars 列表中需要的全部列，并赋值为缺失值 NA
data2$N6RIGHT <- NA
data2$BNT <- NA
data2$CDT4 <- NA
data2$CWTCRIGH <- NA
data2$N1 <- NA
data2$N2 <- NA


# data3: Cases 2732-3043, includes "a152", "a153", "a154", "a155"
data3 <- read.xlsx("库3.xlsx")
data3$NUM  # Display "NUM" column for reference

# data4: Cases 3044-3200
data4 <- read.xlsx("库4.xlsx")
data4$NUM

# data5: Cases 3201-3340
data5 <- read.xlsx("库5.xlsx")
data5$NUM

# data6: Cases 3341-3639, includes KM001
data6 <- read.xlsx("库6.xlsx")
data6$NUM

# 选择data6$NUM等于3559的行
#NUM_3559 <- data6[data6$NUM == 3559, ]
#print(NUM_3559)
#write.xlsx(NUM_3559, "NUM_3559.xlsx")

# data7: Cases 3640-3775 , includes cases 3613 and 3619 (missing data: 3651,3753)
data7 <- read.xlsx("库7.xlsx")
data7$NUM


# Define the list of variables to be extracted from each dataset
myVars <- c("NUM", "NAME", "C08", "C09", "R10", #C08代表性别 ，C09代表年龄  R10代表受教育程度
            "C021", "C024", "C026",  # 临床诊断分组
            "C042", # 磁共振编号
            "J02",  # 高血压 1 有 2 无 3 不详
            "J03",  # 心脏病 1 有 2 无 3 不详
            "J041", # 糖尿病 1 有 2 无 3 不详
            "J05",  # 高血脂 1 有 2 无 3 不详
            #"T04",  # 利手 1 左 2 右
            "T07",  # 体重指数：BMI
            "J08",
            "J09",
            "T04",
            "CDRDEGRE", # CDR 得分
            "MMSETOTA", # MMSE总分
            "ADLTOTAL", # 生活能力总分
            "H14",      # HIS量表总分
            "N1RIGHT", "N2RIGHT", "N3RIGHT",             # immediate 记忆
            "N4RIGHT", "N5RIGHT", "N6RIGHT", "REYTOTA1", # 延迟记忆
            "BNT", "VFTJS2",                             # 语言功能:VFTJS2
            "REYTOTAL", "CDT4",                          # 视空间能力
            "CWTCRIGH",                                  # Right of STROOP C
            "SDMT1",                                     # 注意功能：SDMT1
            "TMTBT1", "N1", "N2",
            "C016","R03",# 执行功能: TMTBT1 N1+N2
            "SL01","SL02","SL03","SL04","SL051","SL052","SL053","SL054","SL055","SL056","SL057","SL058","SL059","SL05X","SL06","SL07","SL08","SL09","SL10","SL101","SL102","SL103","SL104","SL10X","SL105",
            "SL05X1")
             

myVars <- c("NUM","SL01","SL02","SL03","SL04","SL051","SL052","SL053","SL054","SL055","SL056","SL057","SL058","SL059","SL05X","SL06","SL07","SL08","SL09","SL10","SL101","SL102","SL103","SL104","SL10X","SL105")                     # C016 入组时间 R03 出生年月
myVars<-c("NUM","CDR7")
# Find the columns in myVars that are missing from the data2 column names
#missing_columns <- setdiff(myVars, names(data2))


# Select specified variables from each dataset
selected_data1 <- data1[, myVars]
selected_data2 <- data2[, myVars]
selected_data3 <- data3[, myVars]
selected_data4 <- data4[, myVars]
selected_data5 <- data5[, myVars]
selected_data6 <- data6[, myVars]
selected_data7 <- data7[, myVars]

# Combine all selected data into one dataset
clinic_data <- rbind(selected_data1, selected_data2, selected_data3, selected_data4, selected_data5, selected_data6, selected_data7)

# 找出哪些行有重复

duplicated_rows <- clinic_data[duplicated(clinic_data), ]
unique(duplicated_rows)

# 保留第一行
clinic_data <- clinic_data[!duplicated(clinic_data), ]

# 将NUM列a开头（没有血浆）的被试a替换为9，避免后续转化为num型时为NA
clinic_data$NUM <- gsub("[a]", "9", as.character(clinic_data$NUM))

# 检查num是否有缺失值
sum(is.na(clinic_data$NUM))




#####--------------数据清洗——————————————
# 定义数据清晰函数process_data，临床数据清理
process_data <- function(data) {
 
  
  
  # 更改分类型变量名称和类型
  data <- data %>%
    rename(Sex = C08) %>%
    mutate(Sex = factor(Sex, levels = c(1, 2), labels = c("Male", "Female")))
  
  
  # 更改数值型变量名称和类型
  data <- data %>%
    rename( 
           Education = R10, 
           MMSE = MMSETOTA, 
           CDR = CDRDEGRE, 
           HIS = H14, 
           ADL = ADLTOTAL,
    ) %>%
    mutate(
      Age = as.numeric(Age),
      Education = as.numeric(Education),
      MMSE = as.numeric(MMSE),
      CDR = as.numeric(CDR),
      HIS = as.numeric(HIS),
      ADL = as.numeric(ADL)
    )
  
  # 处理高血压、心脏病、糖尿病、高血脂列
  data$HBP <- ifelse(is.na(data$J02) | (data$J02 != 1), "without HBP", "with HBP")
  data$Cardiology <- ifelse(is.na(data$J03) | (data$J03 != 1), "without", "with Cardiology")
  data$Diabetes_mellitus <- ifelse(is.na(data$J041) | (data$J041 != 1), "without", "with Diabetes_mellitus")
  data$Dyslipidemia <- ifelse(is.na(data$J05) | (data$J05 != 1), "without", "with Dyslipidemia")
  
  return(data)
}

# 定义认知功能数据清理函数
cognition_process <- function(data) {
  # 将涉及的变量转换为数值型
  num_vars <- c("N1RIGHT", "N2RIGHT", "N3RIGHT", "N1", "N2", "N4RIGHT", "N5RIGHT", "N6RIGHT", "BNT", "REYTOTAL", "VFTJS2", "CDT4", "TMTBT1", "CWTCRIGH", "SDMT1")
  data <- data %>%
    mutate(across(all_of(num_vars), as.numeric))
  
  # 创建新变量并根据条件赋值
  data$AVLT_N13 <- data$N1RIGHT + data$N2RIGHT + data$N3RIGHT
  data$DST <- data$N1 + data$N2
  
  # 更改数值型变量名称和类型
  data <- data %>%
    rename(AVLT_N4 = N4RIGHT, 
           AVLT_N5 = N5RIGHT, 
           AVLT_N6 = N6RIGHT, 
           BNT = BNT,
           VFT = VFTJS2,
           ROCF = REYTOTAL,
           CDT = CDT4,
           TMTB = TMTBT1,
           STROOP = CWTCRIGH,
           SDMT = SDMT1
    )
  
  # 返回处理后的数据框
  return(data)
}

# 将每个队列执行process_data和cognition_process
alps_clinic_age <- process_data(dt5)
alps_clinic_age <- cognition_process(alps_clinic_age)
colnames(alps_clinic_age)

########------------缺失值处理--------#########
colSums(is.na(alps_clinic_age))
# 需要插补的特定变量列表
imp_vars <- c("Aβ42","Sex","J02", "J03", "J041","J05","J08","J09","T04",
              "T07","HIS",
              "N1RIGHT", "N2RIGHT","N3RIGHT",
              "AVLT_N4","AVLT_N5","AVLT_N6",
              "REYTOTA1","BNT","VFT","ROCF",
              "CDT","STROOP","SDMT",
              "TMTB","N1","N2","AVLT_N13","DST")


# 使用MICE进行缺失值插补
mice_imp <- mice(alps_clinic_age[,c(imp_vars)], m = 5, method = "pmm", maxit = 50, seed = 123)


#从多重插补对象 mice_imp 中，提取第1次插补生成的完整数据集
alps_imputed <- complete(mice_imp, action = 1) 

# 将插补后的变量合并回原始数据集（保留非插补变量）
alps_clinic_age[, imp_vars] <- alps_imputed[, imp_vars]

# 验证插补结果（缺失值应全部为0）
cat("插补后缺失值数量：\n")
colSums(is.na(alps_clinic_age))

#####-------找出离群值----------#########
colSums(is.na(alps_clinic_age))
data_positive_cleaned <- alps_clinic_age %>% filter(Status == "prodromal AD")
num_vars <- names(data_positive_cleaned)[
  sapply(data_positive_cleaned, is.numeric)
]

outlier_count <- sapply(HCYdata_positive_cleaned[num_vars], function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr_val <- Q3 - Q1
  lower <- Q1 - 3 * iqr_val
  upper <- Q3 + 3 * iqr_val
  sum(!is.na(x) & (x < lower | x > upper))
})

outlier_count

Q1 <- quantile(data_positive_cleaned$Executive, 0.25, na.rm = TRUE)
Q3 <- quantile(data_positive_cleaned$Executive, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

lower_bound <- Q1 - 3 * IQR
upper_bound <- Q3 + 3 * IQR
outlier_num <- data_positive_cleaned$NUM[
  !is.na(data_positive_cleaned$Executive) &
    (data_positive_cleaned$Executive < lower_bound | data_positive_cleaned$Executive > upper_bound)
]

outlier_num

data_negative_cleaned <- alps_clinic_age %>% filter(Status == "NC")

num_vars <- names(data_negative_cleaned)[
  sapply(data_negative_cleaned, is.numeric)
]

outlier_count <- sapply(data_negative_cleaned[num_vars], function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr_val <- Q3 - Q1
  lower <- Q1 - 3 * iqr_val
  upper <- Q3 + 3 * iqr_val
  sum(!is.na(x) & (x < lower | x > upper))
})

outlier_count

Q1 <- quantile(data_negative_cleaned$Executive, 0.25, na.rm = TRUE)
Q3 <- quantile(data_negative_cleaned$Executive, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

lower_bound <- Q1 - 3 * IQR
upper_bound <- Q3 + 3 * IQR
outlier_num <- data_negative_cleaned$NUM[
  !is.na(data_negative_cleaned$Executive) &
    (data_negative_cleaned$Executive < lower_bound | data_negative_cleaned$Executive > upper_bound)
]

outlier_num

#####---------------------------------离群值处理————————————————————————————###############
# 2. 使用 Tukey 方法检测离群值
# 定义函数来识别和处理离群值
handle_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  # 替换超出1.5倍IQR的值为Q1或Q3
  x[x < lower_bound] <- Q1
  x[x > upper_bound] <- Q3
  return(x)
}

# 需要处理离群值的特定变量列表
outlier_vars <- c("Aβ42","Ptau217","cBAG","alps_L","alps_R","N1RIGHT", "N2RIGHT","N3RIGHT",
                  "AVLT_N4","AVLT_N5","AVLT_N6",
                  "REYTOTA1","BNT","VFT","ROCF",
                  "CDT","STROOP","SDMT",
                  "TMTB","N1","N2","AVLT_N13","DST")

# 提取ptau217_group == Positive 的数据集
data_positive_cleaned <- alps_clinic_age %>% filter(Status == "prodromal AD")

# 对datan_egative_im进行离群值检查和替换
data_positive_cleaned[outlier_vars] <- lapply(data_positive_cleaned[outlier_vars], handle_outliers)

# 提取ptau217_group == Negative 的数据集
data_negative_cleaned <- alps_clinic_age %>% filter(Status == "NC")

# 对HCYdatan_egative_im进行离群值检查和替换
data_negative_cleaned[outlier_vars] <- lapply(data_negative_cleaned[outlier_vars], handle_outliers)

# 合并正负数据集
alps_clinic_age <- rbind(data_positive_cleaned, data_negative_cleaned)

# 查看数据的维度和列名
dim(alps_clinic_age)
colnames(alps_clinic_age)





