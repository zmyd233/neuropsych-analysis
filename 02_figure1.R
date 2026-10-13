# Load necessary libraries
library(pROC)
library(glmnet)
library(pROC)
library(table1)
library(readxl)
library(dplyr)
library(openxlsx)
library(foreign)
library(haven)
library(ggplot2)
library(compareGroups)
library(mice)
library(outliers)
library(haven)
library(tidyr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggtext)
library(grid)
library(ggsignif)
library(patchwork)

# 加载glue包，提供字符串插值功能
suppressMessages(suppressWarnings(library(glue)))

# 加载dplyr包，提供数据操作功能
suppressMessages(suppressWarnings(library(dplyr)))

# 加载grid包，提供图形布局功能
suppressMessages(suppressWarnings(library(grid)))

# 加载ggtext包，增强ggplot2图形中的文本功能
suppressMessages(suppressWarnings(library(ggtext)))

# 加载ggplot2包，提供数据可视化功能
suppressMessages(suppressWarnings(library(ggplot2)))

# 加载reshape2包，提供数据重整和转换功能
suppressMessages(suppressWarnings(library(reshape2)))

# 加载ggsignif包，提供在ggplot2中添加统计显著性标注的功能
suppressMessages(suppressWarnings(library(ggsignif)))

# 加载patchwork包，提供将多个ggplot2图形拼接成一个图的功能
suppressMessages(suppressWarnings(library(patchwork)))


# 清空环境
rm(list = ls())

# 设置工作目录
setwd("..")
alps_clinic_age <- read.xlsx("alps_clinic_age.xlsx")

####—————————————————————————————— 定义图形函数 -----------------------------------
#jitter_height = 0.3
generate_variable_plot <- function(data, 
                                   variable_group,
                                   ylim_range, 
                                   y_ticks, 
                                   signif_position, 
                                   plot_title, 
                                   color_fill, 
                                   gradient_colors,
                                   jitter_height = 0.02) {
  
  # 渐变背景
  background_gradient <- rasterGrob(
    colorRampPalette(gradient_colors)(256),
    width = unit(1, 'npc'),
    height = unit(1, 'npc'),
    interpolate = TRUE
  )
  
  # 主绘图对象
  combined_plot <- ggplot(
    data = subset(data, Variable == variable_group),
    aes(
      x = Status,
      y = Value,
      fill = Status  # 仅映射填充色到Status
    )
  ) +
    annotation_custom(background_gradient, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    
    # 箱线图（底层）- 保持黑色边框
    geom_boxplot(
      width = 0.72,
      color = "black",
      fill = "white",
      outlier.shape = NA,
      lwd = 0.5,
      alpha = 0.8
    ) +
    
    # 散点图（上层）- 单独映射颜色
    geom_jitter(
      aes(fill = Status),
      shape = 21,
      color = "black",
      stroke = 0.5,
      width = 0.1,
      height = jitter_height,
      alpha = 0.9,
      size = 2
    ) +
    scale_fill_manual(values = c("white", color_fill)) +
    
    # 显著性标记
    geom_signif(
      comparisons = list(c("NC", "pAD")),  # 注意拼写应为 "Negative"
      map_signif_level = function(p) {
        ifelse(
          p < 0.001, 
          "italic(p) < 0.001", 
          sprintf("italic(p) == %.3f", p)
        )
      },
      y_position = signif_position,  # 保持原有位置参数
      test = "t.test",
      textsize = 3.6,
      tip_length = 0.03,
      parse = TRUE  # 必须保留以解析数学表达式
    ) +
    
    # 颜色和填充设置
    scale_fill_manual(values = c("white", color_fill)) +
    scale_color_manual(values = c("#999999", color_fill)) +  # 单独控制散点颜色
    
    # 坐标轴设置
    scale_y_continuous(
      limits = ylim_range,
      breaks = y_ticks,
      expand = c(0, 0)
    ) +
    
    # 标签和主题
    labs(
      title = plot_title,
      x = NULL,
      y = ifelse(variable_group == "OI",  # 仅第一个图显示y轴标签
                 "Scores",
                 "")
    ) +
    theme_classic(base_size = 10) +
    theme(
      legend.position ="none",
      plot.margin = margin(t = 5, r = 10, b = 0, l = 10),
      plot.title = element_textbox_simple(
        size = 12,
        face = "bold",
        color = "#000000",
        halign = 0.5,
        lineheight = 1.25,
        fill = color_fill,
        width = 1.2,
        padding = margin(5, 0, 5, 0),
        margin = margin(0, 0, 20, 0)
      ),
      axis.text.x = element_text(
        angle = 45, 
        hjust = 1, 
        size = 14, 
        color = "black",
        margin = margin(t = 5)  # 保持x轴标签间距
      ),
      axis.text.y = element_text(
        size = 12, 
        color = "black",
        margin = margin(r = 5)  # 保持y轴标签间距
      ),
      axis.line = element_line(color = "black", linewidth = 0.5),  # 坐标轴线样式
      axis.ticks = element_line(color = "black"),  # 刻度线颜色
      axis.title.y = element_text(
        size = 14,
        margin = margin(r = 10)  # 保持y轴标题间距
      )
    )
  
  return(combined_plot)
}

######--------------------导入数据，定义图形函数-----------------------############
# load set
alps_clinic_age <- read_xlsx("alps_clinic_age.xlsx")
dim(alps_clinic_age)
colnames(alps_clinic_age)
colSums(is.na(alps_clinic_age))
alps_clinic_age$Status<-ifelse(alps_clinic_age$Status=="prodromal AD","pAD","NC")
table(alps_clinic_age$Status)



# 选择需要的列组成新的数据框并转化为longdata
plasma_long <- alps_clinic_age %>%
  pivot_longer(cols = c(Ptau217, 'Aβ42',  alps_L,  alps_R, cBAG),    # 选择变量列
               names_to = "Variable",                            # 转换后的变量名列
               values_to = "Value") %>%                          # 转换后的数值列
  dplyr::select(Status, Variable, Value)                                # 仅保留 Status, Variable 和 Value 列



# 确保 Status 是因子
plasma_long$Status <- factor(plasma_long$Status)

# 查看转换后的结果
head(plasma_long)
summary(plasma_long$Value)


#计算y_pos
summary(alps_clinic_age$alps_R)
max_values <-tapply(alps_clinic_age$alps_L, alps_clinic_age$alps_L, max)
offset <- 0.1 * max(max_values)
y_pos <- max(max_values) + offset 
y_pos

# 定义变量组的参数配置
plot_configs <- list(
  list(
    variable_group = "Ptau217",
    title = "p-tau217",
    ylim = c(0 , 1.6),
    yticks = seq(0, 1.6, 0.4),
    signif_y = 1.2177,
    jitter_height = 0.01,
    fill_color = "#FFDD8E",
    gradient_bg = c("#FFFFFF", "#F8F6C4", "#FFDD8E")
  ),
  list(
    variable_group = "Aβ42",
    title = "Aβ42",
    ylim = c(10, 50),
    yticks = seq(10, 50, 10),
    signif_y = 43.472,
    jitter_height = 0.5,
    fill_color = "#49B2C7",
    gradient_bg = c("#FFFFFF", "#7AC3DF", "#49B2C7")
  ),
  list(
    variable_group = "alps_L",
    title = "Left ALPS",
    ylim = c(0.8, 2.0),
    yticks = seq(0.8, 2.0, 0.3),
    signif_y = 1.802845,
    jitter_height = 0.01,
    fill_color = "#2C9678",
    gradient_bg = c("#FFFFFF", "#70CDBE", "#2C9678")
  ),
  list(
    variable_group = "alps_R",
    title = "Right ALPS",
    ylim = c(0.8, 2.0),
    yticks = seq(0.8, 2.0, 0.3),
    signif_y = 1.826748,
    jitter_height = 0.01,
    fill_color = '#AC99D2',  # 填充颜色
    gradient_bg = c('#FFFFFF', '#B3A6C6', '#AC99D2')  # 渐变背景颜色
  ),
  list(
    variable_group = "cBAG",
    title = "cBAG",
    ylim = c(-30, 30),
    yticks = seq(-30, 30, 15),
    signif_y = 23.93194,
    jitter_height = 0.5,
    fill_color = '#EB7E60',  # 填充颜色
    gradient_bg = c('#FFFFFF', '#E3B4B6','#EB7E60')  # 渐变背景颜色
  )
)

# 生成图形（修改循环逻辑）
variable_plots <- lapply(plot_configs, function(config) {
  generate_variable_plot(
    data = plasma_long,  # 使用转换后的长格式数据
    variable_group = config$variable_group,
    ylim_range = config$ylim,
    y_ticks = config$yticks,
    signif_position = config$signif_y,
    plot_title = config$title,
    color_fill = config$fill_color,
    gradient_colors = config$gradient_bg
  )
})

# 合并图形（改为5列）
gg <- wrap_plots(variable_plots, ncol = 5)

print(gg)

# 保存输出
ggsave(
  filename = "figure1.png",
  plot = gg,
  width = 10,
  height =5,
  dpi = 600,
  bg = "white"
)

ggsave(
  filename = "figure1.pdf",
  plot = gg,
  width = 10,
  height =5,
  bg = "white"
)

ggsave(
  filename = "figure1.tiff",
  plot = gg,
  width = 10,
  height =5,
  dpi = 600,
  compression = "lzw",
  bg = "white"
)














