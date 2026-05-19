# 安装所有必需的包（如果尚未安装）
install.packages(c(
  "ggplot2",   "ggpubr",    "dplyr",     "tidyr",     "rstatix",   
  "purrr",     "gridExtra", "ggtext",    "ragg",      "extrafont", 
  "cowplot",   "stringr",   "gtable",    "grid"
))
install.packages(c(  "gtable",    "grid"
))
library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyr)
library(rstatix)
library(purrr)
library(gridExtra)
library(ggtext)
library(ragg)
library(extrafont)
library(cowplot)
library(stringr)
library(readr)
library(gtable)
library(grid)

# 确保Times New Roman字体可用
if("Times New Roman" %in% fonts()) {
  par(family = "Times New Roman")
} else {
  warning("Times New Roman字体未找到，将使用默认字体")
}

# 读取数据
data <- read_csv("age_regression109_PVSBG10.csv")

# 数据预处理
clean_data <- data %>%
  rename(
    PVSVF_Total = `PVSVF-Total`,
    PVSVF_WM = `PVSVF-WM`,
    PVSVF_BG = `PVSVF-BG`,
    FW_WM = `FW-WM`,
    FW_PWMH = `FW-PWMH`,
    FW_DWMH = `FW-DWMH`,
    Ave_DTI_ALPS = `DTI-ALPS index`,
    L_DTI_ALPS = `L-DTI-ALPS index`,
    R_DTI_ALPS = `R-DTI-ALPS index`
  ) %>%
  mutate(
    sex = factor(sex, levels = c(1, 2), labels = c("Male", "Female"))
  ) %>%
  bind_rows(mutate(., sex = "Total")) %>%
  # 将sex因子水平设置为Total, Male, Female的顺序
  mutate(sex = factor(sex, levels = c("Total", "Male", "Female")))

# 定义新名称映射
new_names <- c(
  "PVSVF_Total" = "PVSVF-Total",
  "PVSVF_WM" = "PVSVF-WM",
  "PVSVF_BG" = "PVSVF-BG",
  "FW_WM" = "FW-WM",
  "FW_PWMH" = "FW-PWMH",
  "FW_DWMH" = "FW-DWMH",
  "Ave_DTI_ALPS" = "Ave-DTI-ALPS index",
  "L_DTI_ALPS" = "L-DTI-ALPS index",
  "R_DTI_ALPS" = "R-DTI-ALPS index"
)

# 设置颜色方案（根据您图片中的要求）
group_colors <- c(
  "Total" = "#999999",   # 灰色
  "Male" = "#00CED1",    # 青色
  "Female" = "#F08080"  # 橙色
)

# 正态性信息矩阵
normality_info <- tribble(
  ~metric,          ~Total,       ~Male,         ~Female,
  "PVSVF_Total",    "non-normal", "non-normal",  "non-normal",
  "PVSVF_WM",       "non-normal", "non-normal",  "non-normal",
  "PVSVF_BG",       "non-normal", "non-normal",  "non-normal",
  "FW_WM",          "non-normal", "normal",      "non-normal",
  "FW_PWMH",        "non-normal", "non-normal",  "non-normal",
  "FW_DWMH",        "normal",     "normal",      "non-normal",
  "Ave_DTI_ALPS",   "normal",     "normal",      "normal",
  "L_DTI_ALPS",     "normal",     "normal",      "normal",
  "R_DTI_ALPS",     "non-normal", "non-normal",  "non-normal"
) %>%
  pivot_longer(
    cols = -metric,
    names_to = "sex",
    values_to = "distribution") %>%
  # 应用新名称
  mutate(metric = new_names[metric])


create_correlation_plot <- function(data, metrics, y_label, facet_order) {
  # 应用新名称到指标
  metrics_renamed <- new_names[metrics]
  
  # 转换为长格式
  long_data <- data %>%
    pivot_longer(
      cols = all_of(metrics),
      names_to = "metric",
      values_to = "value"
    ) %>%
    mutate(metric = factor(metric, 
                           levels = facet_order, 
                           labels = new_names[facet_order],
                           ordered = TRUE))
  
  # 处理PVSVF缺失值
  long_data <- long_data %>%
    filter(!(metric %in% c("PVSVF-Total", "PVSVF-WM", "PVSVF-BG") & is.na(value)))
  
  # 改进后的相关性计算函数（处理重复值问题）
  calculate_correlation <- function(age, value) {
    # 检查样本量
    if (length(age) < 3 || length(value) < 3) {
      return(list(estimate = NA, p.value = NA, method = "Insufficient data"))
    }
    
    # 正态性检验（添加错误处理）
    safe_shapiro <- function(x) {
      tryCatch({
        st <- shapiro.test(x)
        st$p.value > 0.05
      }, error = function(e) FALSE)
    }
    
    norm_age <- safe_shapiro(age)
    norm_value <- safe_shapiro(value)
    bivariate_normal <- norm_age && norm_value
    
    # 选择相关性检验方法
    if (bivariate_normal) {
      ct <- tryCatch(
        cor.test(age, value, method = "pearson"),
        error = function(e) list(estimate = NA, p.value = NA)
      )
      method_used <- "Pearson"
    } else {
      # 处理Spearman相关中的重复值问题
      ct <- tryCatch(
        cor.test(age, value, method = "spearman", exact = FALSE),
        error = function(e) list(estimate = NA, p.value = NA)
      )
      method_used <- "Spearman"
    }
    
    return(list(
      estimate = ifelse(is.null(ct$estimate), NA, ct$estimate),
      p.value = ifelse(is.null(ct$p.value), NA, ct$p.value),
      method = method_used
    ))
  }
  
  # 计算相关统计量
  cor_data <- long_data %>%
    group_by(metric, sex) %>%
    filter(!is.na(age) & !is.na(value)) %>%
    filter(n() >= 3) %>%  # 确保足够样本量
    summarise(
      cor_result = list(calculate_correlation(age, value)),
      .groups = "drop"
    ) %>%
    mutate(
      cor = map_dbl(cor_result, "estimate"),
      p_value = map_dbl(cor_result, "p.value"),
      method = map_chr(cor_result, "method")
    ) %>%
    mutate(
      color_label = case_when(
        sex == "Total" ~ sprintf("<b style='color:%s'>R = %.2f, <i>P</i> %s</b>", 
                                 group_colors["Total"], 
                                 cor, 
                                 ifelse(p_value < 0.001, "< 0.001", 
                                        ifelse(is.na(p_value), "= N/A", sprintf("= %.3f", p_value)))),
        sex == "Male" ~ sprintf("<b style='color:%s'>R = %.2f, <i>P</i> %s</b>", 
                                group_colors["Male"], 
                                cor, 
                                ifelse(p_value < 0.001, "< 0.001", 
                                       ifelse(is.na(p_value), "= N/A", sprintf("= %.3f", p_value)))),
        sex == "Female" ~ sprintf("<b style='color:%s'>R = %.2f, <i>P</i> %s</b>", 
                                  group_colors["Female"], 
                                  cor, 
                                  ifelse(p_value < 0.001, "< 0.001", 
                                         ifelse(is.na(p_value), "= N/A", sprintf("= %.3f", p_value))))
      )
    ) %>%
    arrange(metric, match(sex, c("Total", "Male", "Female"))) %>%
    group_by(metric) %>%
    summarise(
      label = paste(color_label, collapse = "<br>"),
      .groups = "drop"
    )
  
  # 确保分面按指定顺序排列
  long_data$metric <- factor(long_data$metric, 
                             levels = new_names[facet_order],
                             ordered = TRUE)
  
  # 创建主绘图对象
  p <- ggplot(long_data, aes(x = age, y = value)) +
    geom_smooth(
      aes(color = sex, linetype = sex, fill = sex),
      method = "lm", 
      formula = y ~ x, 
      se = TRUE, 
      linewidth = 1.2,
      alpha = 0.2
    ) +
    geom_point(
      data = long_data %>% filter(sex != "Total"),
      aes(color = sex),
      size = 2
    ) +
    #统计标签
    ggtext::geom_richtext(
      data = cor_data,
      aes(label = label),
      x = -Inf, y = Inf,
      hjust = -0.08, vjust = 1.2,
      size = 7.92,   #放大字体
      fill = NA, 
      label.color = NA,
      lineheight = 1.6   #行间距
    ) +
    scale_color_manual(
      values = group_colors,
      breaks = names(group_colors),
      name = "Group",
      guide = guide_legend(override.aes = list(
        linetype = c("solid", "33", "33"),
        shape = NA,
        fill = alpha(group_colors, 0.2)
      ))
    ) +
    scale_linetype_manual(
      values = c("Total" = "solid", "Male" = "33", "Female" = "33"),
      breaks = names(group_colors),
      name = "Group"
    ) +
    scale_fill_manual(
      values = setNames(
        alpha(group_colors, 0.2),
        names(group_colors)
      ),
      breaks = names(group_colors),
      name = "Group"
    )  +
    facet_wrap(~ metric, scales = "free_y", nrow = 1) +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) + # 添加这行确保三位小数
    labs(
      x = "Age (years)",
      y = y_label
    ) +
    theme_bw() +
    theme(
      text = element_text(family = "Times New Roman", size = 23.04, face = "bold"),
      axis.title.x = element_text(face = "bold", size = 24, margin = margin(t = 15)),
      axis.title.y = element_text(face = "bold", size = 24, margin = margin(r = 15)),
      axis.text.x = element_text(size = 23.04),
      axis.text.y = element_text(size = 23.04),
      panel.grid.major = element_line(linewidth = 0.5, linetype = 'dashed', color = "gray80"),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(fill = NA, color = "black", linewidth = 1),
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 23.04),
      legend.text = element_text(size = 23.04),
      legend.box = "horizontal",
      legend.direction = "horizontal",
      legend.key = element_rect(fill = NA, color = NA),
      legend.key.width = unit(2.5, "cm"),
      legend.key.height = unit(0.6, "cm"),
      strip.background = element_rect(
        fill = "grey90", 
        color = "black", 
        linewidth = 1
      ),
      strip.text = element_text(
        face = "bold", 
        size = 23.04,
        margin = margin(t = 9, b = 9)
      ),
      panel.spacing = unit(0.5, "cm"),
      plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
    )
  return(p)
}

# ====== 尺寸计算和保存 ======
# 定义尺寸参数 (厘米)
panel_width_cm <- 14.4        # 单个分面宽度
panel_height_cm <- 14.4 * 4/3   # 单个分面高度 (8.267cm)
n_panels <- 3                  # 分面数量
panel_spacing_cm <- 1         # 分面间距

# 计算总尺寸 (厘米)
total_width_cm <- n_panels * panel_width_cm + (n_panels - 1) * panel_spacing_cm + 4
total_height_cm <- panel_height_cm + 4

# 转换为英寸 (1英寸 = 2.54厘米)
total_width_in <- total_width_cm / 2.54
total_height_in <- total_height_cm / 2.54

# 创建并保存FW图
create_correlation_plot(
  clean_data,
  metrics = c("FW_WM", "FW_PWMH", "FW_DWMH"),
  y_label = "FW fraction",
  facet_order = c("FW_WM", "FW_PWMH", "FW_DWMH")
) %>%
  ggsave("Age_FW_Correlation109last.png", ., 
         width = total_width_in, 
         height = total_height_in, 
         dpi = 300, 
         device = ragg::agg_png)

# 创建并保存PVSVF图
create_correlation_plot(
  clean_data,
  metrics = c("PVSVF_Total", "PVSVF_WM", "PVSVF_BG"),
  y_label = "PVS volume fraction",
  facet_order = c("PVSVF_Total", "PVSVF_WM", "PVSVF_BG")
) %>%
  ggsave("Age_PVSVF_Correlation109last.png", ., 
         width = total_width_in, 
         height = total_height_in, 
         dpi = 300, 
         device = ragg::agg_png)

# 创建并保存DTI-ALPS图
create_correlation_plot(
  clean_data,
  metrics = c("Ave_DTI_ALPS", "L_DTI_ALPS", "R_DTI_ALPS"),
  y_label = "DTI-ALPS index",
  facet_order = c("Ave_DTI_ALPS", "L_DTI_ALPS", "R_DTI_ALPS")
) %>%
  ggsave("Age_DTI_ALPS_Correlation109last.png", ., 
         width = total_width_in, 
         height = total_height_in, 
         dpi = 300, 
         device = ragg::agg_png)



