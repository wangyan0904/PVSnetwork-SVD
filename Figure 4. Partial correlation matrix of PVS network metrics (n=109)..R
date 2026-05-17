library(rstudioapi)
library(corrplot)
library(readr)
library(extrafont)
library(Cairo)
library(ggplot2)  # 用于绘制Q-Q图
library(ppcor)    # 用于偏相关分析

# 获取脚本路径并设置为当前路径
current_script_path <- getActiveDocumentContext()$path
current_dir <- dirname(current_script_path)
setwd(current_dir)

# 安装必要的包（如果未安装）
if (!requireNamespace("corrplot", quietly = TRUE)) {
  install.packages("corrplot")
}
if (!requireNamespace("readr", quietly = TRUE)) {
  install.packages("readr")
}
if (!requireNamespace("extrafont", quietly = TRUE)) {
  install.packages("extrafont")
}
if (!requireNamespace("Cairo", quietly = TRUE)) {
  install.packages("Cairo", repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
if (!requireNamespace("ppcor", quietly = TRUE)) {
  install.packages("ppcor")
}

# 确保Times New Roman字体可用
loadfonts()
if ("Times New Roman" %in% fonts()) {
  par(family = "Times New Roman")
} else {
  warning("Times New Roman字体未找到，将使用默认字体")
}

# 读取数据
df <- read_csv("age_sex_edu_part_hot109.csv")

# 检查数据中是否包含age和sex,eduy列
if (!all(c("age", "sex", "eduy") %in% colnames(df))) {
  cat("数据框中现有的列名：\n")
  print(colnames(df))
  stop("数据中必须包含age和sex，eduy列才能进行偏相关分析")
}

# 严格按照指定的变量顺序（只包括分析变量，不包括控制变量）
analysis_vars <- c("PVSVF-Total", "PVSVF-WM", "PVSVF-BG", "FW-WM", "FW-PWM", "FW-DWM",
                   "DTI-ALPS index")

# 检查数据列名是否匹配
if (!all(analysis_vars %in% colnames(df))) {
  stop("数据列名与预期变量不匹配，请检查数据文件")
}

# ----------------------新增：正态性检验----------------------
# 创建正态性检验结果数据框（只对分析变量进行检验）
norm_test_results <- data.frame(
  variable = analysis_vars,
  statistic = NA,
  p_value = NA,
  is_normal = NA,
  n_complete = NA,  # 记录非缺失样本量
  stringsAsFactors = FALSE
)

# 创建Q-Q图查看分布（保存到当前目录的Q-Q_plots文件夹）
if (!dir.exists("Q-Q_plots20260513")) {
  dir.create("Q-Q_plots20260513")
}

# 进行正态性检验并绘制Q-Q图（只对分析变量）
for (var in analysis_vars) {
  # 处理缺失值：仅删除该变量有缺失的行
  data_subset <- na.omit(df[[var]])
  
  # 记录有效样本量
  norm_test_results$n_complete[norm_test_results$variable == var] <- length(data_subset)
  
  # 进行Shapiro-Wilk正态性检验（样本量>3时）
  if (length(data_subset) > 3) {
    shapiro_result <- shapiro.test(data_subset)
    norm_test_results$statistic[norm_test_results$variable == var] <- shapiro_result$statistic
    norm_test_results$p_value[norm_test_results$variable == var] <- shapiro_result$p.value
    # 以p>0.05为正态分布判断标准
    norm_test_results$is_normal[norm_test_results$variable == var] <- shapiro_result$p.value > 0.05
  } else {
    norm_test_results$is_normal[norm_test_results$variable == var] <- FALSE
    warning(paste("变量", var, "有效样本量不足（n=", length(data_subset), "），无法进行正态性检验"))
  }
  
  # 绘制Q-Q图
  qq_plot <- ggplot(data.frame(value = data_subset), aes(sample = value)) +
    stat_qq() +
    stat_qq_line() +
    labs(title = paste("Q-Q Plot for", var, "(n=", length(data_subset), ")"),
         x = "理论分位数", y = "样本分位数") +
    theme_bw() +
    theme(text = element_text(family = "Times New Roman"))
  
  ggsave(file.path("Q-Q_plots20260513", paste0(var, "_QQplot20260513.png")), 
         plot = qq_plot, 
         width = 6, height = 4, dpi = 300)
}

# 打印正态性检验结果
cat("正态性检验结果：\n")
print(norm_test_results)

# ----------------------偏相关分析----------------------
# 初始化偏相关矩阵和p值矩阵（只包括分析变量）
pcor_matrix <- matrix(NA, nrow = length(analysis_vars), ncol = length(analysis_vars))
pcor_p_matrix <- matrix(NA, nrow = length(analysis_vars), ncol = length(analysis_vars))
pcor_method_matrix <- matrix("", nrow = length(analysis_vars), ncol = length(analysis_vars))
pcor_n_matrix <- matrix(NA, nrow = length(analysis_vars), ncol = length(analysis_vars))
pcor_error_matrix <- matrix("", nrow = length(analysis_vars), ncol = length(analysis_vars))
colnames(pcor_matrix) <- rownames(pcor_matrix) <- analysis_vars
colnames(pcor_p_matrix) <- rownames(pcor_p_matrix) <- analysis_vars
colnames(pcor_method_matrix) <- rownames(pcor_method_matrix) <- analysis_vars
colnames(pcor_n_matrix) <- rownames(pcor_n_matrix) <- analysis_vars
colnames(pcor_error_matrix) <- rownames(pcor_error_matrix) <- analysis_vars


# 计算偏相关性和p值（只在分析变量之间进行）
for (i in 1:(length(analysis_vars)-1)) {
  for (j in (i+1):length(analysis_vars)) {
    var1 <- analysis_vars[i]
    var2 <- analysis_vars[j]
    
    # 提取有效数据：包括两个分析变量和三个控制变量
    data_subset <- df[, c(var1, var2, "age", "sex", "eduy")]  # 修改：加入eduy
    data_subset <- na.omit(data_subset)
    current_n <- nrow(data_subset)
    
    # 记录当前分析的样本量
    pcor_n_matrix[i, j] <- pcor_n_matrix[j, i] <- current_n
    
    # 根据正态性检验结果选择偏相关方法
    var1_normal <- norm_test_results$is_normal[norm_test_results$variable == var1]
    var2_normal <- norm_test_results$is_normal[norm_test_results$variable == var2]
    
    # 判断方法：两个变量都正态用Pearson，否则用Spearman（秩转换）
    method <- ifelse(var1_normal && var2_normal, "pearson", "spearman")
    pcor_method_matrix[i, j] <- pcor_method_matrix[j, i] <- method
    
    # 计算偏相关性和p值（样本量>3时）
    if (current_n > 3) {
      # 检查变量方差，避免零方差或接近零方差
      var1_sd <- sd(data_subset[[var1]], na.rm = TRUE)
      var2_sd <- sd(data_subset[[var2]], na.rm = TRUE)
      age_sd <- sd(data_subset[["age"]], na.rm = TRUE)
      eduy_sd <- sd(data_subset[["eduy"]], na.rm = TRUE)  # 新增：检查eduy方差
      
      # 如果任何变量的标准差接近零，跳过计算
      if (var1_sd < 1e-10 || var2_sd < 1e-10 || age_sd < 1e-10 || eduy_sd < 1e-10) {
        warning(paste("变量", var1, "或", var2, "或age或eduy的标准差接近零，跳过计算"))
        pcor_error_matrix[i, j] <- pcor_error_matrix[j, i] <- "零方差"
        next
      }
      
      tryCatch({
        if (method == "pearson") {
          # Pearson偏相关 - 使用原始数据
          pcor_result <- pcor.test(data_subset[[var1]], 
                                   data_subset[[var2]], 
                                   data_subset[, c("age", "sex", "eduy")],  # 修改：加入eduy
                                   method = "pearson")
        } else {
          # Spearman偏相关 - 对连续变量进行秩转换
          temp_data <- data_subset
          
          # 对连续变量进行秩转换（不包括分类变量sex）
          temp_data[[var1]] <- rank(temp_data[[var1]], na.last = "keep")
          temp_data[[var2]] <- rank(temp_data[[var2]], na.last = "keep")
          temp_data[["age"]] <- rank(temp_data[["age"]], na.last = "keep")
          temp_data[["eduy"]] <- rank(temp_data[["eduy"]], na.last = "keep")  # 新增：eduy秩转换
          # sex是分类变量，不需要秩转换
          
          # 使用秩转换后的数据进行Pearson偏相关（即Spearman偏相关）
          pcor_result <- pcor.test(temp_data[[var1]], 
                                   temp_data[[var2]], 
                                   temp_data[, c("age", "sex", "eduy")],  # 修改：加入eduy
                                   method = "pearson")
        }
        
        pcor_matrix[i, j] <- pcor_matrix[j, i] <- pcor_result$estimate
        pcor_p_matrix[i, j] <- pcor_p_matrix[j, i] <- pcor_result$p.value
        
      }, error = function(e) {
        warning(paste("计算", var1, "与", var2, "的偏相关时出错:", e$message))
        pcor_error_matrix[i, j] <- pcor_error_matrix[j, i] <- e$message
        
        # 尝试使用简单相关作为备选
        try({
          if (method == "pearson") {
            cor_result <- cor.test(data_subset[[var1]], data_subset[[var2]], 
                                   method = "pearson")
          } else {
            cor_result <- cor.test(data_subset[[var1]], data_subset[[var2]], 
                                   method = "spearman", exact = FALSE)
          }
          pcor_matrix[i, j] <- pcor_matrix[j, i] <- cor_result$estimate
          pcor_p_matrix[i, j] <- pcor_p_matrix[j, i] <- cor_result$p.value
          pcor_error_matrix[i, j] <- pcor_error_matrix[j, i] <- paste("使用简单相关替代:", e$message)
        }, silent = TRUE)
      })
      
    } else {
      warning(paste(var1, "与", var2, "的有效样本量不足（n=", current_n, "），无法计算偏相关性"))
      pcor_error_matrix[i, j] <- pcor_error_matrix[j, i] <- "样本量不足"
    }
  }
}


# 对角线设置
diag(pcor_matrix) <- 1.00
diag(pcor_p_matrix) <- 0
diag(pcor_method_matrix) <- "-"
diag(pcor_n_matrix) <- norm_test_results$n_complete
diag(pcor_error_matrix) <- "-"

# 保存偏相关分析信息矩阵
write.csv(pcor_method_matrix, "partial_correlation_methods20260513.csv", row.names = TRUE)
write.csv(pcor_n_matrix, "partial_correlation_sample_sizes20260513.csv", row.names = TRUE)
write.csv(pcor_error_matrix, "partial_correlation_errors20260513.csv", row.names = TRUE)

# 输出偏相关方法使用情况汇总
method_summary <- table(pcor_method_matrix[lower.tri(pcor_method_matrix)])
cat("偏相关方法使用情况汇总：\n")
print(method_summary)

# 输出错误情况汇总
error_summary <- table(pcor_error_matrix[lower.tri(pcor_error_matrix)])
if (length(error_summary) > 0) {
  cat("偏相关计算错误情况汇总：\n")
  print(error_summary)
}

# ----------------------绘图函数----------------------
# 创建颜色梯度
col <- colorRampPalette(c("black", "darkblue", "white", "red", "darkred"))(400)

draw_partial_corrplot <- function() {
  par(lwd = 0.5)  # 设置全局线条宽度
  # 同时调整内边距和外侧边距
  par(mar = c(5, 4, 8, 6) + 0.1,  # 内边距：右侧增加到6，为颜色条标题留空间（原4改为6）
      oma = c(0, 0, 0, 2))        # 外侧边距：右侧增加2
  
  # 绘制下三角的数字矩阵
  corrplot(
    pcor_matrix,
    method = "number",
    type = "lower",
    family = "Times New Roman",
    col = col,
    tl.pos = "lt",
    tl.col = "black",
    tl.cex = 0.8,
    tl.srt = 75,
    number.font = 2,
    number.cex = 1.0,
    font = 2,
    cl.cex = 0.8,
    cl.ratio = 0.1,
    addgrid.col = "gray50",
    lwd = 0.5,
    mar = c(2, 2, 4, 4)  # 增加上边距
  )
  
  # 绘制上三角的圆圈和显著性标记
  corrplot(
    pcor_matrix, 
    method = 'circle',
    type = 'upper',
    col = col,
    add = TRUE,
    tl.pos = "n",
    cl.pos = "n",  # 颜色条已在第一次调用时绘制，此处不重复
    diag = FALSE,
    p.mat = pcor_p_matrix,
    sig.level = c(0.001, 0.01, 0.05),
    pch.cex = 1.8,
    insig = 'label_sig',
    pch.col = "white",
    font = 2,
    addgrid.col = "gray50",
    lwd = 0.5
  )
  
  # ================== MODIFICATION START (自动定位版) ==================
  # 为水平颜色条添加标题和单位，自动定位到颜色条左端（-1端）
  par(xpd = TRUE)
  
  # 获取当前图形区域的规范化设备坐标 (NDC: 0-1)
  plt <- par("plt")  # c(x1, x2, y1, y2) 图形区域在设备中的比例
  usr <- par("usr")  # 用户坐标范围 c(xmin, xmax, ymin, ymax)
  
  # 颜色条假设为水平，位于图形下方。通常 corrplot 水平颜色条的：
  # - 左边界与图形左边界对齐（plt[1]）
  # - 右边界为 plt[2]
  # - 垂直位置：位于图形底部外侧，需要估算。常用布局是颜色条中心位于 plt[3] - 某个偏移
  # 简便方法：直接取颜色条中心位于设备坐标 y = plt[3] - 0.05（略低于图形底部）
  # 更精确：我们可以通过 cl.ratio 和 cl.offset 计算，但为通用性，以下采用固定偏移的自动转换
  
  # 1. 颜色条左端: 设备坐标中 x = plt[1]（与图形左边界对齐）
  #    转换为用户坐标
  colorbar_left_x <- grconvertX(plt[1], "ndc", "user")
  
  # 2. 颜色条垂直中心: 设备坐标中 y = plt[3] - 0.06（假设颜色条高度为 0.08，中心在底部向下 0.04处）
  #    这个偏移量可根据实际微调，但以下是自动计算的方法：
  #    获取当前颜色条的高度（通过 cl.ratio 等估算困难），这里采用一个普适偏移。
  #    经过测试，对于默认参数，颜色条中心约在 plt[3] - 0.04 处。
  colorbar_center_y_ndc <- plt[3] - 0.04   # 0.04可微调，但下面是自动方法
  # 转换为用户坐标
  target_y_mid <- grconvertY(colorbar_center_y_ndc, "ndc", "user")
  
  # 添加主标题（右对齐）
  text(x = colorbar_left_x + 0.09 * (usr[4] - usr[3]),
       y = target_y_mid + 0.01 * (usr[4] - usr[3]),  # 稍上方
       labels = "Partial correlation",
       srt = 0,
       adj = 1,    # 右对齐
       cex = 0.75,
       family = "Times New Roman",
       font = 1)  
  
  text(x = colorbar_left_x + 0.09 * (usr[4] - usr[3]),
       y = target_y_mid - 0.02 * (usr[4] - usr[3]),
       labels = "coefficient (r)",
       srt = 0,
       adj = 1,
       cex = 0.75,
       family = "Times New Roman",
       font = 1)
  # ================== MODIFICATION END ==================
}

# 在R中显示偏相关性热图
draw_partial_corrplot()

# 保存偏相关性热图
CairoPNG("partial_correlation_plot_PVS20260513.png", width = 2000, height = 2000, dpi = 300)
draw_partial_corrplot()
dev.off()

# 保存偏相关矩阵和p值矩阵
write.csv(pcor_matrix, "partial_correlation_matrix20260513.csv", row.names = TRUE)
write.csv(pcor_p_matrix, "partial_correlation_pvalues20260513.csv", row.names = TRUE)

cat("偏相关分析完成！\n")
cat("结果文件已保存：\n")
cat("- partial_correlation_plot_PVS20260513.png: 偏相关热图\n")
cat("- partial_correlation_matrix20260513.csv: 偏相关系数矩阵\n")
cat("- partial_correlation_pvalues20260513.csv: 偏相关p值矩阵\n")
cat("- partial_correlation_methods20260513.csv: 偏相关方法矩阵\n")
cat("- partial_correlation_sample_sizes20260513.csv: 样本量矩阵\n")
cat("- partial_correlation_errors20260513.csv: 错误信息矩阵\n")

