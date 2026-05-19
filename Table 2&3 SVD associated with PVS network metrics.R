# 加载必要的包
library(readxl)
library(openxlsx)
library(dplyr)
library(broom)

# 尝试加载openxlsx包，如果未安装则安装
if (!require(openxlsx)) {
  install.packages("openxlsx")
  library(openxlsx)
}

# 设置工作路径（请根据实际情况修改）
setwd("E:/PVS network& CSVD/20251022-112人-110人/统计/相关性分析表格/")  # 修改为您的实际工作路径

# 读取数据
data <- read_excel("regresssion109_MD_FA_PSQI.xlsx")  # 修改后的文件名

# 定义变量组
independent_vars <- c('PVS_TotalVF', 'PVS_WMVF', 'PVS_BGVF10', 'FW_WMx100', 
                      'FW_PWMx100', 'FW_DWMx100', 'ALPSx10')

continuous_dependent_vars <- c('LG10_WMH_Total_1', 'LG10_PWMH_1', 'LG10_DWMH_1')
binary_dependent_vars <- c('Lacune_grade', 'CMB_grade')

# 定义协变量组（加入 psqiall）
model1_covariates <- c('age', 'sex', 'eduy')
model2_covariates <- c('age', 'sex', 'eduy',  'BrainParenchyma', 'psqiall', 'ever_smoking', 'ever_drinking', 
                       'exer', 'diabetes2018', 'hyperlipoidemia2018', 'hypertension2018')

# 数据检查
cat("数据基本信息:\n")
cat("总样本量:", nrow(data), "\n")
cat("变量数量:", ncol(data), "\n\n")

cat("关键变量缺失情况:\n")
all_vars <- c(independent_vars, continuous_dependent_vars, binary_dependent_vars, 
              model1_covariates, model2_covariates)
for (var in all_vars) {
  if (var %in% names(data)) {
    missing_count <- sum(is.na(data[[var]]))
    missing_pct <- round(missing_count / nrow(data) * 100, 1)
    cat(var, ": 缺失", missing_count, "/", nrow(data), "(", missing_pct, "%)\n")
  } else {
    cat(var, ": 变量不存在\n")
  }
}

# 运行多元线性回归函数
run_linear_regression <- function(data, y_var, x_vars, covariates, model_name) {
  results <- data.frame()
  
  for (x_var in x_vars) {
    # 检查变量是否存在
    if (!(x_var %in% names(data)) | !(y_var %in% names(data))) next
    
    # 选择有效的协变量
    valid_covariates <- covariates[covariates %in% names(data)]
    
    # 构建公式
    formula_str <- paste0(y_var, " ~ ", x_var)
    if (length(valid_covariates) > 0) {
      formula_str <- paste0(formula_str, " + ", paste(valid_covariates, collapse = " + "))
    }
    
    # 去除缺失值
    model_vars <- c(y_var, x_var, valid_covariates)
    temp_data <- data[complete.cases(data[model_vars]), ]
    
    if (nrow(temp_data) < 10) next  # 确保有足够样本量
    
    tryCatch({
      # 拟合线性模型
      model <- lm(as.formula(formula_str), data = temp_data)
      
      # 提取结果
      model_summary <- summary(model)
      conf_int <- confint(model)
      
      # 获取特定自变量的结果
      if (x_var %in% rownames(model_summary$coefficients)) {
        coef_row <- model_summary$coefficients[x_var, ]
        conf_row <- conf_int[x_var, ]
        
        result_row <- data.frame(
          Model = model_name,
          Dependent_Variable = y_var,
          Independent_Variable = x_var,
          Beta = coef_row[1],
          CI_lower = conf_row[1],
          CI_upper = conf_row[2],
          P_value = coef_row[4],
          N = nrow(temp_data),
          stringsAsFactors = FALSE
        )
        results <- rbind(results, result_row)
      }
    }, error = function(e) {
      cat("Error in", model_name, "for", y_var, "~", x_var, ":", e$message, "\n")
    })
  }
  return(results)
}

# 运行二元logistic回归函数
run_logistic_regression <- function(data, y_var, x_vars, covariates, model_name) {
  results <- data.frame()
  
  for (x_var in x_vars) {
    # 检查变量是否存在
    if (!(x_var %in% names(data)) | !(y_var %in% names(data))) next
    
    # 选择有效的协变量
    valid_covariates <- covariates[covariates %in% names(data)]
    
    # 构建公式
    formula_str <- paste0(y_var, " ~ ", x_var)
    if (length(valid_covariates) > 0) {
      formula_str <- paste0(formula_str, " + ", paste(valid_covariates, collapse = " + "))
    }
    
    # 去除缺失值
    model_vars <- c(y_var, x_var, valid_covariates)
    temp_data <- data[complete.cases(data[model_vars]), ]
    
    if (nrow(temp_data) < 10) next
    
    # 检查因变量是否有两个类别
    if (length(unique(temp_data[[y_var]])) < 2) next
    
    tryCatch({
      # 拟合logistic模型
      model <- glm(as.formula(formula_str), family = binomial(link = "logit"), data = temp_data)
      
      # 提取结果
      model_summary <- summary(model)
      conf_int <- confint(model)
      
      # 获取特定自变量的结果
      if (x_var %in% rownames(model_summary$coefficients)) {
        coef_row <- model_summary$coefficients[x_var, ]
        conf_row <- conf_int[x_var, ]
        
        # 计算OR值
        OR <- exp(coef_row[1])
        OR_CI_lower <- exp(conf_row[1])
        OR_CI_upper <- exp(conf_row[2])
        
        result_row <- data.frame(
          Model = model_name,
          Dependent_Variable = y_var,
          Independent_Variable = x_var,
          OR = OR,
          CI_lower = OR_CI_lower,
          CI_upper = OR_CI_upper,
          P_value = coef_row[4],
          N = nrow(temp_data),
          stringsAsFactors = FALSE
        )
        results <- rbind(results, result_row)
      }
    }, error = function(e) {
      cat("Error in", model_name, "for", y_var, "~", x_var, ":", e$message, "\n")
    })
  }
  return(results)
}

# 主分析函数 - 修复rbind问题
main_analysis <- function(data) {
  linear_results <- data.frame()
  logistic_results <- data.frame()
  
  # 对连续因变量进行分析（多元线性回归）
  for (dep_var in continuous_dependent_vars) {
    if (dep_var %in% names(data)) {
      cat("分析连续变量:", dep_var, "\n")
      
      # Model 1
      results_model1 <- run_linear_regression(data, dep_var, independent_vars, 
                                              model1_covariates, "Model1")
      if (nrow(results_model1) > 0) {
        linear_results <- rbind(linear_results, results_model1)
      }
      
      # Model 2
      results_model2 <- run_linear_regression(data, dep_var, independent_vars, 
                                              model2_covariates, "Model2")
      if (nrow(results_model2) > 0) {
        linear_results <- rbind(linear_results, results_model2)
      }
    }
  }
  
  # 对二分类因变量进行分析（二元logistic回归）
  for (dep_var in binary_dependent_vars) {
    if (dep_var %in% names(data)) {
      cat("分析二分类变量:", dep_var, "\n")
      
      # Model 1
      results_model1 <- run_logistic_regression(data, dep_var, independent_vars, 
                                                model1_covariates, "Model1")
      if (nrow(results_model1) > 0) {
        logistic_results <- rbind(logistic_results, results_model1)
      }
      
      # Model 2
      results_model2 <- run_logistic_regression(data, dep_var, independent_vars, 
                                                model2_covariates, "Model2")
      if (nrow(results_model2) > 0) {
        logistic_results <- rbind(logistic_results, results_model2)
      }
    }
  }
  
  return(list(linear = linear_results, logistic = logistic_results))
}

# 执行分析
cat("开始数据分析...\n")
results_list <- main_analysis(data)

# 处理线性回归结果
if (nrow(results_list$linear) > 0) {
  # 格式化结果
  results_list$linear$Beta_95CI <- sprintf("%.4f (%.4f - %.4f)", 
                                           results_list$linear$Beta, 
                                           results_list$linear$CI_lower, 
                                           results_list$linear$CI_upper)
  
  # 重新排列列的顺序
  linear_final <- results_list$linear[, c("Model", "Dependent_Variable", "Independent_Variable", 
                                          "Beta", "CI_lower", "CI_upper", "Beta_95CI", "P_value", "N")]
  
  # === 新增FDR校正 ===
  # P_FDR1: 按Model和Dependent_Variable分组，对每组内的P_value进行FDR校正
  linear_final <- linear_final %>%
    group_by(Model, Dependent_Variable) %>%
    mutate(P_FDR1 = p.adjust(P_value, method = "fdr")) %>%
    ungroup()
  
  # P_FDR2: 按Model分组，对每组内所有P_value进行FDR校正
  linear_final <- linear_final %>%
    group_by(Model) %>%
    mutate(P_FDR2 = p.adjust(P_value, method = "fdr")) %>%
    ungroup()
  
  # 调整列顺序，将新列放在Beta_95CI之后，P_value之前（可根据需要调整）
  linear_final <- linear_final[, c("Model", "Dependent_Variable", "Independent_Variable", 
                                   "Beta", "CI_lower", "CI_upper", "Beta_95CI", 
                                   "P_FDR1", "P_FDR2", "P_value", "N")]
} else {
  linear_final <- data.frame()
  cat("没有线性回归结果\n")
}

# 处理logistic回归结果
if (nrow(results_list$logistic) > 0) {
  # 格式化结果
  results_list$logistic$OR_95CI <- sprintf("%.4f (%.4f - %.4f)", 
                                           results_list$logistic$OR, 
                                           results_list$logistic$CI_lower, 
                                           results_list$logistic$CI_upper)
  
  # 重新排列列的顺序
  logistic_final <- results_list$logistic[, c("Model", "Dependent_Variable", "Independent_Variable", 
                                              "OR", "CI_lower", "CI_upper", "OR_95CI", "P_value", "N")]
  
  # === 新增FDR校正 ===
  # P_FDR3: 按Model和Dependent_Variable分组，对每组内的P_value进行FDR校正
  logistic_final <- logistic_final %>%
    group_by(Model, Dependent_Variable) %>%
    mutate(P_FDR3 = p.adjust(P_value, method = "fdr")) %>%
    ungroup()
  
  # P_FDR4: 按Model分组，对每组内所有P_value进行FDR校正
  logistic_final <- logistic_final %>%
    group_by(Model) %>%
    mutate(P_FDR4 = p.adjust(P_value, method = "fdr")) %>%
    ungroup()
  
  # 调整列顺序
  logistic_final <- logistic_final[, c("Model", "Dependent_Variable", "Independent_Variable", 
                                       "OR", "CI_lower", "CI_upper", "OR_95CI", 
                                       "P_FDR3", "P_FDR4", "P_value", "N")]
} else {
  logistic_final <- data.frame()
  cat("没有logistic回归结果\n")
}

# 保存结果
if (nrow(linear_final) > 0 | nrow(logistic_final) > 0) {
  # 创建结果列表
  result_sheets <- list()
  
  if (nrow(linear_final) > 0) {
    result_sheets[["线性回归结果"]] <- linear_final
    cat("线性回归结果:\n")
    print(linear_final)
  }
  
  if (nrow(logistic_final) > 0) {
    result_sheets[["Logistic回归结果"]] <- logistic_final
    cat("\nLogistic回归结果:\n")
    print(logistic_final)
  }
  
  # 保存到Excel
  if (require(openxlsx)) {
    write.xlsx(result_sheets, "correlation_analysis_results_M2_bvpsqi.xlsx")
    cat("\n分析完成！结果已保存至: correlation_analysis_results_M2_bvpsqi.xlsx\n")
  } else {
    # 如果openxlsx不可用，使用CSV格式
    if (nrow(linear_final) > 0) {
      write.csv(linear_final, "linear_regression_results.csv", row.names = FALSE)
    }
    if (nrow(logistic_final) > 0) {
      write.csv(logistic_final, "logistic_regression_results.csv", row.names = FALSE)
    }
    cat("\n分析完成！结果已保存至CSV文件\n")
  }
} else {
  cat("没有生成有效的结果，请检查数据格式和变量名称。\n")
}

# 创建总结表格（基于原始P_value，保持不变）
if (nrow(linear_final) > 0 | nrow(logistic_final) > 0) {
  summary_list <- list()
  
  if (nrow(linear_final) > 0) {
    linear_summary <- linear_final %>%
      group_by(Dependent_Variable, Model) %>%
      summarise(
        N_analyses = n(),
        Significant_p0.05 = sum(P_value < 0.05, na.rm = TRUE),
        Significant_p0.01 = sum(P_value < 0.01, na.rm = TRUE),
        .groups = 'drop'
      )
    summary_list[["线性回归总结"]] <- as.data.frame(linear_summary)
  }
  
  if (nrow(logistic_final) > 0) {
    logistic_summary <- logistic_final %>%
      group_by(Dependent_Variable, Model) %>%
      summarise(
        N_analyses = n(),
        Significant_p0.05 = sum(P_value < 0.05, na.rm = TRUE),
        Significant_p0.01 = sum(P_value < 0.01, na.rm = TRUE),
        .groups = 'drop'
      )
    summary_list[["Logistic回归总结"]] <- as.data.frame(logistic_summary)
  }
  
  # 保存总结表格
  if (require(openxlsx)) {
    write.xlsx(summary_list, "analysis_summary_M2_bvpsqi.xlsx")
    cat("分析总结已保存至: analysis_summary_M2_bvpsqi.xlsx\n")
  } else {
    if (length(summary_list) > 0) {
      for (name in names(summary_list)) {
        write.csv(summary_list[[name]], paste0("summary_", gsub(" ", "_", name), ".csv"), row.names = FALSE)
      }
      cat("分析总结已保存至CSV文件\n")
    }
  }
}

# 如果没有安装必要的包，提供安装代码
missing_packages <- c()
if (!require(readxl)) missing_packages <- c(missing_packages, "readxl")
if (!require(dplyr)) missing_packages <- c(missing_packages, "dplyr")
if (!require(broom)) missing_packages <- c(missing_packages, "broom")
if (!require(openxlsx)) missing_packages <- c(missing_packages, "openxlsx")

if (length(missing_packages) > 0) {
  cat("\n需要安装以下R包:\n")
  cat("install.packages(c('", paste(missing_packages, collapse = "', '"), "'))\n", sep = "")
}

