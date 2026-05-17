# ==========================================================================
# Firth惩罚逻辑回归分析脚本（修正版）
# 功能：对每个因变量（Lacune_grade, CMB_grade）与每个自变量（7个影像指标）
#       分别进行Firth逻辑回归，调整两个协变量模型，输出OR、CI、P值，
#       并按因变量+模型分别进行FDR校正（每个模型内7个检验）
# 数据路径：F:/PVS network& CSVD/20251022-112人-110人/统计/相关性分析表格/regresssion109.xlsx
# ==========================================================================

# 安装并加载必要的包
if (!require("logistf")) install.packages("logistf")
if (!require("readxl")) install.packages("readxl")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")
if (!require("writexl")) install.packages("writexl")

library(logistf)
library(readxl)
library(dplyr)
library(tidyr)
library(writexl)

# 1. 读取数据 ----------------------------------------------------------------
file_path <- "E:/PVS network& CSVD/20251022-112人-110人/统计/相关性分析表格/regresssion109_MD_FA_PSQI.xlsx"
data_raw <- read_excel(file_path)

# 2. 数据预处理 --------------------------------------------------------------
# 确保因变量为0/1数值，协变量二分类转换为因子（便于解释）
data_clean <- data_raw %>%
  mutate(
    Lacune_grade = as.numeric(Lacune_grade),
    CMB_grade = as.numeric(CMB_grade),
    sex = factor(sex, levels = c(1, 2)),          # 假设1=男，2=女，以1为参照
    ever_smoking = factor(ever_smoking, levels = c(0, 1)),
    ever_drinking = factor(ever_drinking, levels = c(0, 1)),
    exer = factor(exer, levels = c(0, 1)),
    diabetes2018 = factor(diabetes2018, levels = c(0, 1)),
    hyperlipoidemia2018 = factor(hyperlipoidemia2018, levels = c(0, 1)),
    hypertension2018 = factor(hypertension2018, levels = c(0, 1))
  )

# 3. 定义变量列表 ------------------------------------------------------------
indep_vars <- c("PVS_TotalVF", "PVS_WMVF", "PVS_BGVF10", 
                "FW_WMx100", "FW_PWMx100", "FW_DWMx100", "ALPSx10")
dep_vars <- c("Lacune_grade", "CMB_grade")
model1_cov <- c("age", "sex", "eduy")
model2_cov <- c("age", "sex", "eduy", "ever_smoking", "ever_drinking", 
                "exer", "diabetes2018", "hyperlipoidemia2018", "hypertension2018", 'BrainParenchyma', 'psqiall')
model_names <- c("Model1", "Model2")

# 4. 初始化结果数据框 ---------------------------------------------------------
results <- data.frame()

# 5. 循环拟合所有模型 ---------------------------------------------------------
for (dv in dep_vars) {
  for (iv in indep_vars) {
    for (i in 1:2) {
      cov_set <- if (i == 1) model1_cov else model2_cov
      model_label <- model_names[i]
      
      formula_str <- paste(dv, "~", iv, "+", paste(cov_set, collapse = "+"))
      formula <- as.formula(formula_str)
      
      mod <- tryCatch(
        logistf(formula, data = data_clean, pl = TRUE, alpha = 0.05, firth = TRUE),
        error = function(e) NULL
      )
      
      if (is.null(mod)) {
        results <- rbind(results, data.frame(
          Dependent = dv, Model = model_label, Independent = iv,
          OR = NA, CI_lower = NA, CI_upper = NA, P_value = NA,
          stringsAsFactors = FALSE
        ))
        next
      }
      
      coef_names <- names(coef(mod))
      iv_index <- which(coef_names == iv)
      if (length(iv_index) == 0) {
        results <- rbind(results, data.frame(
          Dependent = dv, Model = model_label, Independent = iv,
          OR = NA, CI_lower = NA, CI_upper = NA, P_value = NA,
          stringsAsFactors = FALSE
        ))
        next
      }
      
      coef_iv <- coef(mod)[iv_index]
      pval_iv <- mod$prob[iv_index]
      ci_iv <- confint(mod)[iv_index, ]  # 系数尺度的置信区间
      or_iv <- exp(coef_iv)
      ci_lower <- exp(ci_iv[1])
      ci_upper <- exp(ci_iv[2])
      
      results <- rbind(results, data.frame(
        Dependent = dv, Model = model_label, Independent = iv,
        OR = or_iv, CI_lower = ci_lower, CI_upper = ci_upper, P_value = pval_iv,
        stringsAsFactors = FALSE
      ))
    }
  }
}

# 6. FDR校正（按因变量和模型分组，每个模型内对7个自变量进行校正） --------------------
results <- results %>%
  group_by(Dependent, Model) %>%
  mutate(FDR = p.adjust(P_value, method = "fdr")) %>%
  ungroup()

# 7. 输出结果 ----------------------------------------------------------------
output_path <- "E:/PVS network& CSVD/20251022-112人-110人/统计/相关性分析表格/Firth_logistic_results_bvpsqi_20260502.xlsx"
write_xlsx(results, output_path)

# 打印前几行查看
print(head(results, 10))

# 可选：查看每个因变量+模型组合中FDR显著的结果
results %>%
  filter(FDR < 0.05) %>%
  arrange(Dependent, Model, FDR) %>%
  print()

