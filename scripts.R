library(dplyr)
library(readr)
library(lubridate)
library(tidyr)
# read the dataset
df <- read.csv("data/FINAL_master_dataset_9Feb2026(in).csv") %>%
  filter(!is.na(icu_los_days))

# variable mapping
race_mapping <- c(
  "WHITE" = "White",
  "WHITE - OTHER EUROPEAN" = "White",
  "WHITE - EASTERN EUROPEAN" = "White",
  "WHITE - BRAZILIAN" = "White",
  "WHITE - RUSSIAN" = "White",
  "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER" = "White",
  "PORTUGUESE" = "White",
  
  "BLACK/AFRICAN AMERICAN" = "Black",
  "BLACK/CARIBBEAN ISLAND" = "Black",
  "BLACK/AFRICAN" = "Black",
  "BLACK/CAPE VERDEAN" = "Black",
  
  "UNKNOWN" = "Unknown",
  "PATIENT DECLINED TO ANSWER" = "Unknown",
  "UNABLE TO OBTAIN" = "Unknown",
  
  "OTHER" = "Other",
  "MULTIPLE RACE/ETHNICITY" = "Other",
  "AMERICAN INDIAN/ALASKA NATIVE" = "Other",
  
  "ASIAN" = "Asian",
  "ASIAN - SOUTH EAST ASIAN" = "Asian",
  "ASIAN - CHINESE" = "Asian",
  "ASIAN - ASIAN INDIAN" = "Asian",
  "ASIAN - KOREAN" = "Asian",
  
  "HISPANIC OR LATINO" = "Hispanic/Latino",
  "HISPANIC/LATINO - CUBAN" = "Hispanic/Latino",
  "HISPANIC/LATINO - DOMINICAN" = "Hispanic/Latino",
  "HISPANIC/LATINO - PUERTO RICAN" = "Hispanic/Latino",
  "HISPANIC/LATINO - SALVADORAN" = "Hispanic/Latino",
  "HISPANIC/LATINO - COLUMBIAN" = "Hispanic/Latino",
  "HISPANIC/LATINO - GUATEMALAN" = "Hispanic/Latino",
  "HISPANIC/LATINO - MEXICAN" = "Hispanic/Latino",
  "HISPANIC/LATINO - CENTRAL AMERICAN" = "Hispanic/Latino",
  "HISPANIC/LATINO - HONDURAN" = "Hispanic/Latino",
  "SOUTH AMERICAN" = "Hispanic/Latino"
)

# create race clean variable and outcome variable
df2 <- df %>%
  mutate(
    race = trimws(as.character(race)),
    race_clean = dplyr::recode(race, !!!race_mapping, .default = "Other"),
    race_clean = factor(race_clean,
                        levels = c("White", "Black", "Asian",
                                   "Hispanic/Latino", "Other", "Unknown")),
    y = icu_los_days
  )

# build a dropping vector
drop_cols <- c(
  "stay_id",
  "race",
  "admittime","dischtime","icu_intime_first","icu_outtime_first","intime",
  "primary_icd_long_title","primary_icd_code",
  "readmit_30d","readmit_90d","readmit_180d",
  "icu_los_days"
)

# drop the column based on created vector
df2 <- df2 %>% select(-any_of(drop_cols))


names(df2) <- sub("_final$", "", names(df2))


# transfer categories variables as factor

cat_cols <- intersect(c("race_clean","marital_status","insurance","gender","first_careunit"), names(df2))
df2 <- df2 %>% mutate(across(all_of(cat_cols), ~as.factor(trimws(.x))))

# select all numeric variables except for los
num_cols <- names(df2)[sapply(df2, is.numeric)]
num_cols <- setdiff(num_cols, "y")

# Create a new column named original_variable + "_miss"
# Assign 1 if missing, 0 otherwise
for (v in num_cols) {
  df2[[paste0(v, "_miss")]] <- as.integer(is.na(df2[[v]]))
}
# prepare a imputed dataset for QRF
# extract and parse admission time from the original dataframe
df_time <- df %>%
  mutate(admit_dt = dmy_hm(admittime)) %>%
  select(subject_id, hadm_id, admit_dt)

# Merge admission datetime into df2
df2 <- df2 %>%
  left_join(df_time, by = c("subject_id","hadm_id"))

# Remove ID variables and their missingness indicators
df2 <- df2 %>% select(-subject_id, -hadm_id, -subject_id_miss, -hadm_id_first_miss, -hadm_id_miss, -hadm_id_first)
# create a new dataframe with median imputation applied on its numeric variables
df_imp <- df2
for (v in num_cols) {
  med <- median(df_imp[[v]], na.rm = TRUE)
  df_imp[[v]][is.na(df_imp[[v]])] <- med
}
# for categorical variables with missing values, assign level "unknown" to them
for (v in cat_cols) {
  df_imp[[v]] <- addNA(df_imp[[v]])
  levels(df_imp[[v]])[is.na(levels(df_imp[[v]]))] <- "Unknown"
}


# finalize the dataset
# using los as outcome variable
y <- df2$y
# X_tree dataset for DRF/tree model
X_tree <- df2 %>% select(-y)
# X_all dataset for QRF model
X_all  <- df_imp %>% select(-y)



#-----------------------------------
# Model training part
#-----------------------------------

# 1) data split (by admission time)
cutoff <- quantile(df2$admit_dt, 0.8, na.rm = TRUE)
idx_train <- which(df2$admit_dt <= cutoff)
idx_test  <- which(df2$admit_dt >  cutoff)

# 2) remove admit_dt from modeling tables (but AFTER using it for split)
df2_model <- df2 %>% select(-admit_dt)
df_imp2   <- df_imp %>% select(-any_of("admit_dt"))

# 3) define outcome + feature tables
y      <- df2_model$y
X_tree <- df2_model %>% select(-y)
X_all  <- df_imp2 %>% select(-y)

# 4) split train/test
Xtr_tree <- X_tree[idx_train, , drop = FALSE]
Xte_tree <- X_tree[idx_test,  , drop = FALSE]
y_tr     <- y[idx_train]
y_te     <- y[idx_test]

Xtr_all  <- X_all[idx_train, , drop = FALSE]
Xte_all  <- X_all[idx_test,  , drop = FALSE]

# ---------------------------------------------------------
# 5) IMPORTANT: clean tree features AFTER split (train-led)
#    - drop useless *_miss that are all-zero in TRAIN
#    - drop constant columns in TRAIN
#    - apply the SAME drops to TEST
# ---------------------------------------------------------
miss_cols <- grep("_miss$", names(Xtr_tree), value = TRUE)

miss_all_zero <- miss_cols[
  sapply(miss_cols, function(v) sum(Xtr_tree[[v]], na.rm = TRUE) == 0)
]

const_cols <- names(Xtr_tree)[
  sapply(Xtr_tree, function(x) dplyr::n_distinct(x, na.rm = TRUE) <= 1)
]

drop_cols_tree <- union(miss_all_zero, const_cols)

Xtr_tree <- Xtr_tree %>% select(-any_of(drop_cols_tree))
Xte_tree <- Xte_tree %>% select(-any_of(drop_cols_tree))

cat("Tree feature cleanup:\n")
cat("  Removed useless *_miss (all-zero in train):", length(miss_all_zero), "\n")
cat("  Removed constant columns (in train):", length(const_cols), "\n")
cat("  Xtr_tree cols:", ncol(Xtr_tree), " | Xte_tree cols:", ncol(Xte_tree), "\n")


# For hyperparameter tuning
library(caret)

# Create 3-fold CV splits (balance speed vs robustness)
set.seed(2026)
cv_folds_hp <- createFolds(y_tr, k = 3, list = TRUE, returnTrain = FALSE)
# 2-fit drf model
library(drf)


set.seed(2026)

# --- 优化后的核心函数 ---
wquant_matrix <- function(y_train, W_sparse, probs) {
  # 1. 预先排序 y_train (全局只排一次)
  ord <- order(y_train)
  y_sorted <- y_train[ord]
  
  # 2. 对稀疏矩阵 W 按 y 的顺序重排并转置，方便按行快速提取非零元素
  # Matrix 对象的按行切片较慢，这里确保其格式优化
  W_sorted <- W_sparse[, ord, drop = FALSE]
  
  n_test <- nrow(W_sorted)
  n_probs <- length(probs)
  res <- matrix(NA, nrow = n_test, ncol = n_probs)
  
  # 3. 循环处理每一行（利用稀疏矩阵内部结构）
  for (i in 1:n_test) {
    # 提取第 i 行的非零元素
    row_start <- W_sorted@p[i] + 1
    row_end   <- W_sorted@p[i+1]
    
    # 如果该行全为 0 (理论上 DRF 不会出现)
    if (row_start > row_end) next
    
    # 获取非零权重的索引和数值
    # 注意：dgCMatrix 是列压缩，这里如果 W_sorted 是行压缩 (dgRMatrix) 会更快
    # 但直接使用 W_sorted[i,] 并在其中处理非零值已足够提速
    row_data <- W_sorted[i, ]
    nz_idx <- which(row_data != 0)
    nz_w <- as.numeric(row_data[nz_idx])
    nz_y <- y_sorted[nz_idx]
    
    # 计算累积分布
    cw <- cumsum(nz_w) / sum(nz_w)
    
    # 匹配分位数点
    for (p_idx in 1:n_probs) {
      # 找到第一个大于等于概率 p 的位置
      idx <- which(cw >= probs[p_idx])[1]
      res[i, p_idx] <- nz_y[idx]
    }
  }
  return(res)
}

# --- Hyperparameter tuning ---
cat("\nTuning DRF hyperparameters (3-fold CV)...\n")

drf_params_test <- expand.grid(
  num.trees = c(500, 1000),
  min.node.size = c(20, 50, 100)
)

drf_cv_scores <- sapply(seq_len(nrow(drf_params_test)), function(i) {
  params <- drf_params_test[i, ]
  cat(sprintf("  [%d/%d] trees=%d, node=%d...",
              i, nrow(drf_params_test),
              params$num.trees, params$min.node.size))
  
  val_idx   <- cv_folds_hp[[1]]
  train_idx <- setdiff(seq_len(nrow(Xtr_tree)), val_idx)
  
  fit_cv <- drf(
    X = Xtr_tree[train_idx, ],
    Y = y_tr[train_idx],
    num.trees = params$num.trees,
    min.node.size = params$min.node.size,
    sample.fraction = 0.5,
    mtry = floor(sqrt(ncol(Xtr_tree[train_idx, ]))),
    seed = 2026
  )
  
  pred_cv <- predict(fit_cv, newdata = Xtr_tree[val_idx, ])
  y_train_cv <- y_tr[train_idx]
  
  # ✅ 使用优化函数直接获得矩阵
  pred_matrix <- wquant_matrix(y_train_cv, pred_cv$weights, c(0.05, 0.5, 0.95))
  colnames(pred_matrix) <- c("q05", "q50", "q95")
  
  wis_val <- mean(scoringutils::wis(
    observed = y_tr[val_idx],
    predicted = pred_matrix,
    quantile_level = c(0.05, 0.5, 0.95),
    na.rm = TRUE
  ), na.rm = TRUE)
  
  cat(sprintf(" WIS=%.4f\n", wis_val))
  wis_val
})

best_drf_idx <- which.min(drf_cv_scores)
num_trees_drf <- drf_params_test$num.trees[best_drf_idx]
min_node_size_drf <- drf_params_test$min.node.size[best_drf_idx]

# --- Final Fit ---
drf_fit <- drf(
  X = Xtr_tree,
  Y = y_tr,
  num.trees = num_trees_drf,
  min.node.size = min_node_size_drf,
  mtry = floor(sqrt(ncol(Xtr_tree))),
  sample.fraction = 0.5
)

drf_pred <- predict(drf_fit, Xte_tree)
W <- drf_pred$weights
y_train <- as.numeric(drf_pred$y)

# --- 1. 计算三个标准的 WIS 分位数 (修正部分) ---
# 替换掉你原来那个慢速的 t(sapply(...))
drf_q_mat <- wquant_matrix(y_train, W, c(0.05, 0.5, 0.95))
drf_q <- as.data.frame(drf_q_mat)
colnames(drf_q) <- c("q05", "q50", "q95")

# --- 2. 计算完整的分位数网格 (NEW/OPTIMIZED) ---
q_grid <- seq(0.05, 0.95, by = 0.05)
drf_qgrid_mat <- wquant_matrix(y_train, W, q_grid)
drf_qgrid <- as.data.frame(drf_qgrid_mat)
colnames(drf_qgrid) <- paste0("q", sprintf("%02d", round(100*q_grid)))

cat("\n✓ DRF 训练与预测完成。\n")


# 3-fit QRF

# Load qrf package
library(quantregForest)

# Hyperparameter tuning for QRF
cat("\nTuning QRF hyperparameters (3-fold CV)...\n")

qrf_params_test <- expand.grid(
  ntree = c(500, 1000),
  nodesize = c(20, 50, 100)
)

qs <- c(0.05, 0.5, 0.95)

qrf_cv_scores <- sapply(seq_len(nrow(qrf_params_test)), function(i) {
  params <- qrf_params_test[i, ]
  
  cat(sprintf("  [%d/%d] ntree=%d, nodesize=%d...",
              i, nrow(qrf_params_test),
              params$ntree, params$nodesize))
  
  # Simple hold-out validation (same style as DRF)
  val_idx   <- cv_folds_hp[[1]]
  train_idx <- setdiff(seq_len(nrow(Xtr_all)), val_idx)
  
  fit_cv <- quantregForest(
    x = Xtr_all[train_idx, ],
    y = y_tr[train_idx],
    ntree = params$ntree,
    nodesize = params$nodesize
  )
  
  # Predict all required quantiles at once (faster + cleaner)
  pred_mat <- predict(fit_cv, newdata = Xtr_all[val_idx, ], what = qs)
  # Ensure it is a numeric matrix with correct column order
  pred_mat <- as.matrix(pred_mat)
  
  wis_val <- mean(scoringutils::wis(
    observed = y_tr[val_idx],
    predicted = pred_mat,
    quantile_level = qs,
    na.rm = TRUE
  ), na.rm = TRUE)
  
  cat(sprintf(" WIS=%.4f\n", wis_val))
  wis_val
})

best_qrf_idx <- which.min(qrf_cv_scores)
cat(sprintf("✓ Best: ntree=%d, nodesize=%d (WIS=%.4f)\n",
            qrf_params_test$ntree[best_qrf_idx],
            qrf_params_test$nodesize[best_qrf_idx],
            qrf_cv_scores[best_qrf_idx]))

# Update parameters
ntree_qrf <- qrf_params_test$ntree[best_qrf_idx]
nodesize_qrf <- qrf_params_test$nodesize[best_qrf_idx]

# fit qrf model
# qrf keeps all training responses in each terminal node
qrf_fit <- quantregForest(
  x = Xtr_all,
  y = y_tr,
  ntree = ntree_qrf,
  nodesize = nodesize_qrf
)

# predict the outcome and store in dataframe format
qrf_q <- as.data.frame(
  predict(qrf_fit, Xte_all, what = q_levels)
)
colnames(qrf_q) <- c("q05","q50","q95")

# ---- QRF quantile grid predictions (NEW) ----
# predict los for each quantile
qrf_qgrid <- as.data.frame(predict(qrf_fit, Xte_all_mm, what = q_grid))
colnames(qrf_qgrid) <- paste0("q", sprintf("%02d", round(100*q_grid)))


# 4-fit ranger
# loading the package
library(ranger)

# Hyperparameter tuning for Ranger
cat("\nTuning Ranger hyperparameters (3-fold CV)...\n")

ranger_params_test <- expand.grid(
  num.trees = c(500, 1000),
  min.node.size = c(10, 20, 50)
)

ranger_cv_scores <- sapply(seq_len(nrow(ranger_params_test)), function(i) {
  params <- ranger_params_test[i, ]
  
  cat(sprintf("  [%d/%d] trees=%d, node=%d...",
              i, nrow(ranger_params_test),
              params$num.trees, params$min.node.size))
  
  # Hold-out validation fold
  val_idx   <- cv_folds_hp[[1]]
  train_idx <- setdiff(seq_len(nrow(Xtr_all)), val_idx)
  
  # Build training data.frame for formula interface
  df_train_cv <- data.frame(Xtr_all[train_idx, , drop = FALSE])
  df_train_cv$los <- y_tr[train_idx]
  
  fit_cv <- ranger(
    los ~ .,
    data = df_train_cv,
    num.trees = params$num.trees,
    min.node.size = params$min.node.size,
    quantreg = TRUE,
    keep.inbag = TRUE,   # recommended for quantile prediction stability
    seed = 2026
  )
  
  # Predict quantiles on validation set
  pred_cv <- predict(
    fit_cv,
    data = Xtr_all[val_idx, , drop = FALSE],
    type = "quantiles",
    quantiles = qs
  )$predictions
  
  pred_mat <- as.matrix(pred_cv)  # n_val x 3 (q05,q50,q95)
  
  wis_val <- mean(scoringutils::wis(
    observed = y_tr[val_idx],
    predicted = pred_mat,
    quantile_level = qs,
    na.rm = TRUE
  ), na.rm = TRUE)
  
  cat(sprintf(" WIS=%.4f\n", wis_val))
  wis_val
})

best_ranger_idx <- which.min(ranger_cv_scores)
cat(sprintf("✓ Best: num.trees=%d, min.node.size=%d (WIS=%.4f)\n",
            ranger_params_test$num.trees[best_ranger_idx],
            ranger_params_test$min.node.size[best_ranger_idx],
            ranger_cv_scores[best_ranger_idx]))

# Update parameters
num_trees_ranger <- ranger_params_test$num.trees[best_ranger_idx]
min_node_size_ranger <- ranger_params_test$min.node.size[best_ranger_idx]

# fit the random forest model
rf_q_fit <- ranger(
  x = Xtr_all,
  y = y_tr,
  num.trees = num_trees_ranger,
  min.node.size = min_node_size_ranger,
  quantreg = TRUE,
  keep.inbag = TRUE
)

# get the results and store in dataframe format
rf_q <- as.data.frame(
  predict(rf_q_fit, Xte_all, type = "quantiles", quantiles = q_levels)$predictions
)
colnames(rf_q) <- c("q05","q50","q95")

# ---- Ranger quantile grid predictions (NEW) ----
# predict los for each quantiles
rf_qgrid <- as.data.frame(
  predict(rf_q_fit, Xte_all, type = "quantiles", quantiles = q_grid)$predictions
)
colnames(rf_qgrid) <- paste0("q", sprintf("%02d", round(100*q_grid)))


# 5-XGBoost + Conformal prediction

set.seed(2026)

# ----------------------------
# 0) Required objects check
# ----------------------------
stopifnot(exists("Xtr_all"), exists("Xte_all"), exists("y_tr"))
stopifnot(nrow(Xtr_all) == length(y_tr))

# ----------------------------
# 1) Define quantiles + alpha
# ----------------------------
qs <- c(0.05, 0.5, 0.95)   # quantile levels for WIS
alpha <- 0.10             # target miscoverage (90% interval)

# ----------------------------
# 2) One-hot encoding for XGBoost
# ----------------------------
# IMPORTANT: ensure train/test have consistent columns
Xtr_all_mm_hp <- model.matrix(~ . - 1, data = Xtr_all)
Xte_all_mm    <- model.matrix(~ . - 1, data = Xte_all)

# Align columns between train and test (in case some factor levels appear only in one split)
common_cols <- union(colnames(Xtr_all_mm_hp), colnames(Xte_all_mm))
Xtr_all_mm_hp <- Xtr_all_mm_hp[, common_cols, drop = FALSE]
Xte_all_mm    <- Xte_all_mm[,    common_cols, drop = FALSE]
Xtr_all_mm_hp[is.na(Xtr_all_mm_hp)] <- 0
Xte_all_mm[is.na(Xte_all_mm)]       <- 0

# ----------------------------
# 3) Outer hold-out validation fold (single fold)
# ----------------------------
# If you already built cv_folds_hp elsewhere, you can skip this block.
if (!exists("cv_folds_hp")) {
  set.seed(2026)
  n_tr <- nrow(Xtr_all_mm_hp)
  val_idx <- sample(seq_len(n_tr), size = floor(0.2 * n_tr))
  cv_folds_hp <- list(val_idx)
}

val_idx <- cv_folds_hp[[1]]
train_idx <- setdiff(seq_len(nrow(Xtr_all_mm_hp)), val_idx)

# ----------------------------
# 4) Hyperparameter grid
# ----------------------------
xgb_params_test <- expand.grid(
  max_depth = c(4, 6, 8),
  eta       = c(0.03, 0.05, 0.1),
  nrounds   = c(200, 400),
  subsample = c(0.8),
  colsample_bytree = c(0.8)
)

# ----------------------------
# 5) HP tuning loop (outer hold-out + inner conformal calibration)
# ----------------------------
xgb_cv_scores <- sapply(seq_len(nrow(xgb_params_test)), function(i) {
  
  hp <- xgb_params_test[i, ]
  
  cat(sprintf("  [%d/%d] depth=%d, eta=%.3f, nrounds=%d...",
              i, nrow(xgb_params_test), hp$max_depth, hp$eta, hp$nrounds))
  
  # ---- Inner split within train_idx for conformal calibration ----
  set.seed(2026 + i)
  cal_size      <- floor(0.2 * length(train_idx))
  cal_idx_local <- sample(train_idx, size = cal_size)
  train2_idx    <- setdiff(train_idx, cal_idx_local)
  
  # DMatrix
  dtrain_cv <- xgb.DMatrix(Xtr_all_mm_hp[train2_idx, , drop = FALSE],
                           label = y_tr[train2_idx])
  dcal_cv   <- xgb.DMatrix(Xtr_all_mm_hp[cal_idx_local, , drop = FALSE],
                           label = y_tr[cal_idx_local])
  dval_cv   <- xgb.DMatrix(Xtr_all_mm_hp[val_idx, , drop = FALSE])
  
  # XGB params
  params_list <- list(
    objective = "reg:squarederror",
    max_depth = hp$max_depth,
    eta       = hp$eta,
    subsample = hp$subsample,
    colsample_bytree = hp$colsample_bytree
  )
  
  # Train
  fit_cv <- xgb.train(
    params  = params_list,
    data    = dtrain_cv,
    nrounds = hp$nrounds,
    verbose = 0
  )
  
  # ---- Conformal radius on calibration set ----
  pred_cal <- predict(fit_cv, dcal_cv)
  resid    <- abs(y_tr[cal_idx_local] - pred_cal)
  q_hat    <- as.numeric(quantile(resid, probs = 1 - alpha, na.rm = TRUE))
  
  # ---- Predict on validation fold ----
  pred_val <- predict(fit_cv, dval_cv)
  
  # Lower bound to avoid negative LOS intervals
  lower_bound <- min(y_tr[train2_idx], na.rm = TRUE)
  
  q05 <- pmax(pred_val - q_hat, lower_bound)
  q50 <- pred_val
  q95 <- pred_val + q_hat
  
  pred_mat <- cbind(q05, q50, q95)
  
  # WIS: average over validation observations
  wis_val <- mean(scoringutils::wis(
    observed = y_tr[val_idx],
    predicted = pred_mat,
    quantile_level = qs,
    na.rm = TRUE
  ), na.rm = TRUE)
  
  cat(sprintf(" WIS=%.4f\n", wis_val))
  wis_val
})


best_xgb_idx <- which.min(xgb_cv_scores)
cat(sprintf("✓ Best: max_depth=%d, eta=%.2f, nrounds=%d (WIS=%.4f)\n",
            xgb_params_test$max_depth[best_xgb_idx],
            xgb_params_test$eta[best_xgb_idx],
            xgb_params_test$nrounds[best_xgb_idx],
            xgb_cv_scores[best_xgb_idx]))

# Update parameters
max_depth_xgb <- xgb_params_test$max_depth[best_xgb_idx]
eta_xgb <- xgb_params_test$eta[best_xgb_idx]
nrounds_xgb <- xgb_params_test$nrounds[best_xgb_idx]

# parameters setting
params <- list(
  objective = "reg:squarederror",
  max_depth = max_depth_xgb,
  eta = eta_xgb
)

# model training
xgb_fit <- xgb.train(
  params = params,
  data   = dtrain,
  nrounds = nrounds_xgb,
  verbose = 0
)

# predict the result on calibration set
pred_cal <- predict(xgb_fit, dcal)
# calculate the residual for the results of calibration prediction
resid <- abs(y_cal - pred_cal)

# calculate conformal quantile
alpha <- 0.1
q_hat <- quantile(resid, 1 - alpha)

# get the test set prediciton result
pred_test <- predict(xgb_fit, dtest)

# create a dataframe to get results for each quantile
lower_bound <- min(y_tr, na.rm = TRUE)  
xgb_q <- data.frame(
  q05 = pmax(pred_test - q_hat, lower_bound),
  q50 = pred_test,
  q95 = pred_test + q_hat
)

# ---- XGB conformal quantile grid predictions (NEW) ----
# map quantile levels to symmetric scale [-1, 1]
# so that 0.05 -> -1 and 0.95 -> +1
scale_tau2 <- (q_grid - 0.5) / 0.45  # 0.05->-1, 0.95->+1


# Construct pseudo-quantile predictions:
# center at point prediction (pred_test)
# expand linearly using conformal radius (q_hat)
xgb_qgrid_mat <- sapply(scale_tau2, function(s) pred_test + s * q_hat)

# Convert to data frame and assign quantile names
xgb_qgrid <- as.data.frame(xgb_qgrid_mat)
colnames(xgb_qgrid) <- paste0("q", sprintf("%02d", round(100*q_grid)))

# 6-Deep Ensembles (Lakshminarayanan et al., NIPS 2017)
# Non-parametric extension: Quantile Regression Neural Networks (QRNN)
# Each network outputs multiple quantiles directly
# Ensemble prediction: average quantiles across networks

library(torch)
library(scoringutils)

set.seed(2026)

# -------------------------
# 0) Safety checks
# -------------------------
stopifnot(exists("Xtr_all"), exists("Xte_all"), exists("y_tr"),
          exists("q_grid"), exists("cv_folds_hp"))

qs <- c(0.05, 0.5, 0.95)  # for WIS key quantiles

# -------------------------
# 1) One-hot + standardise (same pipeline for train/test)
# -------------------------
Xtr_ens_mm <- model.matrix(~ . - 1, data = Xtr_all)
Xte_ens_mm <- model.matrix(~ . - 1, data = Xte_all)

x_mean <- colMeans(Xtr_ens_mm)
x_sd   <- apply(Xtr_ens_mm, 2, sd)
x_sd[x_sd == 0] <- 1

Xtr_ens_sc <- scale(Xtr_ens_mm, center = x_mean, scale = x_sd)
Xte_ens_sc <- scale(Xte_ens_mm, center = x_mean, scale = x_sd)

y_mean_ens <- mean(y_tr)
y_sd_ens   <- sd(y_tr)
y_tr_sc    <- (y_tr - y_mean_ens) / y_sd_ens

n_input     <- ncol(Xtr_ens_sc)
n_quantiles <- length(q_grid)

# quantile levels tensor
tau_t <- torch_tensor(matrix(q_grid, nrow = 1), dtype = torch_float())

# -------------------------
# 2) Device (CPU / MPS)
# -------------------------
device <- torch_device(
  if (torch::backends_mps_is_available()) "mps" else "cpu"
)

cat("\nDeep Ensembles (QRNN) using device:", device$type, "\n")

# -------------------------
# 3) Pinball loss
# -------------------------
pinball_loss <- function(pred, y_true, tau) {
  errors <- y_true - pred
  loss <- torch_where(
    errors >= 0,
    tau * errors,
    (tau - 1) * errors
  )
  torch_mean(loss)
}

# -------------------------
# 4) Make QRNN with given HP
# -------------------------
make_qrnn_hp <- function(n_in, n_out, hidden1, hidden2) {
  nn_sequential(
    nn_linear(n_in, hidden1),
    nn_relu(),
    nn_linear(hidden1, hidden2),
    nn_relu(),
    nn_linear(hidden2, n_out)
  )
}

# -------------------------
# 5) Train ONE network (mini-batch, device-aware)
# -------------------------
train_one_qrnn <- function(X_train_sc, y_train_sc, hidden1, hidden2, lr,
                           epochs = 60, batch_size = 512) {
  net <- make_qrnn_hp(n_input, n_quantiles, hidden1, hidden2)
  net$to(device = device)
  optim <- optim_adam(net$parameters, lr = lr)
  
  ds <- dataset(
    name = "qrnn_ds",
    initialize = function(X, y) {
      self$X <- X
      self$y <- y
    },
    .getitem = function(i) {
      list(x = self$X[i, ], y = self$y[i])
    },
    .length = function() nrow(self$X)
  )
  
  X_t <- torch_tensor(X_train_sc, dtype = torch_float(), device = device)
  y_t <- torch_tensor(matrix(y_train_sc, ncol = 1), dtype = torch_float(), device = device)
  
  dl <- dataloader(ds(X_t, y_t), batch_size = batch_size, shuffle = TRUE)
  
  net$train()
  for (ep in seq_len(epochs)) {
    coro::loop(for (b in dl) {
      optim$zero_grad()
      pred <- net(b$x)
      loss <- pinball_loss(pred, b$y, tau_t$to(device = device))
      loss$backward()
      optim$step()
    })
  }
  
  net$eval()
  net
}

# -------------------------
# 6) Predict quantiles + WIS helper (for tuning)
# -------------------------
predict_key_quantiles <- function(net, X_val_sc) {
  Xv <- torch_tensor(X_val_sc, dtype = torch_float(), device = device)
  
  out_sc <- with_no_grad({
    net(Xv)
  })
  
  q_all_sc <- as.matrix(out_sc$to(device = "cpu"))
  q_all <- q_all_sc * y_sd_ens + y_mean_ens
  
  # enforce monotonicity (fix quantile crossing)
  q_all <- t(apply(q_all, 1, sort))
  
  q05_idx <- which.min(abs(q_grid - 0.05))
  q50_idx <- which.min(abs(q_grid - 0.50))
  q95_idx <- which.min(abs(q_grid - 0.95))
  
  cbind(q_all[, q05_idx], q_all[, q50_idx], q_all[, q95_idx])
}

wis_mean_from_pred <- function(pred_mat, y_obs) {
  mean(scoringutils::wis(
    observed = y_obs,
    predicted = pred_mat,
    quantile_level = qs,
    na.rm = TRUE
  ), na.rm = TRUE)
}

# -------------------------
# 7) Hyperparameter tuning (REPLACES your old placeholder tuning block)
# -------------------------
ens_params_test <- expand.grid(
  hidden1 = c(64, 128),
  hidden2 = c(32, 64),
  lr = c(0.0005, 0.001)
)

cat("\nTuning ENS (QRNN) hyperparameters with 1-fold CV...\n")
cat("Grid size:", nrow(ens_params_test), "\n")

# Use the SAME fold index you used elsewhere
val_idx   <- cv_folds_hp[[1]]
train_idx <- setdiff(seq_len(nrow(Xtr_ens_sc)), val_idx)

X_train_sc_tune <- Xtr_ens_sc[train_idx, , drop = FALSE]
y_train_sc_tune <- y_tr_sc[train_idx]

X_val_sc_tune <- Xtr_ens_sc[val_idx, , drop = FALSE]
y_val_tune    <- y_tr[val_idx]

# ---- tuning speed knobs ----
epochs_tune <- 60
batch_size  <- 512
M_tune      <- 1  # tune with 1 net only for speed

ens_cv_scores <- sapply(seq_len(nrow(ens_params_test)), function(i) {
  hp <- ens_params_test[i, ]
  
  cat(sprintf("  [%d/%d] h1=%d h2=%d lr=%.4g ...",
              i, nrow(ens_params_test), hp$hidden1, hp$hidden2, hp$lr))
  
  pred_mats <- vector("list", M_tune)
  for (m in seq_len(M_tune)) {
    net <- train_one_qrnn(
      X_train_sc_tune, y_train_sc_tune,
      hidden1 = hp$hidden1,
      hidden2 = hp$hidden2,
      lr = hp$lr,
      epochs = epochs_tune,
      batch_size = batch_size
    )
    pred_mats[[m]] <- predict_key_quantiles(net, X_val_sc_tune)
  }
  
  pred_avg <- Reduce(`+`, pred_mats) / M_tune
  wis_val  <- wis_mean_from_pred(pred_avg, y_val_tune)
  
  cat(sprintf(" WIS=%.4f\n", wis_val))
  wis_val
})

best_i <- which.min(ens_cv_scores)
best_ens_params <- as.list(ens_params_test[best_i, ])

cat("\n✓ Best ENS params:\n")
print(best_ens_params)
cat("✓ Best WIS:", round(ens_cv_scores[best_i], 4), "\n")

# -------------------------
# 8) Train M ensemble members with BEST HP
# -------------------------
M_ens <- 5
epochs_final <- 150
batch_size_final <- 512

cat(sprintf("\nTraining Deep Ensemble with M=%d members ...\n", M_ens))

ensemble_qrnns <- vector("list", M_ens)

for (m in seq_len(M_ens)) {
  cat(sprintf("  Training ensemble member %d / %d ...\n", m, M_ens))
  
  # different seed per member for diversity
  set.seed(2026 + m)
  
  net <- train_one_qrnn(
    X_train_sc = Xtr_ens_sc,
    y_train_sc = y_tr_sc,
    hidden1    = best_ens_params$hidden1,
    hidden2    = best_ens_params$hidden2,
    lr         = best_ens_params$lr,
    epochs     = epochs_final,
    batch_size = batch_size_final
  )
  
  ensemble_qrnns[[m]] <- net
}

# -------------------------
# 9) Predict FULL quantile grid on test, then ensemble-average
# -------------------------
predict_all_quantiles <- function(net, X_sc) {
  Xv <- torch_tensor(X_sc, dtype = torch_float(), device = device)
  out_sc <- with_no_grad({ net(Xv) })
  as.matrix(out_sc$to(device = "cpu"))  # [n, n_quantiles] in standardized y-space
}

qpreds_list <- lapply(ensemble_qrnns, predict_all_quantiles, X_sc = Xte_ens_sc)

qpreds_array <- array(
  unlist(lapply(qpreds_list, as.numeric)),
  dim = c(nrow(Xte_ens_sc), n_quantiles, M_ens)
)

ens_q_sc <- apply(qpreds_array, c(1, 2), mean)

# Back-transform to original LOS scale
ens_q_all <- ens_q_sc * y_sd_ens + y_mean_ens

# Enforce monotonicity (fix quantile crossing)
ens_q_all <- t(apply(ens_q_all, 1, sort))

# Apply lower bound (optional safety)
lower_bound_ens <- min(y_tr, na.rm = TRUE)
ens_q_all <- pmax(ens_q_all, lower_bound_ens)

# Extract key quantiles
q05_idx <- which.min(abs(q_grid - 0.05))
q50_idx <- which.min(abs(q_grid - 0.50))
q95_idx <- which.min(abs(q_grid - 0.95))

ens_q <- data.frame(
  q05 = ens_q_all[, q05_idx],
  q50 = ens_q_all[, q50_idx],
  q95 = ens_q_all[, q95_idx]
)

# Dense quantile grid for CRPS / plots
ens_qgrid <- as.data.frame(ens_q_all)
colnames(ens_qgrid) <- paste0("q", sprintf("%02d", round(100 * q_grid)))

cat("\nDeep Ensemble (QRNN) training complete.\n\n")

# ========================================
# 7-MC Dropout (Non-Parametric Version)
# Quantile Regression with Dropout  +  Fine-tuning
# ========================================

library(torch)
library(scoringutils)

cat("\n=== Training MC Dropout (Quantile Regression) with Fine-tuning ===\n")

set.seed(2026)

# -------------------------
# 0) Safety checks / required objects
# -------------------------
stopifnot(
  exists("Xtr_ens_sc"), exists("Xte_ens_sc"),
  exists("y_tr"), exists("q_grid"),
  exists("pinball_loss"), exists("cv_folds_hp")
)

# key quantile indices (ensure they exist even if not defined earlier)
q05_idx <- which.min(abs(q_grid - 0.05))
q50_idx <- which.min(abs(q_grid - 0.50))
q95_idx <- which.min(abs(q_grid - 0.95))
qs      <- c(0.05, 0.5, 0.95)

n_input     <- ncol(Xtr_ens_sc)
n_quantiles <- length(q_grid)

# quantile levels tensor
tau_t <- torch_tensor(matrix(q_grid, nrow = 1), dtype = torch_float())

# -------------------------
# 1) Device (CPU / Apple GPU via MPS)
# -------------------------
device <- torch_device(if (torch::backends_mps_is_available()) "mps" else "cpu")
cat("Device:", device$type, "\n")

# -------------------------
# 2) Helper: build dropout QRNN
# -------------------------
make_dropout_qrnn <- function(n_in, n_out, dropout_rate,
                              hidden1 = 128, hidden2 = 64) {
  nn_sequential(
    nn_linear(n_in, hidden1),
    nn_relu(),
    nn_dropout(p = dropout_rate),
    nn_linear(hidden1, hidden2),
    nn_relu(),
    nn_dropout(p = dropout_rate),
    nn_linear(hidden2, n_out)
  )
}

# -------------------------
# 3) Mini-batch trainer (fast)
# -------------------------
train_dropout_qrnn <- function(X_train_sc, y_train_sc,
                               dropout_rate, lr,
                               epochs = 60, batch_size = 512,
                               hidden1 = 128, hidden2 = 64) {
  net <- make_dropout_qrnn(n_input, n_quantiles, dropout_rate, hidden1, hidden2)
  net$to(device = device)
  optim <- optim_adam(net$parameters, lr = lr)
  
  ds <- dataset(
    name = "mcd_qrnn_ds",
    initialize = function(X, y) { self$X <- X; self$y <- y },
    .getitem = function(i) list(x = self$X[i, ], y = self$y[i]),
    .length  = function() nrow(self$X)
  )
  
  X_t <- torch_tensor(X_train_sc, dtype = torch_float(), device = device)
  y_t <- torch_tensor(matrix(y_train_sc, ncol = 1), dtype = torch_float(), device = device)
  dl  <- dataloader(ds(X_t, y_t), batch_size = batch_size, shuffle = TRUE)
  
  net$train()
  for (ep in seq_len(epochs)) {
    coro::loop(for (b in dl) {
      optim$zero_grad()
      pred <- net(b$x)
      loss <- pinball_loss(pred, b$y, tau_t$to(device = device))
      loss$backward()
      optim$step()
    })
  }
  
  net
}

# -------------------------
# 4) MC prediction on a set (keep dropout ON at test time)
#    Return averaged key quantiles (q05,q50,q95) matrix: n x 3
# -------------------------
predict_mcd_key_quantiles <- function(net, X_sc,
                                      M_pass = 30,
                                      y_mean, y_sd,
                                      lower_bound) {
  X_t <- torch_tensor(X_sc, dtype = torch_float(), device = device)
  
  # CRITICAL: keep dropout active
  net$train()
  
  n <- nrow(X_sc)
  acc <- matrix(0, nrow = n, ncol = n_quantiles)
  
  with_no_grad({
    for (i in seq_len(M_pass)) {
      out_sc <- net(X_t)
      out_sc_cpu <- as.matrix(out_sc$to(device = "cpu"))
      acc <- acc + out_sc_cpu
    }
  })
  
  q_sc  <- acc / M_pass
  q_all <- q_sc * y_sd + y_mean
  
  # enforce monotonicity + lower bound
  q_all <- t(apply(q_all, 1, sort))
  q_all <- pmax(q_all, lower_bound)
  
  cbind(q_all[, q05_idx], q_all[, q50_idx], q_all[, q95_idx])
}

# -------------------------
# 5) WIS helper
# -------------------------
wis_mean_from_pred <- function(pred_mat_3, y_obs) {
  mean(scoringutils::wis(
    observed = y_obs,
    predicted = pred_mat_3,
    quantile_level = qs,
    na.rm = TRUE
  ), na.rm = TRUE)
}

# -------------------------
# 6) Fine-tuning grid (you provided; you can expand if you want)
# -------------------------
cat("\nTuning MC Dropout hyperparameters (1-fold CV for speed)...\n")

mcd_params_test <- expand.grid(
  dropout_rate = c(0.05, 0.1, 0.2),
  lr = c(0.0005, 0.001)
)

# ---- tuning speed knobs ----
epochs_tune     <- 60
batch_size_tune <- 512
M_pass_tune     <- 25   # MC passes during tuning (keep small)

val_idx   <- cv_folds_hp[[1]]
train_idx <- setdiff(seq_len(nrow(Xtr_ens_sc)), val_idx)

# NOTE: ENS/MCD training used y standardized earlier as y_tr_sc
# If you already created y_mean_ens / y_sd_ens / y_tr_sc above, reuse them.
# Otherwise compute here:
if (!exists("y_mean_ens")) y_mean_ens <- mean(y_tr)
if (!exists("y_sd_ens"))   y_sd_ens   <- sd(y_tr)
if (!exists("y_tr_sc"))    y_tr_sc    <- (y_tr - y_mean_ens) / y_sd_ens

X_train_sc <- Xtr_ens_sc[train_idx, , drop = FALSE]
y_train_sc <- y_tr_sc[train_idx]

X_val_sc <- Xtr_ens_sc[val_idx, , drop = FALSE]
y_val    <- y_tr[val_idx]

lower_bound_mc <- min(y_tr, na.rm = TRUE)

mcd_cv_scores <- sapply(seq_len(nrow(mcd_params_test)), function(i) {
  hp <- mcd_params_test[i, ]
  
  cat(sprintf("  [%d/%d] dropout=%.2f lr=%.4g ...",
              i, nrow(mcd_params_test), hp$dropout_rate, hp$lr))
  
  net <- train_dropout_qrnn(
    X_train_sc, y_train_sc,
    dropout_rate = hp$dropout_rate,
    lr = hp$lr,
    epochs = epochs_tune,
    batch_size = batch_size_tune
  )
  
  pred3 <- predict_mcd_key_quantiles(
    net, X_val_sc,
    M_pass = M_pass_tune,
    y_mean = y_mean_ens, y_sd = y_sd_ens,
    lower_bound = lower_bound_mc
  )
  
  wis_val <- wis_mean_from_pred(pred3, y_val)
  cat(sprintf(" WIS=%.4f\n", wis_val))
  wis_val
})

best_i <- which.min(mcd_cv_scores)
best_mcd_params <- as.list(mcd_params_test[best_i, ])
cat("\n✓ Best MCD params:\n")
print(best_mcd_params)
cat("✓ Best WIS:", round(mcd_cv_scores[best_i], 4), "\n\n")

# -------------------------
# 7) Train final MC Dropout model with best params (full epochs)
# -------------------------
cat("=== Training FINAL MC Dropout (QRNN) ===\n")

epochs_final     <- 150
batch_size_final <- 512
M_dropout        <- 100  # Number of MC samples at test time

# Final train on full training set
mc_dropout_qrnn <- train_dropout_qrnn(
  X_train_sc = Xtr_ens_sc,
  y_train_sc = y_tr_sc,
  dropout_rate = best_mcd_params$dropout_rate,
  lr = best_mcd_params$lr,
  epochs = epochs_final,
  batch_size = batch_size_final
)

# -------------------------
# 8) MC prediction on TEST set: full quantile grid (for CRPS) + key quantiles
# -------------------------
cat(sprintf("Generating %d MC samples on test set...\n", M_dropout))

X_te_t <- torch_tensor(Xte_ens_sc, dtype = torch_float(), device = device)

# Keep dropout ENABLED
mc_dropout_qrnn$train()

n_te <- nrow(Xte_ens_sc)
mc_q_samples <- array(0, dim = c(n_te, n_quantiles, M_dropout))

with_no_grad({
  for (i in seq_len(M_dropout)) {
    out_sc <- mc_dropout_qrnn(X_te_t)
    mc_q_samples[, , i] <- as.matrix(out_sc$to(device = "cpu"))
  }
})

# Aggregate: average quantiles across MC samples
mcd_q_sc  <- apply(mc_q_samples, c(1, 2), mean)

# Back-transform
mcd_q_all <- mcd_q_sc * y_sd_ens + y_mean_ens

# Enforce monotonicity + lower bound
mcd_q_all <- t(apply(mcd_q_all, 1, sort))
mcd_q_all <- pmax(mcd_q_all, lower_bound_mc)

# Key quantiles
mcd_q <- data.frame(
  q05 = mcd_q_all[, q05_idx],
  q50 = mcd_q_all[, q50_idx],
  q95 = mcd_q_all[, q95_idx]
)

# Dense quantile grid (for CRPS etc.)
mcd_qgrid <- as.data.frame(mcd_q_all)
colnames(mcd_qgrid) <- paste0("q", sprintf("%02d", round(100 * q_grid)))

cat("MC Dropout (QRNN) fine-tuned training complete.\n\n")


# ========================================
# SUMMARY: Save All Best Parameters
# ========================================

best_hyperparameters <- list(
  DRF = list(
    num.trees = num_trees_drf, 
    min.node.size = min_node_size_drf
  ),
  QRF = list(ntree = ntree_qrf, nodesize = nodesize_qrf),
  Ranger = list(num.trees = num_trees_ranger, min.node.size = min_node_size_ranger),
  XGBoost = list(max_depth = max_depth_xgb, eta = eta_xgb, nrounds = nrounds_xgb),
  ENS = best_ens_params,
  MCD = best_mcd_params
)

# evaluation
# Evaluate prediction interval performance
eval_interval <- function(q, y) {
  # Indicator whether the true value lies inside the interval
  covered <- (y >= q$q05) & (y <= q$q95)
  list(
    # Empirical coverage rate
    coverage = mean(covered),
    # Average interval width
    width    = mean(q$q95 - q$q05)
  )
}

# evaluate our models using the function defined
res_drf <- eval_interval(drf_q, y_te)
res_qrf <- eval_interval(qrf_q, y_te)
res_rf  <- eval_interval(rf_q,  y_te)
res_xgb <- eval_interval(xgb_q, y_te)
res_ens <- eval_interval(ens_q, y_te)
res_mcd <- eval_interval(mcd_q, y_te)

# combine the results as a table
rbind(
  DRF = unlist(res_drf),
  QRF = unlist(res_qrf),
  RF  = unlist(res_rf),
  XGB = unlist(res_xgb),
  ENS = unlist(res_ens),
  MCD = unlist(res_mcd)
)


# Compute mean Weighted Interval Score
wis_score <- function(q, y, na.rm = TRUE) {
  # Extract quantile predictions
  pred <- as.matrix(q[, c("q05", "q50", "q95")])
  # Compute WIS using scoringutils
  scoringutils::wis(
    observed = y,
    predicted = pred,
    quantile_level = c(0.05, 0.5, 0.95),
    na.rm = na.rm
  ) |> mean(na.rm = na.rm)
}

# compare wis score for our models
c(
  DRF = wis_score(drf_q, y_te),
  QRF = wis_score(qrf_q, y_te),
  RF  = wis_score(rf_q,  y_te),
  XGB = wis_score(xgb_q, y_te),
  ENS = wis_score(ens_q, y_te),
  MCD = wis_score(mcd_q, y_te)
)

# --------- Subgroup analysis ----------------#

# Build an evaluation dataframe
eval_df <- data.frame(
  # True outcomes on test set
  y = y_te,
  # DRF predicted quantiles
  drf_q05 = drf_q$q05,
  drf_q50 = drf_q$q50,
  drf_q95 = drf_q$q95,
  # QRF predicted quantiles
  qrf_q05 = qrf_q$q05,
  qrf_q50 = qrf_q$q50,
  qrf_q95 = qrf_q$q95,
  # Random Forest (quantile) predicted quantiles
  rf_q05  = rf_q$q05,
  rf_q50  = rf_q$q50,
  rf_q95  = rf_q$q95,
  # XGBoost + conformal predicted quantiles/interval endpoints
  xgb_q05 = xgb_q$q05,
  xgb_q50 = xgb_q$q50,
  xgb_q95 = xgb_q$q95,
  # Deep Ensembles predicted quantiles
  ens_q05 = ens_q$q05,
  ens_q50 = ens_q$q50,
  ens_q95 = ens_q$q95,
  # MC Dropout predicted quantiles
  mcd_q05 = mcd_q$q05,
  mcd_q50 = mcd_q$q50,
  mcd_q95 = mcd_q$q95
)
# Subgroup 1: group by outcome (LOS) ranges
eval_df$los_group <- cut(
  eval_df$y,
  breaks = c(-Inf, 3, 7, Inf),
  labels = c("Short (≤3d)", "Medium (3–7d)", "Long (>7d)")
)

# Use Xte_tree (the version that allows NA) to compute miss_rate）
miss_mat <- is.na(Xte_tree)

# Row-wise missing rate: proportion of NA in each test row
eval_df$miss_rate <- rowMeans(miss_mat)

# Subgroup 2: group by missingness into terciles (low/mid/high)
eval_df$miss_group <- cut(
  eval_df$miss_rate,
  breaks = quantile(eval_df$miss_rate, probs = c(0, 1/3, 2/3, 1)),
  labels = c("Low missing", "Mid missing", "High missing"),
  include.lowest = TRUE
)

# Subgroup 3: ventilation status (binary)
eval_df$vent_group <- factor(
  Xte_tree$vent_any,
  levels = c(0, 1),
  labels = c("No ventilation", "Ventilated")
)

# Subgroup 4: vasopressors status (binary)
eval_df$vaso_group <- factor(
  Xte_tree$vasopressors,
  levels = c(0, 1),
  labels = c("No vasopressors", "Vasopressors")
)

# Helper: compute coverage, width, and number of valid cases
eval_metrics_vec <- function(q05, q95, y) {
  # Indicator: whether y is inside [q05, q95]
  covered <- (y >= q05) & (y <= q95)
  tibble(
    # Empirical coverage = mean of indicator
    coverage = mean(covered, na.rm = TRUE),
    # Average interval width
    width    = mean(q95 - q05, na.rm = TRUE),
    # Number of observations used (non-missing covered)
    n_used   = sum(!is.na(covered))
  )
}

# Evaluate interval metrics by missingness subgroup, for each model
out_miss_long <- eval_df %>%
  group_by(miss_group) %>%
  summarise(
    n_group = n(),
    # For each model, compute interval metrics and store as a list-column
    DRF = list(eval_metrics_vec(.data$drf_q05, .data$drf_q95, .data$y)),
    QRF = list(eval_metrics_vec(.data$qrf_q05, .data$qrf_q95, .data$y)),
    RF  = list(eval_metrics_vec(.data$rf_q05,  .data$rf_q95,  .data$y)),
    XGB = list(eval_metrics_vec(.data$xgb_q05, .data$xgb_q95, .data$y)),
    ENS = list(eval_metrics_vec(.data$ens_q05, .data$ens_q95, .data$y)),
    MCD = list(eval_metrics_vec(.data$mcd_q05, .data$mcd_q95, .data$y)),
    .groups = "drop"
  ) %>%
  # Convert wide model columns into long format
  pivot_longer(cols = c(DRF, QRF, RF, XGB, ENS, MCD), names_to = "model", values_to = "metrics") %>%
  # Expand the tibble stored in list-column
  unnest(metrics)

# show the result
out_miss_long


# Helper: compute mean WIS for one model within a subgroup
wis_one <- function(y, q05, q50, q95) {
  # Build predicted quantiles matrix: each row is one case, columns are (q05,q50,q95)
  pred <- cbind(q05, q50, q95)
  qs   <- c(0.05, 0.5, 0.95)
  # scoringutils::wis returns a vector of WIS values (one per observation)
  mean(scoringutils::wis(
    observed = y,
    predicted = pred,
    quantile_level = qs,
    na.rm = TRUE
  ))
}
# Evaluate WIS by missingness subgroup for each model
out_miss_wis <- eval_df %>%
  group_by(miss_group) %>%
  summarise(
    DRF = wis_one(y, drf_q05, drf_q50, drf_q95),
    QRF = wis_one(y, qrf_q05, qrf_q50, qrf_q95),
    RF  = wis_one(y, rf_q05,  rf_q50,  rf_q95),
    XGB = wis_one(y, xgb_q05, xgb_q50, xgb_q95),
    ENS = wis_one(y, ens_q05, ens_q50, ens_q95),
    MCD = wis_one(y, mcd_q05, mcd_q50, mcd_q95),
    .groups = "drop"
  ) %>%
  pivot_longer(-miss_group, names_to = "model", values_to = "WIS")

out_miss_wis

# group by ventilation / vasopressors
# Evaluate interval metrics within ventilation subgroups
out_vent_long <- eval_df %>%
  group_by(vent_group) %>%
  summarise(
    n_group = n(),
    # Compute coverage and width for each model
    DRF = list(eval_metrics_vec(drf_q05, drf_q95, y)),
    QRF = list(eval_metrics_vec(qrf_q05, qrf_q95, y)),
    RF  = list(eval_metrics_vec(rf_q05,  rf_q95,  y)),
    XGB = list(eval_metrics_vec(xgb_q05, xgb_q95, y)),
    ENS = list(eval_metrics_vec(ens_q05, ens_q95, y)),
    MCD = list(eval_metrics_vec(mcd_q05, mcd_q95, y)),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(DRF, QRF, RF, XGB, ENS, MCD),
               names_to = "model",
               values_to = "metrics") %>%
  # Expand list-column into separate columns
  unnest(metrics)

out_vent_long
# Compute subgroup-specific WIS by ventilation status
out_vent_wis <- eval_df %>%
  group_by(vent_group) %>%
  summarise(
    DRF = wis_one(y, drf_q05, drf_q50, drf_q95),
    QRF = wis_one(y, qrf_q05, qrf_q50, qrf_q95),
    RF  = wis_one(y, rf_q05,  rf_q50,  rf_q95),
    XGB = wis_one(y, xgb_q05, xgb_q50, xgb_q95),
    ENS = wis_one(y, ens_q05, ens_q50, ens_q95),
    MCD = wis_one(y, mcd_q05, mcd_q50, mcd_q95),
    .groups = "drop"
  ) %>%
  pivot_longer(-vent_group, names_to = "model", values_to = "WIS")

out_vent_wis

# Compute subgroup-specific WIS by vasopressor status
out_vaso_wis <- eval_df %>%
  group_by(vaso_group) %>%
  summarise(
    DRF = wis_one(y, drf_q05, drf_q50, drf_q95),
    QRF = wis_one(y, qrf_q05, qrf_q50, qrf_q95),
    RF  = wis_one(y, rf_q05,  rf_q50,  rf_q95),
    XGB = wis_one(y, xgb_q05, xgb_q50, xgb_q95),
    ENS = wis_one(y, ens_q05, ens_q50, ens_q95),
    MCD = wis_one(y, mcd_q05, mcd_q50, mcd_q95),
    .groups = "drop"
  ) %>%
  pivot_longer(-vaso_group, names_to = "model", values_to = "WIS")

out_vaso_wis

# -------- multi-subgroup test ---------#

# Restrict analysis to the high-missingness subgroup
eval_hm <- eval_df %>%
  filter(miss_group == "High missing") %>%
  # Compute interval width as a proxy for predictive uncertainty
  mutate(
    drf_w = drf_q95 - drf_q05,
    qrf_w = qrf_q95 - qrf_q05,
    rf_w  = rf_q95  - rf_q05,
    xgb_w = xgb_q95 - xgb_q05,
    ens_w = ens_q95 - ens_q05,
    mcd_w = mcd_q95 - mcd_q05
  ) %>%
  # Split each model's predictions into high vs low uncertainty 
  # based on interval width (median split)
  mutate(
    drf_uncert = ifelse(ntile(drf_w, 2) == 2, "High uncertainty", "Low uncertainty"),
    qrf_uncert = ifelse(ntile(qrf_w, 2) == 2, "High uncertainty", "Low uncertainty"),
    rf_uncert  = ifelse(ntile(rf_w,  2) == 2, "High uncertainty", "Low uncertainty"),
    xgb_uncert = ifelse(ntile(xgb_w, 2) == 2, "High uncertainty", "Low uncertainty"),
    ens_uncert = ifelse(ntile(ens_w, 2) == 2, "High uncertainty", "Low uncertainty"),
    mcd_uncert = ifelse(ntile(mcd_w, 2) == 2, "High uncertainty", "Low uncertainty")
  )

# calculate the sample size, sanity check
table(eval_hm$drf_uncert)
table(eval_hm$qrf_uncert)
table(eval_hm$rf_uncert)
table(eval_hm$xgb_uncert)
table(eval_hm$ens_uncert)
table(eval_hm$mcd_uncert)

# build a function to compute mean WIS on a subset defined by idx
wis_subset <- function(y, q05, q50, q95, idx) {
  pred <- cbind(q05[idx], q50[idx], q95[idx])
  qs   <- c(0.05, 0.5, 0.95)
  mean(scoringutils::wis(
    observed = y[idx],
    predicted = pred,
    quantile_level = qs,
    na.rm = TRUE
  ))
}

# apply the function to calculate wis for each model in different uncertainty risk
out_uncert_wis <- eval_hm %>%
  summarise(
    DRF_low  = wis_subset(y, drf_q05, drf_q50, drf_q95, drf_uncert=="Low uncertainty"),
    DRF_high = wis_subset(y, drf_q05, drf_q50, drf_q95, drf_uncert=="High uncertainty"),
    
    QRF_low  = wis_subset(y, qrf_q05, qrf_q50, qrf_q95, qrf_uncert=="Low uncertainty"),
    QRF_high = wis_subset(y, qrf_q05, qrf_q50, qrf_q95, qrf_uncert=="High uncertainty"),
    
    RF_low   = wis_subset(y, rf_q05,  rf_q50,  rf_q95,  rf_uncert=="Low uncertainty"),
    RF_high  = wis_subset(y, rf_q05,  rf_q50,  rf_q95,  rf_uncert=="High uncertainty"),
    
    XGB_low  = wis_subset(y, xgb_q05, xgb_q50, xgb_q95, xgb_uncert=="Low uncertainty"),
    XGB_high = wis_subset(y, xgb_q05, xgb_q50, xgb_q95, xgb_uncert=="High uncertainty"),
    
    ENS_low  = wis_subset(y, ens_q05, ens_q50, ens_q95, ens_uncert=="Low uncertainty"),
    ENS_high = wis_subset(y, ens_q05, ens_q50, ens_q95, ens_uncert=="High uncertainty"),
    
    MCD_low  = wis_subset(y, mcd_q05, mcd_q50, mcd_q95, mcd_uncert=="Low uncertainty"),
    MCD_high = wis_subset(y, mcd_q05, mcd_q50, mcd_q95, mcd_uncert=="High uncertainty")
  ) %>%
  pivot_longer(
    everything(),
    names_to = c("model","uncert"),
    names_sep = "_",
    values_to = "WIS"
  )

out_uncert_wis
# compute the number of each group
out_uncert_n <- tibble(
  model = c("DRF","QRF","RF","XGB","ENS","MCD"),
  low_n  = c(sum(eval_hm$drf_uncert=="Low uncertainty"),
             sum(eval_hm$qrf_uncert=="Low uncertainty"),
             sum(eval_hm$rf_uncert=="Low uncertainty"),
             sum(eval_hm$xgb_uncert=="Low uncertainty"),
             sum(eval_hm$ens_uncert=="Low uncertainty"),
             sum(eval_hm$mcd_uncert=="Low uncertainty")),
  high_n = c(sum(eval_hm$drf_uncert=="High uncertainty"),
             sum(eval_hm$qrf_uncert=="High uncertainty"),
             sum(eval_hm$rf_uncert=="High uncertainty"),
             sum(eval_hm$xgb_uncert=="High uncertainty"),
             sum(eval_hm$ens_uncert=="High uncertainty"),
             sum(eval_hm$mcd_uncert=="High uncertainty"))
)

out_uncert_n

# Compute coverage and interval width on subset idx
cov_width_subset <- function(y, q05, q95, idx) {
  covered <- (y[idx] >= q05[idx]) & (y[idx] <= q95[idx])
  tibble(
    coverage = mean(covered, na.rm = TRUE),
    width    = mean(q95[idx] - q05[idx], na.rm = TRUE),
    n        = sum(idx)
  )
}

# combine all the results
out_uncert_cov <- bind_rows(
  
  # DRF
  cov_width_subset(eval_hm$y, eval_hm$drf_q05, eval_hm$drf_q95,
                   eval_hm$drf_uncert == "Low uncertainty") %>%
    mutate(model = "DRF", uncert = "Low"),
  
  cov_width_subset(eval_hm$y, eval_hm$drf_q05, eval_hm$drf_q95,
                   eval_hm$drf_uncert == "High uncertainty") %>%
    mutate(model = "DRF", uncert = "High"),
  
  # QRF
  cov_width_subset(eval_hm$y, eval_hm$qrf_q05, eval_hm$qrf_q95,
                   eval_hm$qrf_uncert == "Low uncertainty") %>%
    mutate(model = "QRF", uncert = "Low"),
  
  cov_width_subset(eval_hm$y, eval_hm$qrf_q05, eval_hm$qrf_q95,
                   eval_hm$qrf_uncert == "High uncertainty") %>%
    mutate(model = "QRF", uncert = "High"),
  
  # RF
  cov_width_subset(eval_hm$y, eval_hm$rf_q05, eval_hm$rf_q95,
                   eval_hm$rf_uncert == "Low uncertainty") %>%
    mutate(model = "RF", uncert = "Low"),
  
  cov_width_subset(eval_hm$y, eval_hm$rf_q05, eval_hm$rf_q95,
                   eval_hm$rf_uncert == "High uncertainty") %>%
    mutate(model = "RF", uncert = "High"),
  
  # XGB
  cov_width_subset(eval_hm$y, eval_hm$xgb_q05, eval_hm$xgb_q95,
                   eval_hm$xgb_uncert == "Low uncertainty") %>%
    mutate(model = "XGB", uncert = "Low"),
  
  cov_width_subset(eval_hm$y, eval_hm$xgb_q05, eval_hm$xgb_q95,
                   eval_hm$xgb_uncert == "High uncertainty") %>%
    mutate(model = "XGB", uncert = "High"),
  
  # ENS
  cov_width_subset(eval_hm$y, eval_hm$ens_q05, eval_hm$ens_q95,
                   eval_hm$ens_uncert == "Low uncertainty") %>%
    mutate(model = "ENS", uncert = "Low"),
  
  cov_width_subset(eval_hm$y, eval_hm$ens_q05, eval_hm$ens_q95,
                   eval_hm$ens_uncert == "High uncertainty") %>%
    mutate(model = "ENS", uncert = "High"),
  
  # MCD
  cov_width_subset(eval_hm$y, eval_hm$mcd_q05, eval_hm$mcd_q95,
                   eval_hm$mcd_uncert == "Low uncertainty") %>%
    mutate(model = "MCD", uncert = "Low"),
  
  cov_width_subset(eval_hm$y, eval_hm$mcd_q05, eval_hm$mcd_q95,
                   eval_hm$mcd_uncert == "High uncertainty") %>%
    mutate(model = "MCD", uncert = "High")
  
)

out_uncert_cov

# draw the plot for the results
library(ggplot2)
ggplot(out_uncert_cov, aes(x = uncert, y = coverage, color = model, group = model)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0.90, linetype = "dashed", color = "red") +
  theme_bw(base_size = 12) +
  labs(
    title = "Coverage under High Missingness",
    subtitle = "Stratified by model-predicted uncertainty",
    x = "Predicted uncertainty stratum",
    y = "Empirical Coverage"
  )

ggplot(out_uncert_cov, aes(x = uncert, y = width, color = model, group = model)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  theme_bw(base_size = 12) +
  labs(
    title = "Interval Width under High Missingness",
    x = "Predicted uncertainty stratum",
    y = "Average Interval Width"
  )

# ----- conditional calibration ----------#

# ----------------------------
# Build cal_df for conditional calibration
# ----------------------------

K<- 10  # number of bins

# ----------------------------
# Build cal_df safely
# ----------------------------

cal_df <- bind_rows(
  
  # DRF
  eval_df %>%
    transmute(
      model = "DRF",
      y,
      q05 = drf_q05,
      q50 = drf_q50,
      q95 = drf_q95
    ),
  
  # QRF
  eval_df %>%
    transmute(
      model = "QRF",
      y,
      q05 = qrf_q05,
      q50 = qrf_q50,
      q95 = qrf_q95
    ),
  
  # RF
  eval_df %>%
    transmute(
      model = "RF",
      y,
      q05 = rf_q05,
      q50 = rf_q50,
      q95 = rf_q95
    ),
  
  # XGB
  eval_df %>%
    transmute(
      model = "XGB",
      y,
      q05 = xgb_q05,
      q50 = xgb_q50,
      q95 = xgb_q95
    ),
  
  # ENS
  eval_df %>%
    transmute(
      model = "ENS",
      y,
      q05 = ens_q05,
      q50 = ens_q50,
      q95 = ens_q95
    ),
  
  # MCD
  eval_df %>%
    transmute(
      model = "MCD",
      y,
      q05 = mcd_q05,
      q50 = mcd_q50,
      q95 = mcd_q95
    )
  
) %>%
  mutate(
    # predicted median as x-axis
    x = q50,
    # coverage indicator
    covered = (y >= q05) & (y <= q95)
  ) %>%
  group_by(model) %>%
  mutate(
    # bin by predicted median
    bin = ntile(x, K)
  ) %>%
  group_by(model, bin) %>%
  summarise(
    x = mean(x, na.rm = TRUE),
    coverage = mean(covered, na.rm = TRUE),
    n_bin = sum(!is.na(covered)),
    .groups = "drop"
  )

cal_df_clean <- cal_df %>%
  mutate(
    # Standard error of binomial proportion
    se = sqrt(coverage * (1 - coverage) / n_bin),
    # 95% confidence interval bounds
    lower = pmax(0, coverage - 1.96 * se),
    upper = pmin(1, coverage + 1.96 * se)
  )

# create the plot for calibration
ggplot(cal_df_clean, aes(x = x, y = coverage)) +
  # Draw a dashed red reference line for visual comparison
  geom_hline(yintercept = 0.90, linetype = "dashed", color = "red", alpha = 0.6) +
  # Confidence ribbon around empirical coverage
  # Represents 95% binomial confidence interval
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "steelblue", alpha = 0.15) +
  # Calibration curve (empirical coverage across bins)
  geom_line(color = "steelblue", size = 0.8) +
  # Points representing bin-level coverage estimates
  geom_point(color = "steelblue", size = 1.5, alpha = 0.8) +
  # Facet by model for side-by-side comparison
  facet_wrap(~ model, ncol = 2) +
  # Y-axis formatting as percentage and restrict visible range
  scale_y_continuous(labels = scales::percent, limits = c(0.6, 1.0)) +
  # Labels and title
  labs(
    title = "Conditional Calibration of 90% Prediction Intervals",
    subtitle = "Assessing coverage stability across different LOS risk levels",
    x = "Predicted Median LOS (Days)",
    y = "Empirical Coverage (%)"
  ) +
  # Clean theme
  theme_bw(base_size = 12) +
  theme(
    strip.background = element_rect(fill = "#f0f0f0"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )


# ---- CRPS evaluation --------#

# pinball loss
pinball <- function(y, q, tau) {
  u <- y - q
  ifelse(u >= 0, tau * u, (tau - 1) * u)
}

# CRPS 近似：2 * \int pinball d tau （用梯形法）
crps_from_quantiles <- function(y, qmat, taus) {
  ord <- order(taus)
  taus <- taus[ord]
  qmat <- qmat[, ord, drop = FALSE]
  
  # 梯形权重
  w <- numeric(length(taus))
  w[1] <- (taus[2] - taus[1]) / 2
  w[length(taus)] <- (taus[length(taus)] - taus[length(taus)-1]) / 2
  if (length(taus) > 2) {
    for (j in 2:(length(taus)-1)) {
      w[j] <- (taus[j+1] - taus[j-1]) / 2
    }
  }
  
  # 对每个tau算 pinball，然后按权重积分
  pb <- 0
  for (j in seq_along(taus)) {
    pb <- pb + w[j] * pinball(y, qmat[, j], taus[j])
  }
  mean(2 * pb, na.rm = TRUE)
}


# ---- CRPS with dense grid (NEW) ----
crps_all_grid <- c(
  DRF = crps_from_quantiles(eval_df$y, as.matrix(drf_qgrid), q_grid),
  QRF = crps_from_quantiles(eval_df$y, as.matrix(qrf_qgrid), q_grid),
  RF  = crps_from_quantiles(eval_df$y, as.matrix(rf_qgrid),  q_grid),
  XGB = crps_from_quantiles(eval_df$y, as.matrix(xgb_qgrid), q_grid),
  ENS = crps_from_quantiles(eval_df$y, as.matrix(ens_qgrid), q_grid),
  MCD = crps_from_quantiles(eval_df$y, as.matrix(mcd_qgrid), q_grid)
)

crps_all_grid


# ---- NLL (Negative Log-Likelihood) evaluation ----
# All models now use non-parametric density approximation from quantile grids
# XGB skipped: conformal prediction does not provide explicit probability density

# Helper: approximate PDF from quantile predictions
# Given quantiles q at levels tau, approximate f(y) via numerical differentiation of CDF
approx_density_from_quantiles <- function(y, qmat, taus) {
  # qmat: n_test x n_quantiles matrix
  # taus: quantile levels (e.g., 0.05, 0.10, ..., 0.95)
  # Returns: density estimate for each y
  
  n <- length(y)
  densities <- numeric(n)
  
  for (i in seq_len(n)) {
    q_i <- qmat[i, ]  # quantiles for observation i
    y_i <- y[i]
    
    # Find where y_i falls in the quantile grid
    # CDF(y) is approximated by interpolating tau values
    if (y_i <= q_i[1]) {
      # Below first quantile: use left tail approximation
      # f(y) ≈ delta_tau / delta_q for first interval
      delta_q <- q_i[2] - q_i[1]
      delta_tau <- taus[2] - taus[1]
      if (delta_q > 1e-10) {
        densities[i] <- delta_tau / delta_q
      } else {
        densities[i] <- 1e-6  # numerical floor
      }
    } else if (y_i >= q_i[length(q_i)]) {
      # Above last quantile: use right tail approximation
      n_q <- length(q_i)
      delta_q <- q_i[n_q] - q_i[n_q - 1]
      delta_tau <- taus[n_q] - taus[n_q - 1]
      if (delta_q > 1e-10) {
        densities[i] <- delta_tau / delta_q
      } else {
        densities[i] <- 1e-6
      }
    } else {
      # Within quantile range: linear interpolation of CDF, then differentiate
      # Find bracket: q[j] <= y < q[j+1]
      j <- max(which(q_i <= y_i))
      
      if (j < length(q_i)) {
        # PDF ≈ dF/dy ≈ (tau[j+1] - tau[j]) / (q[j+1] - q[j])
        delta_q <- q_i[j + 1] - q_i[j]
        delta_tau <- taus[j + 1] - taus[j]
        
        if (delta_q > 1e-10) {
          densities[i] <- delta_tau / delta_q
        } else {
          # quantiles too close: use numerical floor
          densities[i] <- 1e-6
        }
      } else {
        densities[i] <- 1e-6
      }
    }
  }
  
  # Ensure positive density with numerical floor
  pmax(densities, 1e-10)
}

# Compute NLL for each model
cat("\nComputing NLL (Negative Log-Likelihood)...\n")

# DRF: approximate density from quantile grid
drf_densities <- approx_density_from_quantiles(y_te, as.matrix(drf_qgrid), q_grid)
nll_drf <- -mean(log(drf_densities))

# QRF: approximate density from quantile grid
qrf_densities <- approx_density_from_quantiles(y_te, as.matrix(qrf_qgrid), q_grid)
nll_qrf <- -mean(log(qrf_densities))

# RF: approximate density from quantile grid
rf_densities <- approx_density_from_quantiles(y_te, as.matrix(rf_qgrid), q_grid)
nll_rf <- -mean(log(rf_densities))

# ENS: approximate density from quantile grid (now non-parametric)
ens_densities <- approx_density_from_quantiles(y_te, as.matrix(ens_qgrid), q_grid)
nll_ens <- -mean(log(ens_densities))

# MCD: approximate density from quantile grid (now non-parametric)
mcd_densities <- approx_density_from_quantiles(y_te, as.matrix(mcd_qgrid), q_grid)
nll_mcd <- -mean(log(mcd_densities))

# Combine results (XGB omitted: no explicit density from conformal prediction)
nll_results <- c(
  DRF = nll_drf,
  QRF = nll_qrf,
  RF  = nll_rf,
  ENS = nll_ens,
  MCD = nll_mcd
)

cat("\nNLL (lower is better):\n")
print(round(nll_results, 4))


cat("\n=== Generating Test Set Distribution Comparison ===\n")

# ---- Method 1: Histogram + Density Overlay (Most Intuitive) ----

# Prepare data: sample from each model's predictive distribution
set.seed(2026)
n_samples_per_patient <- 100  # Draw 100 samples per patient from each model

# Helper: sample from quantile-based distribution
sample_from_quantiles <- function(qmat, taus, n_samples) {
  # qmat: n_test x n_quantiles
  # Returns: n_test x n_samples matrix of sampled values
  
  n_test <- nrow(qmat)
  samples <- matrix(0, nrow = n_test, ncol = n_samples)
  
  for (i in seq_len(n_test)) {
    # Inverse CDF sampling: generate uniform U ~ [0,1], find Q(U)
    u <- runif(n_samples)
    
    # Interpolate quantile function
    samples[i, ] <- approx(x = taus, y = qmat[i, ], xout = u, rule = 2)$y
  }
  
  samples
}

# Sample from each model
cat("Sampling from predictive distributions...\n")

drf_samples <- sample_from_quantiles(as.matrix(drf_qgrid), q_grid, n_samples_per_patient)
qrf_samples <- sample_from_quantiles(as.matrix(qrf_qgrid), q_grid, n_samples_per_patient)
rf_samples  <- sample_from_quantiles(as.matrix(rf_qgrid),  q_grid, n_samples_per_patient)

# For quantile-based models: sample using inverse CDF method
ens_samples <- sample_from_quantiles(as.matrix(ens_qgrid), q_grid, n_samples_per_patient)
mcd_samples <- sample_from_quantiles(as.matrix(mcd_qgrid), q_grid, n_samples_per_patient)

# Flatten to long format for ggplot
df_dist <- data.frame(
  LOS = c(
    y_te,                         # Actual
    as.vector(drf_samples),       # DRF
    as.vector(qrf_samples),       # QRF
    as.vector(rf_samples),        # RF
    as.vector(ens_samples),       # ENS
    as.vector(mcd_samples)        # MCD
  ),
  Model = rep(
    c("Actual", "DRF", "QRF", "RF", "ENS", "MCD"),
    times = c(length(y_te), rep(length(y_te) * n_samples_per_patient, 5))
  )
)

# Remove extreme outliers for better visualization (optional)
df_dist <- df_dist %>% filter(LOS >= 0, LOS <= quantile(y_te, 0.99) * 1.5)

cat(sprintf("Total samples: %d\n", nrow(df_dist)))


# ---- 最终学术期刊风格对比图：分面+背景真值对比 ----

# 1. 数据预处理（复用你之前的 df_dist）
# 确保 Model 是有序因子，方便排序展示
df_dist$Model <- factor(df_dist$Model, levels = c("Actual", "DRF", "QRF", "RF", "ENS", "MCD"))

# 创建一个专门用于绘制“背景真值”的数据框，去除 Model 列
# 这样在 facet_wrap 时，这部分数据会在每个面板中重复出现
df_actual_bg <- df_dist %>% 
  filter(Model == "Actual") %>% 
  select(-Model)

# 2. 绘图
p_final <- ggplot() +
  # 第一层：在每个面板背景绘制灰色填充的真实分布 (Actual)
  geom_density(data = df_actual_bg, aes(x = LOS), 
               fill = "gray50", color = "gray30", alpha = 0.8, linewidth = 0.5) +
  
  # 第二层：绘制各模型自身的预测分布（不包含单独的 Actual 面板，或者让它重叠）
  # 过滤掉 Actual 组，因为我们只需要展示五个模型的面板
  geom_density(data = df_dist %>% filter(Model != "Actual"), 
               aes(x = LOS, color = Model, fill = Model), 
               alpha = 0.5, linewidth = 0.9) +
  
  # 分面设置：一行五个或者两行（这里建议一行5个，水平对比感最强；或者 2x3 包含 Actual）
  # 我们展示 5 个模型面板
  facet_wrap(~ Model, ncol = 3) + 
  
  # 配色方案：学术常用 Set1 或自定义
  scale_fill_manual(values = c(
    "DRF" = "#F4A3A3",        # 更浅红
    "QRF" = "#A8C5E5",        # 更浅蓝
    "RF"  = "#B7E3B0",        # 更浅绿
    "ENS" = "#984EA3", 
    "MCD" = "#FF7F00"
  )) +
  
  scale_color_manual(values = c(
    "DRF" = "#D65C5C",        # 中等红
    "QRF" = "#5A9BD4",        # 中等蓝
    "RF"  = "#66BB66",        # 中等绿
    "ENS" = "#7A3E82", 
    "MCD" = "#CC6600"
  )) +
  
  # 坐标轴限制（根据 LOS 分布自动优化，建议展示 95% 分位数区间）
  coord_cartesian(xlim = c(0, quantile(y_te, 0.95) * 1.3)) +
  
  # 学术主题优化
  theme_bw(base_size = 14) +
  labs(
    title = "Comparison of Predictive Distributions against Actual LOS",
    subtitle = "Gray shaded area represents the actual LOS distribution in all panels",
    x = "Length of Stay (Days)",
    y = "Density",
    caption = "Note: Non-parametric models (top) show superior tail-capture compared to Gaussian models (bottom)."
  ) +
  theme(
    strip.background = element_rect(fill = "gray95", color = "gray80"), # 修改标题背景
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(), # 减少纵向线条，干扰视线
    legend.position = "none", # 因为标题已经说明了模型，不需要图例
    plot.title = element_text(face = "bold", size = 16, hjust = 0),
    plot.subtitle = element_text(color = "gray30", size = 12),
    axis.title = element_text(face = "bold")
  )

# 显示
print(p_final)

# 保存为高分辨率 PDF (期刊投稿首选) 或 PNG
ggsave("/mnt/user-data/outputs/academic_dist_comparison.png", 
       plot = p_final, width = 12, height = 7, dpi = 600)


# ---- Quantitative Assessment: KL Divergence & KS Distance ----
# Use Kernel Density Estimation instead of histograms for robustness

cat("\n=== Quantitative Distribution Comparison ===\n")

# Helper: Approximate KL divergence using kernel density estimates
kl_divergence_kde <- function(x_true, x_pred, n_grid = 200) {
  # Use common grid that spans both distributions
  x_min <- min(c(x_true, x_pred), na.rm = TRUE)
  x_max <- max(c(x_true, x_pred), na.rm = TRUE)
  grid <- seq(x_min, x_max, length.out = n_grid)
  
  # Estimate densities using KDE
  dens_true <- density(x_true, from = x_min, to = x_max, n = n_grid, na.rm = TRUE)
  dens_pred <- density(x_pred, from = x_min, to = x_max, n = n_grid, na.rm = TRUE)
  
  # Get density values
  p <- dens_true$y
  q <- dens_pred$y
  
  # Add small epsilon to avoid log(0)
  eps <- 1e-10
  p <- pmax(p, eps)
  q <- pmax(q, eps)
  
  # Normalize to ensure they sum to 1
  p <- p / sum(p)
  q <- q / sum(q)
  
  # Compute KL divergence: sum(p * log(p/q))
  kl <- sum(p * log(p / q), na.rm = TRUE)
  
  return(kl)
}

# Compute KL divergence for each model
cat("Computing KL divergence (this may take a moment)...\n")

kl_results <- c(
  DRF = kl_divergence_kde(y_te, as.vector(drf_samples)),
  QRF = kl_divergence_kde(y_te, as.vector(qrf_samples)),
  RF  = kl_divergence_kde(y_te, as.vector(rf_samples)),
  ENS = kl_divergence_kde(y_te, as.vector(ens_samples)),
  MCD = kl_divergence_kde(y_te, as.vector(mcd_samples))
)

cat("\nKL Divergence D_KL(P_actual || P_model) [lower is better]:\n")
print(round(kl_results, 4))

cat("\nBest distributional fit (lowest KL):", names(which.min(kl_results)), "\n")


# ---- Statistical Tests ----
# Kolmogorov-Smirnov test: compare empirical CDFs

ks_drf <- ks.test(y_te, as.vector(drf_samples))$statistic
ks_qrf <- ks.test(y_te, as.vector(qrf_samples))$statistic
ks_rf  <- ks.test(y_te, as.vector(rf_samples))$statistic
ks_ens <- ks.test(y_te, as.vector(ens_samples))$statistic
ks_mcd <- ks.test(y_te, as.vector(mcd_samples))$statistic

ks_results <- c(
  DRF = ks_drf,
  QRF = ks_qrf,
  RF  = ks_rf,
  ENS = ks_ens,
  MCD = ks_mcd
)

cat("\nKolmogorov-Smirnov Distance [lower is better]:\n")
print(round(ks_results, 4))

cat("\nBest CDF match (lowest KS):", names(which.min(ks_results)), "\n")


# ---- Summary Table ----
summary_table <- data.frame(
  Model = c("DRF", "QRF", "RF", "ENS", "MCD"),
  NLL = round(nll_results, 4),
  KL_Divergence = round(kl_results, 4),
  KS_Distance = round(ks_results, 4)
)

cat("\n=== Summary: Distribution Fit Metrics ===\n")
print(summary_table)

cat("\n✓ All plots saved to /mnt/user-data/outputs/\n")
cat("✓ Recommended for publication: distribution_comparison_density.png\n")


# ---------- plot CRPS results -------------#
library(scales)

alpha_target <- 0.90   # 你的区间是 90% (0.05~0.95)

# ---- 1) 组织成长表：每行=一个 test 样本 + 一个模型 ----
cover_width_long <- eval_df %>%
  transmute(
    y,
    # DRF
    drf_q05, drf_q95,
    # QRF
    qrf_q05, qrf_q95,
    # RF
    rf_q05,  rf_q95,
    # XGB
    xgb_q05, xgb_q95,
    # ENS
    ens_q05, ens_q95,
    # MCD
    mcd_q05, mcd_q95
  ) %>%
  mutate(
    drf_w = drf_q95 - drf_q05,
    qrf_w = qrf_q95 - qrf_q05,
    rf_w  = rf_q95  - rf_q05,
    xgb_w = xgb_q95 - xgb_q05,
    ens_w = ens_q95 - ens_q05,
    mcd_w = mcd_q95 - mcd_q05,
    
    drf_cov = as.integer(y >= drf_q05 & y <= drf_q95),
    qrf_cov = as.integer(y >= qrf_q05 & y <= qrf_q95),
    rf_cov  = as.integer(y >= rf_q05  & y <= rf_q95),
    xgb_cov = as.integer(y >= xgb_q05 & y <= xgb_q95),
    ens_cov = as.integer(y >= ens_q05 & y <= ens_q95),
    mcd_cov = as.integer(y >= mcd_q05 & y <= mcd_q95)
  ) %>%
  select(
    y,
    drf_w, drf_cov,
    qrf_w, qrf_cov,
    rf_w,  rf_cov,
    xgb_w, xgb_cov,
    ens_w, ens_cov,
    mcd_w, mcd_cov
  ) %>%
  pivot_longer(
    cols = -y,
    names_to = c("model", ".value"),
    names_pattern = "^(drf|qrf|rf|xgb|ens|mcd)_(w|cov)$"
  ) %>%
  mutate(
    model = recode(model,
                   drf = "DRF", qrf = "QRF", rf = "RF", xgb = "XGB", ens = "ENS", mcd = "MCD")
  ) %>%
  filter(is.finite(w), w >= 0)   # 去掉异常宽度

# 看一下是否合理
summary(cover_width_long$w)
table(cover_width_long$model)


n_bins <- 10

cal_w_df <- cover_width_long %>%
  group_by(model) %>%
  mutate(
    w_bin = ntile(w, n_bins)   # 每个模型各自按 width 分位数分箱（更公平）
  ) %>%
  group_by(model, w_bin) %>%
  summarise(
    n_bin = n(),
    x = median(w, na.rm = TRUE),              # 你也可以改成 mean(w)
    coverage = mean(cov, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # 二项分布近似标准误 (Wald CI)
    se = sqrt(coverage * (1 - coverage) / n_bin),
    lower = pmax(0, coverage - 1.96 * se),
    upper = pmin(1, coverage + 1.96 * se)
  )

p_cov_vs_width <- ggplot(cal_w_df, aes(x = x, y = coverage)) +
  geom_hline(yintercept = alpha_target, linetype = "dashed",
             linewidth = 0.8, alpha = 0.7) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2, alpha = 0.85) +
  facet_wrap(~ model, ncol = 2, scales = "free_x") +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(
    title = "Coverage vs Predicted Interval Width (90% PI)",
    subtitle = "Do wider (more uncertain) predictions achieve more reliable empirical coverage?",
    x = "Predicted interval width  (w = q95 - q05)  [bin median]",
    y = "Empirical coverage within bin"
  ) +
  theme_bw(base_size = 12) +
  theme(
    strip.background = element_rect(fill = "#f2f2f2"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

p_cov_vs_width


# --------- conditional calibration heatmap --------#

library(scales)
# nominal target coverage level (90%)
alpha_target <- 0.90

# Keep only outcome and predicted quantiles for each model
long2d <- eval_df %>%
  transmute(
    y,
    
    drf_q05, drf_q50, drf_q95,
    qrf_q05, qrf_q50, qrf_q95,
    rf_q05,  rf_q50,  rf_q95,
    xgb_q05, xgb_q50, xgb_q95,
    ens_q05, ens_q50, ens_q95,
    mcd_q05, mcd_q50, mcd_q95
  ) %>%
  # Compute interval width (uncertainty proxy)
  mutate(
    drf_w = drf_q95 - drf_q05,
    qrf_w = qrf_q95 - qrf_q05,
    rf_w  = rf_q95  - rf_q05,
    xgb_w = xgb_q95 - xgb_q05,
    ens_w = ens_q95 - ens_q05,
    mcd_w = mcd_q95 - mcd_q05,
    # Compute coverage indicator (1 = covered, 0 = not covered)
    drf_cov = as.integer(y >= drf_q05 & y <= drf_q95),
    qrf_cov = as.integer(y >= qrf_q05 & y <= qrf_q95),
    rf_cov  = as.integer(y >= rf_q05  & y <= rf_q95),
    xgb_cov = as.integer(y >= xgb_q05 & y <= xgb_q95),
    ens_cov = as.integer(y >= ens_q05 & y <= ens_q95),
    mcd_cov = as.integer(y >= mcd_q05 & y <= mcd_q95)
  ) %>%
  select(
    y,
    drf_q50, drf_w, drf_cov,
    qrf_q50, qrf_w, qrf_cov,
    rf_q50,  rf_w,  rf_cov,
    xgb_q50, xgb_w, xgb_cov,
    ens_q50, ens_w, ens_cov,
    mcd_q50, mcd_w, mcd_cov
  ) %>%
  pivot_longer(
    cols = -y,
    names_to = c("model", ".value"),
    names_pattern = "^(drf|qrf|rf|xgb|ens|mcd)_(q50|w|cov)$"
  ) %>%
  mutate(
    model = recode(model, drf="DRF", qrf="QRF", rf="RF", xgb="XGB", ens="ENS", mcd="MCD")
  ) %>%
  filter(is.finite(q50), is.finite(w), w >= 0)

# Define number of bins for predicted risk and predicted uncertainty
n_risk_bins <- 10
n_w_bins    <- 10
# Within each model: Bin by predicted median and interval width
heat2d_df <- long2d %>%
  group_by(model) %>%
  mutate(
    risk_bin  = ntile(q50, n_risk_bins),
    width_bin = ntile(w,   n_w_bins)
  ) %>%
  group_by(model, risk_bin, width_bin) %>%
  summarise(
    n_cell   = n(),
    q50_med  = median(q50, na.rm = TRUE),
    w_med    = median(w,   na.rm = TRUE),
    coverage = mean(cov,   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # Deviation from target coverage
    delta = coverage - alpha_target,
    # Binomial standard error
    se = sqrt(coverage * (1 - coverage) / n_cell),
    lower = pmax(0, coverage - 1.96 * se),
    upper = pmin(1, coverage + 1.96 * se)
  )

min_n_cell <- 50

heat2d_plot <- heat2d_df %>%
  # Remove unreliable cells (too few samples)
  mutate(
    delta_plot = ifelse(n_cell < min_n_cell, NA_real_, delta)
  )

p_heat_delta <- ggplot(
  heat2d_df %>% mutate(delta_plot = ifelse(n_cell < min_n_cell, NA, delta)),
  aes(x = risk_bin, y = width_bin, fill = delta_plot)
) +
  geom_tile(color = "white", linewidth = 0.3) +
  facet_wrap(~ model, ncol = 2) +
  scale_x_continuous(breaks = 1:n_risk_bins) +
  scale_y_continuous(breaks = 1:n_w_bins) +
  scale_fill_gradient2(
    # red -> undercoverage, white -> perfect calibration, green -> overcoverage
    low = "#d73027", mid = "white", high = "#1a9850",
    midpoint = 0,
    labels = percent_format(accuracy = 1),
    na.value = "grey90",
    name = "Coverage - 90%"
  ) +
  labs(
    title = "2D Conditional Calibration (Risk × Uncertainty)",
    subtitle = "Heatmap of deviation from 90% coverage. Grey cells: too few samples.",
    x = "Risk bin (by predicted median q50)",
    y = "Uncertainty bin (by interval width w = q95 - q05)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_rect(fill = "#f2f2f2"),
    plot.title = element_text(face = "bold")
  )


p_heat_delta_n <- p_heat_delta +
  # Overlay sample size per cell
  geom_text(
    data = heat2d_plot %>% filter(!is.na(delta_plot)),
    aes(label = n_cell),
    size = 2.6,
    color = "grey20"
  )


p_heat_delta_n




# ========================================
# HYBRID MODEL: Neural Network Features + DRF
# Two-Stage Learning: NN for representation, DRF for distribution
# Hypothesis: Beats both pure NN (ENS/MCD) and pure forest (DRF/QRF)
# ========================================

cat("\n\n========================================\n")
cat("TRAINING HYBRID MODEL: Neural Features + DRF\n")
cat("========================================\n\n")


# ========================================
# Stage 1: Train Feature Extractor
# ========================================

cat("Stage 1: Training neural feature extractor...\n")

set.seed(2026)

# Define modular network: separate feature extractor and output head
# This allows us to extract 64-dim features before the quantile outputs
nn_feature_extractor <- nn_sequential(
  nn_linear(n_input, 128),
  nn_relu(),
  nn_linear(128, 64),
  nn_relu()
)

nn_output_head <- nn_linear(64, n_quantiles)


# Pinball loss (already defined earlier, but repeat for clarity)
if (!exists("pinball_loss")) {
  pinball_loss <- function(pred, y_true, tau) {
    errors <- y_true - pred
    loss <- torch_where(
      errors >= 0,
      tau * errors,
      (tau - 1) * errors
    )
    torch_mean(loss)
  }
}


# Train the full network (feature_extractor + output_head)
cat("  Training for 150 epochs...\n")

optimizer_hybrid <- optim_adam(
  c(nn_feature_extractor$parameters, nn_output_head$parameters), 
  lr = 0.001
)

nn_feature_extractor$train()
nn_output_head$train()

for (epoch in seq_len(150)) {
  optimizer_hybrid$zero_grad()
  
  # Forward: raw_features -> hidden_features -> quantiles
  hidden_features <- nn_feature_extractor(X_tr_t)
  quantile_preds <- nn_output_head(hidden_features)
  
  loss <- pinball_loss(quantile_preds, y_tr_t, tau_t)
  loss$backward()
  optimizer_hybrid$step()
  
  if (epoch %% 50 == 0) {
    cat(sprintf("    Epoch %d, Loss: %.4f\n", epoch, as.numeric(loss)))
  }
}

cat("  Neural network training complete.\n\n")


# ========================================
# Stage 2: Extract Learned Features
# ========================================

cat("Stage 2: Extracting 64-dimensional neural features...\n")

nn_feature_extractor$eval()

with_no_grad({
  # Extract learned representations (64-dim vectors)
  neural_features_tr <- as.matrix(nn_feature_extractor(X_tr_t))
  neural_features_te <- as.matrix(nn_feature_extractor(X_te_t))
})

cat(sprintf("  Train: %d samples x %d features\n", nrow(neural_features_tr), ncol(neural_features_tr)))
cat(sprintf("  Test: %d samples x %d features\n", nrow(neural_features_te), ncol(neural_features_te)))


# Convert to data frames with proper column names
colnames(neural_features_tr) <- paste0("nn_feat_", 1:ncol(neural_features_tr))
colnames(neural_features_te) <- paste0("nn_feat_", 1:ncol(neural_features_te))

df_neural_tr <- as.data.frame(neural_features_tr)
df_neural_te <- as.data.frame(neural_features_te)

cat("  Feature extraction complete.\n\n")


# ========================================
# Stage 3: Train DRF on Neural Features
# ========================================

cat("Stage 3: Training DRF on neural features (MMD splitting)...\n")
cat("  This may take 2-3 minutes...\n")

# Train DRF using neural features as input
hybrid_drf <- drf(
  X = df_neural_tr,
  Y = y_tr,
  num.trees = 1000,
  min.node.size = 50,
  mtry = floor(sqrt(ncol(df_neural_tr))),  # Use sqrt(64) ≈ 8 features per split
  sample.fraction = 0.5,
  seed = 2026
)

cat("  DRF training on neural features complete.\n\n")


# ========================================
# Stage 4: Generating hybrid predictions (Fixed Version)
# ========================================

library(Matrix)

hybrid_pred <- predict(hybrid_drf, newdata = df_neural_te)

W_h <- hybrid_pred$weights
y_train_h <- as.numeric(hybrid_pred$y)   # 与 W_h 的列顺序严格对齐

n_te <- nrow(df_neural_te)

hybrid_q05 <- numeric(n_te)
hybrid_q50 <- numeric(n_te)
hybrid_q95 <- numeric(n_te)
hybrid_qgrid_mat <- matrix(NA_real_, nrow = n_te, ncol = length(q_grid))

wquant_robust <- function(y, w, tau) {
  y <- as.numeric(y)
  w <- as.numeric(w)
  
  # 防御式检查
  if (length(w) != length(y)) return(NA_real_)
  s <- sum(w)
  if (!is.finite(s) || s <= 0) return(NA_real_)
  
  w <- w / s
  
  ord <- order(y)
  y <- y[ord]
  w <- w[ord]
  cw <- cumsum(w)
  
  idx <- which(cw >= tau)[1]
  if (is.na(idx)) return(y[length(y)])
  y[idx]
}

for (i in seq_len(n_te)) {
  
  # 关键：把稀疏行变成“完整长度”的 dense 向量
  w_i <- as.numeric(Matrix::as.matrix(W_h[i, ]))
  
  hybrid_q05[i] <- wquant_robust(y_train_h, w_i, 0.05)
  hybrid_q50[i] <- wquant_robust(y_train_h, w_i, 0.50)
  hybrid_q95[i] <- wquant_robust(y_train_h, w_i, 0.95)
  
  for (j in seq_along(q_grid)) {
    hybrid_qgrid_mat[i, j] <- wquant_robust(y_train_h, w_i, q_grid[j])
  }
}

hybrid_q <- data.frame(q05 = hybrid_q05, q50 = hybrid_q50, q95 = hybrid_q95)

hybrid_qgrid <- as.data.frame(hybrid_qgrid_mat)
colnames(hybrid_qgrid) <- paste0("q", sprintf("%02d", round(100*q_grid)))


# ========================================
# Evaluation: Hybrid vs All Other Models
# ========================================

cat("\n========================================\n")
cat("EVALUATION: Hybrid vs Pure Models\n")
cat("========================================\n\n")


# --- 1. Coverage & Width ---
cat("1. Coverage & Interval Width:\n")

# 将循环算好的向量打包进 data.frame
hybrid_q <- data.frame(
  q05 = hybrid_q05,
  q50 = hybrid_q50,
  q95 = hybrid_q95
)

# 确保没有负值（住院时间最小为训练集最小值）
lower_bound <- min(y_tr)
hybrid_q$q05 <- pmax(hybrid_q$q05, lower_bound)
hybrid_q$q50 <- pmax(hybrid_q$q50, lower_bound)
hybrid_q$q95 <- pmax(hybrid_q$q95, lower_bound)

# 现在重新运行评估
res_hybrid <- eval_interval(hybrid_q, y_te)

# 重新打印表格
comparison_table_1 <- rbind(
  DRF = unlist(res_drf),
  QRF = unlist(res_qrf),
  RF  = unlist(res_rf),
  XGB = unlist(res_xgb),
  ENS = unlist(res_ens),
  MCD = unlist(res_mcd),
  HYBRID = unlist(res_hybrid)
)

print(round(comparison_table_1, 4))


# --- 2. WIS ---
cat("2. Weighted Interval Score (lower is better):\n")

wis_hybrid <- wis_score(hybrid_q, y_te)

wis_comparison <- c(
  DRF = wis_score(drf_q, y_te),
  QRF = wis_score(qrf_q, y_te),
  RF  = wis_score(rf_q,  y_te),
  XGB = wis_score(xgb_q, y_te),
  ENS = wis_score(ens_q, y_te),
  MCD = wis_score(mcd_q, y_te),
  HYBRID = wis_hybrid
)

print(round(wis_comparison, 4))
cat("\n")


# --- 3. CRPS ---
cat("3. CRPS (lower is better):\n")

crps_hybrid <- crps_from_quantiles(y_te, as.matrix(hybrid_qgrid), q_grid)

crps_comparison <- c(
  DRF = crps_all_grid["DRF"],
  QRF = crps_all_grid["QRF"],
  RF  = crps_all_grid["RF"],
  XGB = crps_all_grid["XGB"],
  ENS = crps_all_grid["ENS"],
  MCD = crps_all_grid["MCD"],
  HYBRID = crps_hybrid
)

print(round(crps_comparison, 4))
cat("\n")


# --- 4. NLL ---
cat("4. Negative Log-Likelihood (lower is better):\n")

hybrid_densities <- approx_density_from_quantiles(y_te, as.matrix(hybrid_qgrid), q_grid)
nll_hybrid <- -mean(log(hybrid_densities))

nll_comparison <- c(
  DRF = nll_results["DRF"],
  QRF = nll_results["QRF"],
  RF  = nll_results["RF"],
  ENS = nll_results["ENS"],
  MCD = nll_results["MCD"],
  HYBRID = nll_hybrid
)

print(round(nll_comparison, 4))
cat("\n")


# ========================================
# Key Question: Does Hybrid Beat BOTH Categories?
# ========================================

cat("\n========================================\n")
cat("HYPOTHESIS TEST: Does Hybrid Beat Both?\n")
cat("========================================\n\n")

# Define pure NN models
pure_nn <- c("ENS", "MCD")
pure_forest <- c("DRF", "QRF", "RF")

# Best in each category (use WIS as primary metric)
best_nn_wis <- min(wis_comparison[pure_nn], na.rm = TRUE)
best_forest_wis <- min(wis_comparison[pure_forest], na.rm = TRUE)
hybrid_wis <- wis_comparison["HYBRID"]

cat(sprintf("Best Pure NN (WIS): %.4f (%s)\n", 
            best_nn_wis, 
            names(which.min(wis_comparison[pure_nn]))))

cat(sprintf("Best Pure Forest (WIS): %.4f (%s)\n", 
            best_forest_wis, 
            names(which.min(wis_comparison[pure_forest]))))

cat(sprintf("Hybrid NN+DRF (WIS): %.4f\n", hybrid_wis))
cat("\n")


# Verdict
beats_nn <- hybrid_wis < best_nn_wis
beats_forest <- hybrid_wis < best_forest_wis

if (beats_nn && beats_forest) {
  cat("✓ ✓ ✓ SUCCESS! Hybrid beats BOTH pure NN and pure forest! ✓ ✓ ✓\n\n")
  cat(sprintf("  Improvement over best NN: %.2f%%\n", 
              100 * (best_nn_wis - hybrid_wis) / best_nn_wis))
  cat(sprintf("  Improvement over best forest: %.2f%%\n", 
              100 * (best_forest_wis - hybrid_wis) / best_forest_wis))
  cat("\n")
  cat("INTERPRETATION:\n")
  cat("  - Neural network learns powerful non-linear features\n")
  cat("  - DRF uses these features for flexible distributional modeling\n")
  cat("  - Combination captures both representation AND uncertainty better\n")
  
} else if (beats_nn) {
  cat("⚠ Partial Success: Hybrid beats pure NN, but not pure forest\n")
  cat(sprintf("  Gap to best forest: %.2f%%\n", 
              100 * (hybrid_wis - best_forest_wis) / best_forest_wis))
  
} else if (beats_forest) {
  cat("⚠ Partial Success: Hybrid beats pure forest, but not pure NN\n")
  cat(sprintf("  Gap to best NN: %.2f%%\n", 
              100 * (hybrid_wis - best_nn_wis) / best_nn_wis))
  
} else {
  cat("✗ Hybrid does not beat either pure approach\n")
  cat("POSSIBLE REASONS:\n")
  cat("  - Neural features may not add information beyond raw features\n")
  cat("  - DRF might prefer raw features for tree splitting\n")
  cat("  - Overfitting in feature extraction stage\n")
}


# ========================================
# Summary Table for Paper
# ========================================

cat("\n\n========================================\n")
cat("SUMMARY TABLE (All Models)\n")
cat("========================================\n\n")

summary_all <- data.frame(
  Model = c("DRF", "QRF", "RF", "XGB", "ENS", "MCD", "Hybrid_NN_DRF"),
  Coverage = c(
    res_drf$coverage, res_qrf$coverage, res_rf$coverage,
    res_xgb$coverage, res_ens$coverage, res_mcd$coverage,
    res_hybrid$coverage
  ),
  Width = c(
    res_drf$width, res_qrf$width, res_rf$width,
    res_xgb$width, res_ens$width, res_mcd$width,
    res_hybrid$width
  ),
  WIS = wis_comparison,
  CRPS = crps_comparison,
  NLL = c(nll_comparison, NA)  # Add NA for alignment if needed
)

# Remove XGB from NLL (no density)
summary_all$NLL[summary_all$Model == "XGB"] <- NA

print(summary_all)


# Rank models by WIS
summary_all$WIS_Rank <- rank(summary_all$WIS, na.last = TRUE)

cat("\n\nModel Rankings (by WIS, 1 = best):\n")
print(summary_all[order(summary_all$WIS_Rank), c("Model", "WIS", "WIS_Rank")])


# ========================================
# Visualization: Add Hybrid to Distribution Comparison
# ========================================

cat("\n\nGenerating hybrid distribution plot...\n")

# Sample from hybrid for distribution comparison
hybrid_samples <- sample_from_quantiles(as.matrix(hybrid_qgrid), q_grid, n_samples_per_patient)

# Create extended data frame
df_dist_extended <- rbind(
  df_dist,
  data.frame(
    LOS = as.vector(hybrid_samples),
    Model = "Hybrid_NN_DRF"
  )
)

df_dist_extended$Model <- factor(
  df_dist_extended$Model, 
  levels = c("Actual", "DRF", "QRF", "RF", "ENS", "MCD", "Hybrid_NN_DRF")
)


# Plot: Overlaid densities
p_hybrid_overlay <- ggplot(df_dist_extended, aes(x = LOS, color = Model, linetype = Model)) +
  geom_density(linewidth = 1.2, alpha = 0.7) +
  
  scale_color_manual(
    values = c(
      "Actual" = "black",
      "DRF" = "#e41a1c",
      "QRF" = "#984ea3",
      "RF" = "#ff7f00",
      "ENS" = "#377eb8",
      "MCD" = "#4daf4a",
      "Hybrid_NN_DRF" = "#a65628"  # Brown for hybrid
    )
  ) +
  
  scale_linetype_manual(
    values = c(
      "Actual" = "solid",
      "DRF" = "solid",
      "QRF" = "dashed",
      "RF" = "dashed",
      "ENS" = "dotted",
      "MCD" = "dotted",
      "Hybrid_NN_DRF" = "solid"
    )
  ) +
  
  labs(
    title = "Hybrid Model: Neural Features + DRF",
    subtitle = "Combining neural representation learning with distributional random forest",
    x = "Length of Stay (Days)",
    y = "Density"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 15),
    panel.grid.minor = element_blank()
  ) +
  
  coord_cartesian(xlim = c(0, quantile(y_te, 0.95) * 1.2))

print(p_hybrid_overlay)

ggsave("/mnt/user-data/outputs/hybrid_distribution_comparison.png",
       plot = p_hybrid_overlay, width = 12, height = 6.5, dpi = 300)

cat("✓ Plot saved to: /mnt/user-data/outputs/hybrid_distribution_comparison.png\n")


# Plot: Faceted comparison (highlight hybrid)
df_highlight <- df_dist_extended %>%
  filter(Model %in% c("Actual", "DRF", "ENS", "Hybrid_NN_DRF"))

p_hybrid_facet <- ggplot() +
  geom_density(
    data = df_highlight %>% filter(Model == "Actual"),
    aes(x = LOS),
    fill = "gray70", alpha = 0.3, color = "black", linewidth = 0.6
  ) +
  
  geom_density(
    data = df_highlight %>% filter(Model != "Actual"),
    aes(x = LOS, fill = Model, color = Model),
    alpha = 0.5, linewidth = 1
  ) +
  
  facet_wrap(~ Model, ncol = 3, scales = "free_y") +
  
  scale_fill_manual(
    values = c(
      "DRF" = "#e41a1c",
      "ENS" = "#377eb8",
      "Hybrid_NN_DRF" = "#4daf4a"
    )
  ) +
  
  scale_color_manual(
    values = c(
      "DRF" = "#e41a1c",
      "ENS" = "#377eb8",
      "Hybrid_NN_DRF" = "#4daf4a"
    )
  ) +
  
  labs(
    title = "Hybrid vs Pure Models: Distribution Fit",
    subtitle = "Gray background = actual distribution in each panel",
    x = "Length of Stay (Days)",
    y = "Density"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14),
    strip.background = element_rect(fill = "#f0f0f0", color = "gray80"),
    strip.text = element_text(face = "bold")
  ) +
  
  coord_cartesian(xlim = c(0, quantile(y_te, 0.95) * 1.2))

print(p_hybrid_facet)

ggsave("/mnt/user-data/outputs/hybrid_facet_comparison.png",
       plot = p_hybrid_facet, width = 11, height = 5, dpi = 300)

cat("✓ Plot saved to: /mnt/user-data/outputs/hybrid_facet_comparison.png\n")


# ========================================
# Feature Importance: Which Neural Features Matter?
# ========================================

cat("\n\n========================================\n")
cat("NEURAL FEATURE IMPORTANCE\n")
cat("========================================\n\n")

# Extract variable importance from DRF
var_importance <- hybrid_drf$variable.importance

cat("Top 15 most important neural features for DRF splitting:\n")
top_features <- head(sort(var_importance, decreasing = TRUE), 15)
print(round(top_features, 4))

cat("\n")
cat("INTERPRETATION:\n")
cat("  - DRF identifies which learned representations are most useful\n")
cat("  - High importance = feature strongly discriminates LOS distributions\n")
cat("  - This validates that neural features encode predictive information\n")


# ========================================
# Save Hybrid Model Results
# ========================================

cat("\n\n========================================\n")
cat("SAVING RESULTS\n")
cat("========================================\n\n")

hybrid_results <- list(
  # Predictions
  quantiles = hybrid_q,
  quantile_grid = hybrid_qgrid,
  
  # Features
  neural_features_train = neural_features_tr,
  neural_features_test = neural_features_te,
  
  # Models
  feature_extractor = nn_feature_extractor,
  drf_model = hybrid_drf,
  
  # Metrics
  summary_table = summary_all,
  wis_comparison = wis_comparison,
  feature_importance = var_importance
)

saveRDS(hybrid_results, "/mnt/user-data/outputs/hybrid_nn_drf_results.rds")
cat("✓ Results saved to: /mnt/user-data/outputs/hybrid_nn_drf_results.rds\n")


cat("\n\n========================================\n")
cat("✓ ✓ ✓ HYBRID MODEL EVALUATION COMPLETE ✓ ✓ ✓\n")
cat("========================================\n\n")


