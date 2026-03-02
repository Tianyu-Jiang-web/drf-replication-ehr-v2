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
  predict(qrf_fit, Xte_all, what = qs)
)
colnames(qrf_q) <- c("q05","q50","q95")

# ---- QRF quantile grid predictions ----
qrf_qgrid <- as.data.frame(predict(qrf_fit, Xte_all, what = q_grid))
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
  predict(rf_q_fit, Xte_all, type = "quantiles", quantiles = qs)$predictions
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

# --- Build final DMatrices with proper train/cal/test split ---
set.seed(2026)
n_tr_full    <- nrow(Xtr_all_mm_hp)
cal_size_fin <- floor(0.15 * n_tr_full)
cal_idx_fin  <- sample(seq_len(n_tr_full), size = cal_size_fin)
train_idx_fin <- setdiff(seq_len(n_tr_full), cal_idx_fin)

dtrain <- xgb.DMatrix(Xtr_all_mm_hp[train_idx_fin, , drop = FALSE],
                      label = y_tr[train_idx_fin])
dcal   <- xgb.DMatrix(Xtr_all_mm_hp[cal_idx_fin,   , drop = FALSE],
                      label = y_tr[cal_idx_fin])
dtest  <- xgb.DMatrix(Xte_all_mm)
y_cal  <- y_tr[cal_idx_fin]

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

# ============================================================
# SHARED SETUP FOR ALL NEURAL MODELS
# ============================================================
library(torch)
library(scoringutils)

set.seed(2026)
qs          <- c(0.05, 0.5, 0.95)
q05_idx     <- which.min(abs(q_grid - 0.05))
q50_idx     <- which.min(abs(q_grid - 0.50))
q95_idx     <- which.min(abs(q_grid - 0.95))

# One-hot encode + standardise X  (train-led, applied to test)
Xtr_ens_mm  <- model.matrix(~ . - 1, data = Xtr_all)
Xte_ens_mm  <- model.matrix(~ . - 1, data = Xte_all)
# align columns
common_nn   <- union(colnames(Xtr_ens_mm), colnames(Xte_ens_mm))
Xtr_ens_mm  <- Xtr_ens_mm[, common_nn, drop = FALSE]; Xtr_ens_mm[is.na(Xtr_ens_mm)] <- 0
Xte_ens_mm  <- Xte_ens_mm[, common_nn, drop = FALSE]; Xte_ens_mm[is.na(Xte_ens_mm)] <- 0

x_mean      <- colMeans(Xtr_ens_mm)
x_sd        <- apply(Xtr_ens_mm, 2, sd); x_sd[x_sd == 0] <- 1
Xtr_ens_sc  <- scale(Xtr_ens_mm, center = x_mean, scale = x_sd)
Xte_ens_sc  <- scale(Xte_ens_mm, center = x_mean, scale = x_sd)

n_input     <- ncol(Xtr_ens_sc)
n_quantiles <- length(q_grid)

# Device
device <- torch_device(if (torch::backends_mps_is_available()) "mps" else "cpu")
cat("Neural models using device:", device$type, "\n")

# Shared CV fold
val_idx_nn   <- cv_folds_hp[[1]]
train_idx_nn <- setdiff(seq_len(nrow(Xtr_ens_sc)), val_idx_nn)
X_tr_cv      <- Xtr_ens_sc[train_idx_nn, , drop = FALSE]
y_tr_cv      <- y_tr[train_idx_nn]
X_va_cv      <- Xtr_ens_sc[val_idx_nn,   , drop = FALSE]
y_va_cv      <- y_tr[val_idx_nn]

# ============================================================
# SHARED: Gamma NLL loss (log-space params -> shape, rate)
# ============================================================
gamma_nll_loss <- function(params, y_true) {
  # params: [B, 2]  col1=log_shape, col2=log_rate
  shape <- torch_exp(params[, 1, drop = FALSE])
  rate  <- torch_exp(params[, 2, drop = FALSE])
  nll   <- -(
    (shape - 1) * torch_log(y_true + 1e-8) -
      rate  * y_true +
      shape * torch_log(rate + 1e-8) -
      torch_lgamma(shape)
  )
  torch_mean(nll)
}

# ============================================================
# SHARED: Dataset / DataLoader builder
# ============================================================
make_dl <- function(X_sc, y_obs, batch_size = 512) {
  ds_def <- dataset(
    name       = "gamma_ds",
    initialize = function(X, y) { self$X <- X; self$y <- y },
    .getitem   = function(i)    list(x = self$X[i, ], y = self$y[i]),
    .length    = function()     nrow(self$X)
  )
  X_t <- torch_tensor(X_sc,                    dtype = torch_float(), device = device)
  y_t <- torch_tensor(matrix(y_obs, ncol = 1), dtype = torch_float(), device = device)
  dataloader(ds_def(X_t, y_t), batch_size = batch_size, shuffle = TRUE)
}

# ============================================================
# SHARED: extract Gamma params from net  ->  list(shape, rate)
# ============================================================
get_gamma_params <- function(net, X_sc) {
  Xv     <- torch_tensor(X_sc, dtype = torch_float(), device = device)
  params <- with_no_grad({ net(Xv) })
  pm     <- as.matrix(params$to(device = "cpu"))
  list(shape = exp(pm[, 1]), rate = exp(pm[, 2]))
}

# ============================================================
# SHARED: quantile grid from Gamma params
# ============================================================
gamma_qgrid <- function(shape, rate, taus = q_grid) {
  n <- length(shape); K <- length(taus)
  out <- matrix(NA_real_, n, K)
  for (k in seq_len(K)) out[, k] <- qgamma(taus[k], shape = shape, rate = rate)
  out
}

# WIS helper
gamma_wis <- function(net, X_sc, y_obs) {
  p    <- get_gamma_params(net, X_sc)
  pmat <- gamma_qgrid(p$shape, p$rate, qs)
  mean(scoringutils::wis(y_obs, pmat, qs, na.rm = TRUE), na.rm = TRUE)
}

# ============================================================
# MODEL 6: Deep Ensemble (Gamma)
# M=5 independent DNNs, each trained with Gamma NLL.
# Prediction = mixture of M Gamma distributions (avg CDF → invert).
# ============================================================
cat("\n========================================\n")
cat("MODEL 6: Deep Ensemble (Gamma DNN)\n")
cat("========================================\n")

# --- architecture ---
make_gamma_dnn <- function(n_in, h1, h2) {
  nn_sequential(
    nn_linear(n_in, h1), nn_relu(),
    nn_linear(h1,   h2), nn_relu(),
    nn_linear(h2,    2)           # log_shape, log_rate
  )
}

train_gamma_dnn <- function(X_sc, y_obs, h1, h2, lr, epochs = 60, batch_size = 512) {
  net   <- make_gamma_dnn(n_input, h1, h2)$to(device = device)
  optim <- optim_adam(net$parameters, lr = lr)
  dl    <- make_dl(X_sc, y_obs, batch_size)
  net$train()
  for (ep in seq_len(epochs)) {
    coro::loop(for (b in dl) {
      optim$zero_grad()
      loss <- gamma_nll_loss(net(b$x), b$y)
      loss$backward(); optim$step()
    })
  }
  net$eval(); net
}

# HP tuning
cat("Tuning ENS-Gamma hyperparameters...\n")
ens_grid <- expand.grid(h1 = c(64, 128), h2 = c(32, 64), lr = c(5e-4, 1e-3))

ens_cv <- sapply(seq_len(nrow(ens_grid)), function(i) {
  hp <- ens_grid[i, ]
  cat(sprintf("  [%d/%d] h1=%d h2=%d lr=%.4g ...", i, nrow(ens_grid), hp$h1, hp$h2, hp$lr))
  net <- train_gamma_dnn(X_tr_cv, y_tr_cv, hp$h1, hp$h2, hp$lr, epochs = 60)
  w   <- gamma_wis(net, X_va_cv, y_va_cv)
  cat(sprintf(" WIS=%.4f\n", w)); w
})

best_ens_hp  <- as.list(ens_grid[which.min(ens_cv), ])
cat(sprintf("✓ Best ENS-Gamma: h1=%d h2=%d lr=%.4g (WIS=%.4f)\n",
            best_ens_hp$h1, best_ens_hp$h2, best_ens_hp$lr, min(ens_cv)))

# Train M members
M_ens         <- 5
ens_members   <- vector("list", M_ens)
cat(sprintf("Training %d ENS-Gamma members (150 epochs each)...\n", M_ens))
for (m in seq_len(M_ens)) {
  set.seed(2026 + m)
  cat(sprintf("  Member %d/%d\n", m, M_ens))
  ens_members[[m]] <- train_gamma_dnn(Xtr_ens_sc, y_tr,
                                      best_ens_hp$h1, best_ens_hp$h2, best_ens_hp$lr,
                                      epochs = 150)
}

# Predict: mixture of M Gamma CDFs (averaged per patient)
cat("Computing ENS-Gamma predictions (mixture CDF inversion)...\n")

ens_shapes <- lapply(ens_members, function(net) get_gamma_params(net, Xte_ens_sc)$shape)
ens_rates  <- lapply(ens_members, function(net) get_gamma_params(net, Xte_ens_sc)$rate)

n_te_ens  <- nrow(Xte_ens_sc)
n_cdf_pts <- 800  # resolution for CDF inversion
ens_qgrid_mat <- matrix(NA_real_, n_te_ens, length(q_grid))

for (i in seq_len(n_te_ens)) {
  # per-patient range
  ql <- min(sapply(seq_len(M_ens), function(m) qgamma(0.001, ens_shapes[[m]][i], ens_rates[[m]][i])))
  qh <- max(sapply(seq_len(M_ens), function(m) qgamma(0.999, ens_shapes[[m]][i], ens_rates[[m]][i])))
  xg <- seq(max(ql, 1e-6), qh, length.out = n_cdf_pts)
  # mixture CDF = average over members
  mix_cdf <- rowMeans(sapply(seq_len(M_ens), function(m)
    pgamma(xg, shape = ens_shapes[[m]][i], rate = ens_rates[[m]][i])))
  # invert by first-crossing
  for (k in seq_along(q_grid)) {
    idx <- which(mix_cdf >= q_grid[k])[1]
    ens_qgrid_mat[i, k] <- if (is.na(idx)) xg[n_cdf_pts] else xg[idx]
  }
}

ens_q     <- data.frame(q05 = ens_qgrid_mat[, q05_idx],
                        q50 = ens_qgrid_mat[, q50_idx],
                        q95 = ens_qgrid_mat[, q95_idx])
ens_qgrid <- as.data.frame(ens_qgrid_mat)
colnames(ens_qgrid) <- paste0("q", sprintf("%02d", round(100 * q_grid)))

# NLL: log-mixture density = log(mean(exp(log_dens_m)))
ens_log_dens_list <- lapply(seq_len(M_ens), function(m)
  dgamma(y_te, shape = ens_shapes[[m]], rate = ens_rates[[m]], log = TRUE))
nll_ens <- -mean(log(rowMeans(exp(do.call(cbind, ens_log_dens_list)) + 1e-300)))

cat("ENS-Gamma complete.\n\n")


# ============================================================
# MODEL 7: MC Dropout (Gamma)
# Single DNN with dropout; at test time keep dropout ON and
# average shape/rate over M forward passes → one Gamma per patient.
# ============================================================
cat("========================================\n")
cat("MODEL 7: MC Dropout (Gamma DNN)\n")
cat("========================================\n")

make_gamma_dropout_dnn <- function(n_in, h1 = 128, h2 = 64, p_drop = 0.1) {
  nn_sequential(
    nn_linear(n_in, h1), nn_relu(), nn_dropout(p_drop),
    nn_linear(h1,   h2), nn_relu(), nn_dropout(p_drop),
    nn_linear(h2,    2)
  )
}

train_gamma_dropout_dnn <- function(X_sc, y_obs, p_drop, lr,
                                    h1 = 128, h2 = 64, epochs = 60, batch_size = 512) {
  net   <- make_gamma_dropout_dnn(n_input, h1, h2, p_drop)$to(device = device)
  optim <- optim_adam(net$parameters, lr = lr)
  dl    <- make_dl(X_sc, y_obs, batch_size)
  net$train()
  for (ep in seq_len(epochs)) {
    coro::loop(for (b in dl) {
      optim$zero_grad()
      loss <- gamma_nll_loss(net(b$x), b$y)
      loss$backward(); optim$step()
    })
  }
  net   # leave in train() mode so dropout stays active at inference
}

mcd_avg_params <- function(net, X_sc, M_pass = 50) {
  net$train()  # keep dropout ON
  X_t <- torch_tensor(X_sc, dtype = torch_float(), device = device)
  acc_s <- acc_r <- matrix(0, nrow(X_sc), 1)
  with_no_grad({
    for (p in seq_len(M_pass)) {
      pm   <- as.matrix(net(X_t)$to(device = "cpu"))
      acc_s <- acc_s + exp(pm[, 1, drop = FALSE])
      acc_r <- acc_r + exp(pm[, 2, drop = FALSE])
    }
  })
  list(shape = as.vector(acc_s / M_pass), rate = as.vector(acc_r / M_pass))
}

# HP tuning
cat("Tuning MCD-Gamma hyperparameters...\n")
mcd_grid <- expand.grid(p_drop = c(0.05, 0.10, 0.20), lr = c(5e-4, 1e-3))

mcd_cv <- sapply(seq_len(nrow(mcd_grid)), function(i) {
  hp  <- mcd_grid[i, ]
  cat(sprintf("  [%d/%d] dropout=%.2f lr=%.4g ...", i, nrow(mcd_grid), hp$p_drop, hp$lr))
  net <- train_gamma_dropout_dnn(X_tr_cv, y_tr_cv, hp$p_drop, hp$lr, epochs = 60)
  p   <- mcd_avg_params(net, X_va_cv, M_pass = 25)
  pm  <- gamma_qgrid(p$shape, p$rate, qs)
  w   <- mean(scoringutils::wis(y_va_cv, pm, qs, na.rm = TRUE), na.rm = TRUE)
  cat(sprintf(" WIS=%.4f\n", w)); w
})

best_mcd_hp <- as.list(mcd_grid[which.min(mcd_cv), ])
cat(sprintf("✓ Best MCD-Gamma: dropout=%.2f lr=%.4g (WIS=%.4f)\n",
            best_mcd_hp$p_drop, best_mcd_hp$lr, min(mcd_cv)))

set.seed(2026)
cat("Training final MCD-Gamma model (150 epochs)...\n")
mc_dropout_net <- train_gamma_dropout_dnn(Xtr_ens_sc, y_tr,
                                          best_mcd_hp$p_drop, best_mcd_hp$lr,
                                          epochs = 150)

M_dropout     <- 100
cat(sprintf("Generating %d MC passes on test set...\n", M_dropout))
mcd_params_te <- mcd_avg_params(mc_dropout_net, Xte_ens_sc, M_pass = M_dropout)

mcd_qgrid_mat <- gamma_qgrid(mcd_params_te$shape, mcd_params_te$rate)
mcd_q         <- data.frame(q05 = mcd_qgrid_mat[, q05_idx],
                            q50 = mcd_qgrid_mat[, q50_idx],
                            q95 = mcd_qgrid_mat[, q95_idx])
mcd_qgrid     <- as.data.frame(mcd_qgrid_mat)
colnames(mcd_qgrid) <- paste0("q", sprintf("%02d", round(100 * q_grid)))

nll_mcd <- -mean(dgamma(y_te, shape = mcd_params_te$shape,
                        rate = mcd_params_te$rate, log = TRUE))
cat("MCD-Gamma complete.\n\n")


# ============================================================
# MODEL 8: DDNN — Distributional DNN (Gamma, single model)
# Single deterministic DNN, Gamma NLL, more epochs.
# Captures aleatoric uncertainty only (no epistemic).
# ============================================================
cat("========================================\n")
cat("MODEL 8: DDNN — Distributional DNN (Gamma)\n")
cat("========================================\n")

cat("Tuning DDNN hyperparameters...\n")
ddnn_grid <- expand.grid(h1 = c(64, 128), h2 = c(32, 64), lr = c(5e-4, 1e-3))

ddnn_cv <- sapply(seq_len(nrow(ddnn_grid)), function(i) {
  hp  <- ddnn_grid[i, ]
  cat(sprintf("  [%d/%d] h1=%d h2=%d lr=%.4g ...", i, nrow(ddnn_grid), hp$h1, hp$h2, hp$lr))
  net <- train_gamma_dnn(X_tr_cv, y_tr_cv, hp$h1, hp$h2, hp$lr, epochs = 60)
  w   <- gamma_wis(net, X_va_cv, y_va_cv)
  cat(sprintf(" WIS=%.4f\n", w)); w
})

best_ddnn_hp <- as.list(ddnn_grid[which.min(ddnn_cv), ])
cat(sprintf("✓ Best DDNN: h1=%d h2=%d lr=%.4g (WIS=%.4f)\n",
            best_ddnn_hp$h1, best_ddnn_hp$h2, best_ddnn_hp$lr, min(ddnn_cv)))

set.seed(2026 + 50)
cat("Training final DDNN (200 epochs)...\n")
ddnn_net <- train_gamma_dnn(Xtr_ens_sc, y_tr,
                            best_ddnn_hp$h1, best_ddnn_hp$h2, best_ddnn_hp$lr,
                            epochs = 200)

ddnn_params   <- get_gamma_params(ddnn_net, Xte_ens_sc)
ddnn_qgrid_mat <- gamma_qgrid(ddnn_params$shape, ddnn_params$rate)
ddnn_q        <- data.frame(q05 = ddnn_qgrid_mat[, q05_idx],
                            q50 = ddnn_qgrid_mat[, q50_idx],
                            q95 = ddnn_qgrid_mat[, q95_idx])
ddnn_qgrid    <- as.data.frame(ddnn_qgrid_mat)
colnames(ddnn_qgrid) <- paste0("q", sprintf("%02d", round(100 * q_grid)))

nll_ddnn <- -mean(dgamma(y_te, shape = ddnn_params$shape,
                         rate = ddnn_params$rate, log = TRUE))
cat("DDNN complete.\n\n")


# ============================================================
# MODEL 9: Ensemble DRF
# B independent DRFs trained on bootstrap samples.
# Prediction = average of per-member quantile grids.
# ============================================================
cat("========================================\n")
cat("MODEL 9: Ensemble DRF\n")
cat("========================================\n")

B_edrf      <- 5
edrf_members <- vector("list", B_edrf)
cat(sprintf("Training %d DRF members on bootstrap samples...\n", B_edrf))

for (b in seq_len(B_edrf)) {
  set.seed(2026 + b * 100)
  boot_idx <- sample(nrow(Xtr_tree), replace = TRUE)
  cat(sprintf("  Member %d/%d (n=%d)...\n", b, B_edrf, length(boot_idx)))
  edrf_members[[b]] <- drf(
    X               = Xtr_tree[boot_idx, ],
    Y               = y_tr[boot_idx],
    num.trees       = num_trees_drf,
    min.node.size   = min_node_size_drf,
    mtry            = floor(sqrt(ncol(Xtr_tree))),
    sample.fraction = 0.5,
    seed            = 2026 + b
  )
}

cat("Averaging quantile grids from all Ensemble-DRF members...\n")
edrf_qgrid_list <- lapply(edrf_members, function(fit) {
  pr <- predict(fit, Xte_tree)
  wquant_matrix(as.numeric(pr$y), pr$weights, q_grid)
})

edrf_qgrid_mat <- Reduce(`+`, edrf_qgrid_list) / B_edrf
edrf_q         <- data.frame(q05 = edrf_qgrid_mat[, q05_idx],
                             q50 = edrf_qgrid_mat[, q50_idx],
                             q95 = edrf_qgrid_mat[, q95_idx])
edrf_qgrid     <- as.data.frame(edrf_qgrid_mat)
colnames(edrf_qgrid) <- paste0("q", sprintf("%02d", round(100 * q_grid)))
cat("Ensemble-DRF complete.\n\n")


# ============================================================
# MODEL 10: BNN — Bayesian Neural Network (Gamma, Mean-Field VI)
# Uses Bayes-by-Backprop (local reparameterisation).
# Prior: N(0, sigma^2_prior) on each weight.
# Loss = Gamma NLL + KL(q||p) / N.
# Inference: M stochastic forward passes -> average shape/rate.
# ============================================================
cat("========================================\n")
cat("MODEL 10: BNN (Gamma, Mean-Field VI)\n")
cat("========================================\n")

# --- Bayesian linear layer ---
bnn_linear <- nn_module(
  classname = "bnn_linear",
  initialize = function(in_f, out_f, prior_sigma = 1.0) {
    self$prior_sigma <- prior_sigma
    self$w_mu  <- nn_parameter(torch_randn(out_f, in_f)  * 0.01)
    self$w_rho <- nn_parameter(torch_full(c(out_f, in_f), -3.0))
    self$b_mu  <- nn_parameter(torch_zeros(out_f))
    self$b_rho <- nn_parameter(torch_full(c(out_f),       -3.0))
  },
  forward = function(x) {
    w_sig <- torch_log1p(torch_exp(self$w_rho))
    b_sig <- torch_log1p(torch_exp(self$b_rho))
    w     <- self$w_mu + w_sig * torch_randn_like(w_sig)
    b     <- self$b_mu + b_sig * torch_randn_like(b_sig)
    nnf_linear(x, w, b)
  },
  kl = function() {
    ps   <- self$prior_sigma
    w_s  <- torch_log1p(torch_exp(self$w_rho))
    b_s  <- torch_log1p(torch_exp(self$b_rho))
    kl_w <- 0.5 * torch_sum((self$w_mu^2 + w_s^2) / ps^2 - 1 - torch_log(w_s^2 / ps^2))
    kl_b <- 0.5 * torch_sum((self$b_mu^2 + b_s^2) / ps^2 - 1 - torch_log(b_s^2 / ps^2))
    kl_w + kl_b
  }
)

bnn_gamma_net <- nn_module(
  classname = "bnn_gamma",
  initialize = function(n_in, h1, h2, prior_sigma = 1.0) {
    self$l1 <- bnn_linear(n_in, h1, prior_sigma)
    self$l2 <- bnn_linear(h1,   h2, prior_sigma)
    self$l3 <- bnn_linear(h2,    2, prior_sigma)
  },
  forward = function(x) {
    x <- nnf_relu(self$l1(x))
    x <- nnf_relu(self$l2(x))
    self$l3(x)
  },
  kl = function() { self$l1$kl() + self$l2$kl() + self$l3$kl() }
)

train_bnn_gamma <- function(X_sc, y_obs, h1, h2, lr,
                            epochs = 60, batch_size = 512, prior_sigma = 1.0) {
  N     <- nrow(X_sc)
  net   <- bnn_gamma_net(n_input, h1, h2, prior_sigma)$to(device = device)
  optim <- optim_adam(net$parameters, lr = lr)
  dl    <- make_dl(X_sc, y_obs, batch_size)
  net$train()
  for (ep in seq_len(epochs)) {
    coro::loop(for (b in dl) {
      optim$zero_grad()
      nll  <- gamma_nll_loss(net(b$x), b$y)
      kl   <- net$kl() / N
      loss <- nll + kl
      loss$backward(); optim$step()
    })
  }
  net
}

bnn_avg_params <- function(net, X_sc, M_pass = 50) {
  net$train()  # keep stochasticity ON
  X_t   <- torch_tensor(X_sc, dtype = torch_float(), device = device)
  acc_s <- acc_r <- matrix(0, nrow(X_sc), 1)
  with_no_grad({
    for (p in seq_len(M_pass)) {
      pm    <- as.matrix(net(X_t)$to(device = "cpu"))
      acc_s <- acc_s + exp(pm[, 1, drop = FALSE])
      acc_r <- acc_r + exp(pm[, 2, drop = FALSE])
    }
  })
  list(shape = as.vector(acc_s / M_pass), rate = as.vector(acc_r / M_pass))
}

# HP tuning
cat("Tuning BNN hyperparameters...\n")
bnn_grid <- expand.grid(h1 = c(64, 128), h2 = c(32, 64), lr = c(5e-4, 1e-3))

bnn_cv <- sapply(seq_len(nrow(bnn_grid)), function(i) {
  hp  <- bnn_grid[i, ]
  cat(sprintf("  [%d/%d] h1=%d h2=%d lr=%.4g ...", i, nrow(bnn_grid), hp$h1, hp$h2, hp$lr))
  net <- train_bnn_gamma(X_tr_cv, y_tr_cv, hp$h1, hp$h2, hp$lr, epochs = 60)
  p   <- bnn_avg_params(net, X_va_cv, M_pass = 25)
  pm  <- gamma_qgrid(p$shape, p$rate, qs)
  w   <- mean(scoringutils::wis(y_va_cv, pm, qs, na.rm = TRUE), na.rm = TRUE)
  cat(sprintf(" WIS=%.4f\n", w)); w
})

best_bnn_hp <- as.list(bnn_grid[which.min(bnn_cv), ])
cat(sprintf("✓ Best BNN: h1=%d h2=%d lr=%.4g (WIS=%.4f)\n",
            best_bnn_hp$h1, best_bnn_hp$h2, best_bnn_hp$lr, min(bnn_cv)))

set.seed(2026 + 77)
cat("Training final BNN (150 epochs)...\n")
bnn_net <- train_bnn_gamma(Xtr_ens_sc, y_tr,
                           best_bnn_hp$h1, best_bnn_hp$h2, best_bnn_hp$lr,
                           epochs = 150)

bnn_params_te  <- bnn_avg_params(bnn_net, Xte_ens_sc, M_pass = 100)
bnn_qgrid_mat  <- gamma_qgrid(bnn_params_te$shape, bnn_params_te$rate)
bnn_q          <- data.frame(q05 = bnn_qgrid_mat[, q05_idx],
                             q50 = bnn_qgrid_mat[, q50_idx],
                             q95 = bnn_qgrid_mat[, q95_idx])
bnn_qgrid      <- as.data.frame(bnn_qgrid_mat)
colnames(bnn_qgrid) <- paste0("q", sprintf("%02d", round(100 * q_grid)))

nll_bnn <- -mean(dgamma(y_te, shape = bnn_params_te$shape,
                        rate = bnn_params_te$rate, log = TRUE))
cat("BNN complete.\n\n")


# ============================================================
# MODEL 11: Hybrid NN + DRF
# Stage 1: QRNN (pinball loss) trains a 128→64 feature extractor.
# Stage 2: 64-dim features fed into DRF.
# Prediction: DRF weighted quantiles on neural features.
# ============================================================
cat("========================================\n")
cat("MODEL 11: Hybrid NN + DRF\n")
cat("========================================\n")

# --- Stage 1: train QRNN feature extractor (mini-batch, pinball) ---
cat("Stage 1: Training neural feature extractor (QRNN, 150 epochs)...\n")
set.seed(2026)

y_mean_ens  <- mean(y_tr);  y_sd_ens <- sd(y_tr)
y_tr_sc     <- (y_tr - y_mean_ens) / y_sd_ens
tau_t       <- torch_tensor(matrix(q_grid, nrow = 1), dtype = torch_float())

pinball_loss <- function(pred, y_true, tau) {
  err  <- y_true - pred
  loss <- torch_where(err >= 0, tau * err, (tau - 1) * err)
  torch_mean(loss)
}

nn_feat_extractor <- nn_sequential(
  nn_linear(n_input, 128), nn_relu(),
  nn_linear(128, 64),      nn_relu()
)
nn_out_head <- nn_linear(64, n_quantiles)

nn_feat_extractor$to(device = device)
nn_out_head$to(device = device)

opt_hyb <- optim_adam(c(nn_feat_extractor$parameters, nn_out_head$parameters), lr = 1e-3)

dl_hyb <- make_dl(Xtr_ens_sc, y_tr_sc, batch_size = 512)

nn_feat_extractor$train(); nn_out_head$train()
for (epoch in seq_len(150)) {
  coro::loop(for (b in dl_hyb) {
    opt_hyb$zero_grad()
    feats <- nn_feat_extractor(b$x)
    preds <- nn_out_head(feats)
    loss  <- pinball_loss(preds, b$y, tau_t$to(device = device))
    loss$backward(); opt_hyb$step()
  })
  if (epoch %% 50 == 0)
    cat(sprintf("    Epoch %d, Loss: %.4f\n", epoch, as.numeric(loss)))
}
cat("  Feature extractor training complete.\n\n")

# --- Stage 2: extract features ---
cat("Stage 2: Extracting 64-dim neural features...\n")
nn_feat_extractor$eval()
X_tr_t <- torch_tensor(Xtr_ens_sc, dtype = torch_float(), device = device)
X_te_t <- torch_tensor(Xte_ens_sc, dtype = torch_float(), device = device)
with_no_grad({
  neural_tr <- as.matrix(nn_feat_extractor(X_tr_t))
  neural_te <- as.matrix(nn_feat_extractor(X_te_t))
})
colnames(neural_tr) <- colnames(neural_te) <- paste0("nn_feat_", seq_len(ncol(neural_tr)))
df_neural_tr <- as.data.frame(neural_tr)
df_neural_te <- as.data.frame(neural_te)
cat(sprintf("  Features: %d train x %d dims,  %d test x %d dims\n",
            nrow(df_neural_tr), ncol(df_neural_tr), nrow(df_neural_te), ncol(df_neural_te)))

# --- Stage 3: DRF on neural features ---
cat("Stage 3: Training DRF on neural features...\n")
hybrid_drf <- drf(
  X               = df_neural_tr,
  Y               = y_tr,
  num.trees       = 1000,
  min.node.size   = 50,
  mtry            = floor(sqrt(ncol(df_neural_tr))),
  sample.fraction = 0.5,
  seed            = 2026
)

# --- Stage 4: predict ---
cat("Stage 4: Computing Hybrid predictions (wquant_matrix)...\n")
hybrid_pred   <- predict(hybrid_drf, newdata = df_neural_te)
hybrid_qgrid_mat <- wquant_matrix(as.numeric(hybrid_pred$y), hybrid_pred$weights, q_grid)
lower_hyb     <- min(y_tr)
hybrid_qgrid_mat <- pmax(hybrid_qgrid_mat, lower_hyb)

hybrid_q      <- data.frame(q05 = hybrid_qgrid_mat[, q05_idx],
                            q50 = hybrid_qgrid_mat[, q50_idx],
                            q95 = hybrid_qgrid_mat[, q95_idx])
hybrid_qgrid  <- as.data.frame(hybrid_qgrid_mat)
colnames(hybrid_qgrid) <- paste0("q", sprintf("%02d", round(100 * q_grid)))
cat("Hybrid NN+DRF complete.\n\n")


# ============================================================
# SAVE BEST HYPERPARAMETERS
# ============================================================
best_hyperparameters <- list(
  DRF          = list(num.trees = num_trees_drf,    min.node.size = min_node_size_drf),
  QRF          = list(ntree = ntree_qrf,            nodesize = nodesize_qrf),
  Ranger       = list(num.trees = num_trees_ranger, min.node.size = min_node_size_ranger),
  XGBoost      = list(max_depth = max_depth_xgb,   eta = eta_xgb, nrounds = nrounds_xgb),
  ENS_Gamma    = best_ens_hp,
  MCD_Gamma    = best_mcd_hp,
  DDNN_Gamma   = best_ddnn_hp,
  Ensemble_DRF = list(B = B_edrf, num.trees = num_trees_drf, min.node.size = min_node_size_drf),
  BNN_Gamma    = best_bnn_hp
)


# ============================================================
# UNIFIED EVALUATION (all 10 models)
# ============================================================
cat("\n========================================\n")
cat("UNIFIED EVALUATION — ALL MODELS\n")
cat("========================================\n\n")

# --- Helper definitions ---
eval_interval <- function(q, y) {
  covered <- (y >= q$q05) & (y <= q$q95)
  list(coverage = mean(covered, na.rm = TRUE),
       width    = mean(q$q95 - q$q05, na.rm = TRUE))
}

wis_score <- function(q, y, na.rm = TRUE) {
  pred <- as.matrix(q[, c("q05","q50","q95")])
  mean(scoringutils::wis(y, pred, c(0.05,0.5,0.95), na.rm = na.rm), na.rm = na.rm)
}

pinball_fn <- function(y, q, tau) {
  u <- y - q; ifelse(u >= 0, tau * u, (tau - 1) * u)
}
crps_from_quantiles <- function(y, qmat, taus) {
  ord   <- order(taus); taus <- taus[ord]; qmat <- qmat[, ord, drop = FALSE]
  w <- numeric(length(taus))
  w[1] <- (taus[2] - taus[1]) / 2
  w[length(taus)] <- (taus[length(taus)] - taus[length(taus)-1]) / 2
  if (length(taus) > 2)
    for (j in 2:(length(taus)-1)) w[j] <- (taus[j+1] - taus[j-1]) / 2
  pb <- 0
  for (j in seq_along(taus)) pb <- pb + w[j] * pinball_fn(y, qmat[, j], taus[j])
  mean(2 * pb, na.rm = TRUE)
}

approx_density_from_quantiles <- function(y, qmat, taus) {
  n   <- length(y); dens <- numeric(n)
  for (i in seq_len(n)) {
    qi <- qmat[i, ]; yi <- y[i]
    if (yi <= qi[1]) {
      dq <- qi[2]-qi[1]; dt <- taus[2]-taus[1]
      dens[i] <- if (dq > 1e-10) dt/dq else 1e-6
    } else if (yi >= qi[length(qi)]) {
      nq <- length(qi); dq <- qi[nq]-qi[nq-1]; dt <- taus[nq]-taus[nq-1]
      dens[i] <- if (dq > 1e-10) dt/dq else 1e-6
    } else {
      j  <- max(which(qi <= yi))
      dq <- qi[j+1]-qi[j]; dt <- taus[j+1]-taus[j]
      dens[i] <- if (dq > 1e-10) dt/dq else 1e-6
    }
  }
  pmax(dens, 1e-10)
}

# Named list: model label -> prediction data.frame (q05, q50, q95)
all_q <- list(
  DRF          = drf_q,
  QRF          = qrf_q,
  RF           = rf_q,
  XGB          = xgb_q,
  ENS_Gamma    = ens_q,
  MCD_Gamma    = mcd_q,
  DDNN_Gamma   = ddnn_q,
  Ensemble_DRF = edrf_q,
  BNN_Gamma    = bnn_q,
  Hybrid_NN_DRF = hybrid_q
)
all_qgrid <- list(
  DRF          = drf_qgrid,
  QRF          = qrf_qgrid,
  RF           = rf_qgrid,
  XGB          = xgb_qgrid,
  ENS_Gamma    = ens_qgrid,
  MCD_Gamma    = mcd_qgrid,
  DDNN_Gamma   = ddnn_qgrid,
  Ensemble_DRF = edrf_qgrid,
  BNN_Gamma    = bnn_qgrid,
  Hybrid_NN_DRF = hybrid_qgrid
)
model_names <- names(all_q)

# Coverage & width
cov_width <- do.call(rbind, lapply(model_names, function(m) {
  r <- eval_interval(all_q[[m]], y_te)
  data.frame(Model = m, Coverage = r$coverage, Width = r$width)
}))
cat("Coverage & Width:\n"); print(round(cov_width, 4))

# WIS
wis_vec <- sapply(model_names, function(m) wis_score(all_q[[m]], y_te))
cat("\nWIS (lower = better):\n"); print(round(wis_vec, 4))

# CRPS
crps_vec <- sapply(model_names, function(m)
  crps_from_quantiles(y_te, as.matrix(all_qgrid[[m]]), q_grid))
cat("\nCRPS (lower = better):\n"); print(round(crps_vec, 4))

# NLL  (Gamma-exact for parametric models, quantile-approx for non-parametric)
nll_vec <- sapply(model_names, function(m) {
  if (m == "XGB")          return(NA_real_)
  if (m == "ENS_Gamma")    return(nll_ens)
  if (m == "MCD_Gamma")    return(nll_mcd)
  if (m == "DDNN_Gamma")   return(nll_ddnn)
  if (m == "BNN_Gamma")    return(nll_bnn)
  # non-parametric: quantile-based approximation
  -mean(log(approx_density_from_quantiles(y_te, as.matrix(all_qgrid[[m]]), q_grid)))
})
cat("\nNLL (lower = better, NA = XGB/conformal):\n"); print(round(nll_vec, 4))

# Summary table
summary_all <- data.frame(
  Model    = model_names,
  Coverage = cov_width$Coverage,
  Width    = cov_width$Width,
  WIS      = wis_vec,
  CRPS     = crps_vec,
  NLL      = nll_vec
)
summary_all$WIS_Rank <- rank(summary_all$WIS, na.last = "keep")
cat("\n=== SUMMARY TABLE ===\n")
print(round(summary_all[order(summary_all$WIS_Rank), ], 4))


# ---- eval_df: wide format for subgroup analysis ----
eval_df <- as.data.frame(lapply(all_q, function(qdf) qdf))
names(eval_df) <- paste0(
  rep(tolower(gsub("[^a-zA-Z0-9]", "_", names(all_q))), each = 3),
  rep(c("_q05","_q50","_q95"), times = length(all_q))
)
# rebuild properly
eval_df <- do.call(data.frame, lapply(names(all_q), function(m) {
  mn <- tolower(gsub("[^a-zA-Z0-9]", "_", m))
  q  <- all_q[[m]]
  setNames(q, paste0(mn, c("_q05","_q50","_q95")))
}))
eval_df$y          <- y_te

eval_df$los_group  <- cut(y_te, c(-Inf,3,7,Inf), labels=c("Short(≤3d)","Medium(3-7d)","Long(>7d)"))
eval_df$miss_rate  <- rowMeans(is.na(Xte_tree))
eval_df$miss_group <- cut(eval_df$miss_rate,
                          breaks = quantile(eval_df$miss_rate, c(0,1/3,2/3,1)),
                          labels = c("Low missing","Mid missing","High missing"),
                          include.lowest = TRUE)
eval_df$vent_group <- factor(Xte_tree$vent_any,    0:1, c("No ventilation","Ventilated"))
eval_df$vaso_group <- factor(Xte_tree$vasopressors, 0:1, c("No vasopressors","Vasopressors"))

# WIS helper for subgroup
wis_one <- function(y, q05, q50, q95) {
  pred <- cbind(q05, q50, q95)
  mean(scoringutils::wis(y, pred, c(0.05,0.5,0.95), na.rm=TRUE), na.rm=TRUE)
}

# Subgroup WIS (miss_group)
make_model_col_map <- function() {
  lapply(names(all_q), function(m) {
    mn <- tolower(gsub("[^a-zA-Z0-9]","_",m))
    list(label=m, q05=paste0(mn,"_q05"), q50=paste0(mn,"_q50"), q95=paste0(mn,"_q95"))
  })
}
col_map <- make_model_col_map()

subgroup_wis_table <- function(df, group_var) {
  df %>%
    group_by(.data[[group_var]]) %>%
    summarise(
      across(everything(), ~ NA, .names = "{.col}"),
      .groups = "drop"
    ) %>%
    select(1) -> grps
  
  wis_cols <- lapply(col_map, function(cm) {
    df %>%
      group_by(.data[[group_var]]) %>%
      summarise(!!cm$label := wis_one(y, .data[[cm$q05]], .data[[cm$q50]], .data[[cm$q95]]),
                .groups = "drop") %>%
      pull(!!cm$label)
  })
  bind_cols(grps, setNames(as.data.frame(wis_cols), sapply(col_map, `[[`, "label")))
}

out_miss_wis <- subgroup_wis_table(eval_df, "miss_group")
cat("\n=== WIS by Missingness Subgroup ===\n"); print(out_miss_wis)

out_vent_wis <- subgroup_wis_table(eval_df, "vent_group")
cat("\n=== WIS by Ventilation Subgroup ===\n"); print(out_vent_wis)

out_vaso_wis <- subgroup_wis_table(eval_df, "vaso_group")
cat("\n=== WIS by Vasopressor Subgroup ===\n"); print(out_vaso_wis)


# ---- CRPS all grid (already computed above) ----
cat("\nCRPS (dense grid):\n"); print(round(crps_vec, 4))


# ---- NLL summary ----
nll_results <- nll_vec
cat("\nNLL:\n"); print(round(nll_results, 4))


# ============================================================
# Distribution comparison plots (all 10 models)
# ============================================================
library(ggplot2); library(scales)
set.seed(2026)
n_spp <- 100  # samples per patient

sample_from_quantiles <- function(qmat, taus, n_samples) {
  n <- nrow(qmat)
  m <- matrix(0, n, n_samples)
  for (i in seq_len(n)) {
    u    <- runif(n_samples)
    m[i, ] <- approx(taus, qmat[i, ], xout = u, rule = 2)$y
  }
  m
}

# for Gamma parametric models sample directly from the distribution
sample_gamma_mix <- function(shapes_list, rates_list, n_samples) {
  M   <- length(shapes_list)
  n   <- length(shapes_list[[1]])
  out <- matrix(NA_real_, n, n_samples)
  for (i in seq_len(n)) {
    # randomly pick a member for each sample
    mem <- sample(M, n_samples, replace = TRUE)
    for (m in seq_len(M)) {
      idx <- which(mem == m)
      if (length(idx) > 0)
        out[i, idx] <- rgamma(length(idx), shape = shapes_list[[m]][i], rate = rates_list[[m]][i])
    }
  }
  out
}

cat("\nSampling from predictive distributions...\n")
samples_list <- list(
  DRF          = sample_from_quantiles(as.matrix(drf_qgrid),    q_grid, n_spp),
  QRF          = sample_from_quantiles(as.matrix(qrf_qgrid),    q_grid, n_spp),
  RF           = sample_from_quantiles(as.matrix(rf_qgrid),     q_grid, n_spp),
  ENS_Gamma    = sample_gamma_mix(ens_shapes, ens_rates,         n_spp),
  MCD_Gamma    = matrix(rgamma(length(y_te)*n_spp,
                               shape = rep(mcd_params_te$shape, n_spp),
                               rate  = rep(mcd_params_te$rate,  n_spp)),
                        nrow = length(y_te)),
  DDNN_Gamma   = matrix(rgamma(length(y_te)*n_spp,
                               shape = rep(ddnn_params$shape, n_spp),
                               rate  = rep(ddnn_params$rate,  n_spp)),
                        nrow = length(y_te)),
  Ensemble_DRF = sample_from_quantiles(as.matrix(edrf_qgrid),   q_grid, n_spp),
  BNN_Gamma    = matrix(rgamma(length(y_te)*n_spp,
                               shape = rep(bnn_params_te$shape, n_spp),
                               rate  = rep(bnn_params_te$rate,  n_spp)),
                        nrow = length(y_te)),
  Hybrid_NN_DRF = sample_from_quantiles(as.matrix(hybrid_qgrid), q_grid, n_spp)
)
# XGB: symmetric conformal, sample from triangular approx
samples_list$XGB <- sample_from_quantiles(as.matrix(xgb_qgrid), q_grid, n_spp)

n_act <- length(y_te); n_mod <- n_act * n_spp

df_dist <- bind_rows(
  data.frame(LOS = y_te, Model = "Actual"),
  bind_rows(lapply(names(samples_list), function(m)
    data.frame(LOS = as.vector(samples_list[[m]]), Model = m)))
) %>% filter(LOS >= 0, LOS <= quantile(y_te, 0.99) * 1.5)

all_levels <- c("Actual", model_names)
df_dist$Model <- factor(df_dist$Model, levels = all_levels)

# Colour palette
MODEL_COLOR <- c(
  Actual       = "gray20",
  DRF          = "#D65C5C", QRF       = "#5A9BD4", RF            = "#66BB66",
  XGB          = "#E8A838", ENS_Gamma = "#9B59B6", MCD_Gamma     = "#E67E22",
  DDNN_Gamma   = "#1ABC9C", Ensemble_DRF = "#C0392B", BNN_Gamma  = "#2471A3",
  Hybrid_NN_DRF = "#6E2F1A"
)
MODEL_FILL <- c(
  DRF          = "#F4A3A3", QRF       = "#A8C5E5", RF            = "#B7E3B0",
  XGB          = "#FAD7A0", ENS_Gamma = "#D7BDE2", MCD_Gamma     = "#FDEBD0",
  DDNN_Gamma   = "#A3E4D7", Ensemble_DRF = "#F1948A", BNN_Gamma = "#AED6F1",
  Hybrid_NN_DRF = "#D7BFAE"
)

df_actual_bg <- df_dist %>% filter(Model == "Actual") %>% select(-Model)

p_dist <- ggplot() +
  geom_density(data = df_actual_bg, aes(x = LOS),
               fill = "gray60", color = "gray30", alpha = 0.7, linewidth = 0.4) +
  geom_density(data = df_dist %>% filter(Model != "Actual"),
               aes(x = LOS, fill = Model, color = Model),
               alpha = 0.45, linewidth = 0.85) +
  facet_wrap(~ Model, ncol = 3) +
  scale_fill_manual(values  = MODEL_FILL,  na.value = "gray80") +
  scale_color_manual(values = MODEL_COLOR, na.value = "gray40") +
  coord_cartesian(xlim = c(0, quantile(y_te, 0.95) * 1.3)) +
  theme_bw(base_size = 12) +
  labs(title    = "Predictive Distributions vs Actual LOS (All 10 Models)",
       subtitle = "Gray = actual LOS distribution repeated in every panel",
       x = "Length of Stay (Days)", y = "Density") +
  theme(strip.text       = element_text(face = "bold"),
        legend.position  = "none",
        panel.grid.minor = element_blank(),
        plot.title       = element_text(face = "bold"))

print(p_dist)
ggsave("/mnt/user-data/outputs/all_models_dist_comparison.png",
       plot = p_dist, width = 16, height = 14, dpi = 300)
cat("✓ Saved: all_models_dist_comparison.png\n")


# ---- Conditional calibration ----
K <- 10
cal_df <- bind_rows(lapply(names(all_q), function(m) {
  eval_df %>%
    transmute(model = m, y,
              q05 = all_q[[m]]$q05,
              q50 = all_q[[m]]$q50,
              q95 = all_q[[m]]$q95)
})) %>%
  mutate(x = q50, covered = (y >= q05) & (y <= q95)) %>%
  group_by(model) %>%
  mutate(bin = ntile(x, K)) %>%
  group_by(model, bin) %>%
  summarise(x = mean(x, na.rm=TRUE), coverage = mean(covered, na.rm=TRUE),
            n_bin = n(), .groups="drop") %>%
  mutate(se    = sqrt(coverage*(1-coverage)/n_bin),
         lower = pmax(0, coverage-1.96*se),
         upper = pmin(1, coverage+1.96*se),
         model = factor(model, levels = model_names))

p_cal <- ggplot(cal_df, aes(x=x, y=coverage)) +
  geom_hline(yintercept=0.90, linetype="dashed", color="red", alpha=0.6) +
  geom_ribbon(aes(ymin=lower, ymax=upper), fill="steelblue", alpha=0.15) +
  geom_line(color="steelblue", linewidth=0.8) +
  geom_point(color="steelblue", size=1.5, alpha=0.8) +
  facet_wrap(~model, ncol=2) +
  scale_y_continuous(labels=percent, limits=c(0.55,1.0)) +
  labs(title="Conditional Calibration of 90% Prediction Intervals (All Models)",
       x="Predicted Median LOS (Days)", y="Empirical Coverage (%)") +
  theme_bw(base_size=11) +
  theme(strip.background=element_rect(fill="#f0f0f0"),
        panel.grid.minor=element_blank(),
        plot.title=element_text(face="bold"))

print(p_cal)
ggsave("/mnt/user-data/outputs/conditional_calibration_all.png",
       plot=p_cal, width=12, height=18, dpi=300)
cat("✓ Saved: conditional_calibration_all.png\n")


# ---- Coverage vs Width ----
cover_long <- bind_rows(lapply(names(all_q), function(m) {
  eval_df %>%
    transmute(model=m, y,
              w   = all_q[[m]]$q95 - all_q[[m]]$q05,
              cov = as.integer(y>=all_q[[m]]$q05 & y<=all_q[[m]]$q95))
})) %>% filter(is.finite(w), w>=0) %>%
  mutate(model = factor(model, levels = model_names))

cal_w <- cover_long %>%
  group_by(model) %>%
  mutate(w_bin = ntile(w, 10)) %>%
  group_by(model, w_bin) %>%
  summarise(n_bin=n(), x=median(w, na.rm=TRUE), coverage=mean(cov, na.rm=TRUE), .groups="drop") %>%
  mutate(se    = sqrt(coverage*(1-coverage)/n_bin),
         lower = pmax(0, coverage-1.96*se),
         upper = pmin(1, coverage+1.96*se))

p_cov_w <- ggplot(cal_w, aes(x=x, y=coverage)) +
  geom_hline(yintercept=0.90, linetype="dashed", linewidth=0.8, alpha=0.7) +
  geom_ribbon(aes(ymin=lower,ymax=upper), alpha=0.15) +
  geom_line(linewidth=0.9) + geom_point(size=2, alpha=0.85) +
  facet_wrap(~model, ncol=2, scales="free_x") +
  scale_y_continuous(labels=percent_format(accuracy=1), limits=c(0,1)) +
  labs(title="Coverage vs Predicted Interval Width (90% PI) — All Models",
       x="Predicted interval width [bin median]", y="Empirical coverage") +
  theme_bw(base_size=11) +
  theme(strip.background=element_rect(fill="#f2f2f2"),
        panel.grid.minor=element_blank(), plot.title=element_text(face="bold"))

print(p_cov_w)
ggsave("/mnt/user-data/outputs/coverage_vs_width_all.png",
       plot=p_cov_w, width=12, height=18, dpi=300)
cat("✓ Saved: coverage_vs_width_all.png\n")


# ---- KL divergence & KS test ----
kl_divergence_kde <- function(x_true, x_pred, n_grid = 200) {
  xl <- min(c(x_true, x_pred), na.rm=TRUE); xh <- max(c(x_true, x_pred), na.rm=TRUE)
  p  <- density(x_true, from=xl, to=xh, n=n_grid)$y
  q  <- density(x_pred, from=xl, to=xh, n=n_grid)$y
  eps <- 1e-10; p <- pmax(p,eps)/sum(pmax(p,eps)); q <- pmax(q,eps)/sum(pmax(q,eps))
  sum(p * log(p/q), na.rm=TRUE)
}

cat("\nComputing KL divergence and KS distance...\n")
kl_results <- sapply(names(samples_list), function(m)
  kl_divergence_kde(y_te, as.vector(samples_list[[m]])))
ks_results <- sapply(names(samples_list), function(m)
  ks.test(y_te, as.vector(samples_list[[m]]))$statistic)

cat("\nKL Divergence (lower = better):\n"); print(round(kl_results, 4))
cat("\nKS Distance   (lower = better):\n"); print(round(ks_results, 4))


# ---- 2D calibration heatmap ----
n_risk_bins <- 8; n_w_bins <- 8; min_n_cell <- 30

long2d <- bind_rows(lapply(names(all_q), function(m) {
  eval_df %>%
    transmute(model = m, y,
              q50  = all_q[[m]]$q50,
              w    = all_q[[m]]$q95 - all_q[[m]]$q05,
              cov  = as.integer(y>=all_q[[m]]$q05 & y<=all_q[[m]]$q95))
})) %>% filter(is.finite(q50), is.finite(w), w>=0) %>%
  mutate(model = factor(model, levels=model_names))

heat2d <- long2d %>%
  group_by(model) %>%
  mutate(risk_bin  = ntile(q50, n_risk_bins),
         width_bin = ntile(w,   n_w_bins)) %>%
  group_by(model, risk_bin, width_bin) %>%
  summarise(n_cell=n(), coverage=mean(cov, na.rm=TRUE), .groups="drop") %>%
  mutate(delta = coverage - 0.90,
         delta_plot = ifelse(n_cell < min_n_cell, NA, delta))

p_heat <- ggplot(heat2d, aes(x=risk_bin, y=width_bin, fill=delta_plot)) +
  geom_tile(color="white", linewidth=0.3) +
  facet_wrap(~model, ncol=2) +
  scale_fill_gradient2(low="#d73027", mid="white", high="#1a9850",
                       midpoint=0, labels=percent_format(accuracy=1),
                       na.value="grey90", name="Coverage\n- 90%") +
  labs(title   = "2D Conditional Calibration (Risk × Uncertainty) — All Models",
       subtitle = "Grey = too few samples (<30)",
       x = "Risk bin (predicted median)", y = "Uncertainty bin (interval width)") +
  theme_bw(base_size=11) +
  theme(panel.grid=element_blank(), strip.background=element_rect(fill="#f2f2f2"),
        plot.title=element_text(face="bold"))

print(p_heat)
ggsave("/mnt/user-data/outputs/calibration_heatmap_all.png",
       plot=p_heat, width=12, height=20, dpi=300)
cat("✓ Saved: calibration_heatmap_all.png\n")


# ---- Hypothesis test: does Hybrid beat all categories? ----
cat("\n========================================\n")
cat("HYPOTHESIS: Does Hybrid beat all categories?\n")
cat("========================================\n")

gamma_nn_models  <- c("ENS_Gamma","MCD_Gamma","DDNN_Gamma","BNN_Gamma")
forest_models    <- c("DRF","QRF","RF","Ensemble_DRF")

best_gamma_wis   <- min(wis_vec[gamma_nn_models], na.rm=TRUE)
best_forest_wis  <- min(wis_vec[forest_models],   na.rm=TRUE)
hybrid_wis_val   <- wis_vec["Hybrid_NN_DRF"]

cat(sprintf("Best Gamma-NN WIS:   %.4f  (%s)\n", best_gamma_wis,
            names(which.min(wis_vec[gamma_nn_models]))))
cat(sprintf("Best Forest WIS:     %.4f  (%s)\n", best_forest_wis,
            names(which.min(wis_vec[forest_models]))))
cat(sprintf("Hybrid NN+DRF WIS:   %.4f\n", hybrid_wis_val))

beats_nn     <- hybrid_wis_val < best_gamma_wis
beats_forest <- hybrid_wis_val < best_forest_wis
if (beats_nn && beats_forest) {
  cat("\n✓ ✓ ✓  Hybrid beats BOTH Gamma-NN and forest categories!\n")
  cat(sprintf("  Δ vs best Gamma-NN:  %.2f%%\n", 100*(best_gamma_wis-hybrid_wis_val)/best_gamma_wis))
  cat(sprintf("  Δ vs best forest:    %.2f%%\n", 100*(best_forest_wis-hybrid_wis_val)/best_forest_wis))
} else if (beats_nn)     cat("\n⚠ Hybrid beats Gamma-NN but not forest\n")
else if (beats_forest)   cat("\n⚠ Hybrid beats forest but not Gamma-NN\n")
else                     cat("\n✗ Hybrid does not beat either category\n")


# ---- Save outputs ----
write.csv(summary_all, "/mnt/user-data/outputs/summary_all_models.csv",    row.names=FALSE)
write.csv(out_miss_wis,"/mnt/user-data/outputs/subgroup_miss_wis.csv",     row.names=FALSE)
write.csv(out_vent_wis,"/mnt/user-data/outputs/subgroup_vent_wis.csv",     row.names=FALSE)
write.csv(out_vaso_wis,"/mnt/user-data/outputs/subgroup_vaso_wis.csv",     row.names=FALSE)

saveRDS(list(
  summary          = summary_all,
  wis              = wis_vec,
  crps             = crps_vec,
  nll              = nll_results,
  kl               = kl_results,
  ks               = ks_results,
  best_hyperparams = best_hyperparameters,
  # model objects
  drf_fit          = drf_fit,
  qrf_fit          = qrf_fit,
  rf_q_fit         = rf_q_fit,
  xgb_fit          = xgb_fit,
  ens_members      = ens_members,
  mc_dropout_net   = mc_dropout_net,
  ddnn_net         = ddnn_net,
  edrf_members     = edrf_members,
  bnn_net          = bnn_net,
  hybrid_drf       = hybrid_drf,
  nn_feat_extractor = nn_feat_extractor
), "/mnt/user-data/outputs/all_model_results.rds")

cat("\n✓ All results saved to /mnt/user-data/outputs/\n")
cat("\n========================================\n")
cat("✓ ✓ ✓  ALL 10 MODELS COMPLETE  ✓ ✓ ✓\n")
cat("========================================\n\n")



