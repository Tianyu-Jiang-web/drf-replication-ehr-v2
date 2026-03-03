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
# MODEL 8: DDNN — Distributional DNN (Gamma)
# Following Marcjasz et al. (2023, Energy Economics):
#   - N_runs=4 independent HP optimisation trials (counteract local minima)
#   - Per-run: train on tr_cv, evaluate on va_cv, pick best HP
#   - Final: retrain each run on full train set with its own best HP
#   - Ensemble via HORIZONTAL (quantile) averaging of N_runs distributions
#     (qEns scheme: mean of same-quantile predictions, cf. Section 4.2.2)
#   - Separate L1 regularisation weights for each Gamma output parameter
#     (per-parameter layer regularisation, cf. Eq. 4 in paper)
#   - Activation: softplus (most frequently selected in paper, Table 6)
#   - Early stopping on validation NLL (patience = 20 epochs)
# ============================================================
cat("========================================\n")
cat("MODEL 8: DDNN (Gamma, Marcjasz et al. 2023)\n")
cat("========================================\n")

# Architecture: softplus activations (most selected in paper, Table 6)
# Output layer: 2 params (log_shape, log_rate) with separate L1 reg
make_ddnn_net <- function(n_in, h1, h2, dropout_rate = 0.0) {
  nn_sequential(
    nn_linear(n_in, h1), nn_softplus(),
    if (dropout_rate > 0) nn_dropout(dropout_rate) else nn_identity(),
    nn_linear(h1,   h2), nn_softplus(),
    nn_linear(h2,    2)   # log_shape, log_rate
  )
}

# Training with early stopping (Marcjasz et al. use patience=50 but shorter runs here)
train_ddnn <- function(X_tr, y_tr, X_va, y_va, h1, h2, lr,
                       dropout_rate = 0.0, l1_hidden = 0.0,
                       l1_shape = 0.0, l1_rate = 0.0,
                       max_epochs = 200, patience = 20,
                       batch_size = 32, seed = 42) {
  set.seed(seed)
  torch_manual_seed(seed)
  net   <- make_ddnn_net(n_input, h1, h2, dropout_rate)$to(device = device)
  optim <- optim_adam(net$parameters, lr = lr)
  dl    <- make_dl(X_tr, y_tr, batch_size)
  
  # Validation tensors for early stopping
  Xva_t <- torch_tensor(X_va, dtype = torch_float(), device = device)
  yva_t <- torch_tensor(matrix(y_va, ncol = 1), dtype = torch_float(), device = device)
  
  best_val_nll <- Inf; best_state <- NULL; patience_cnt <- 0
  
  net$train()
  for (ep in seq_len(max_epochs)) {
    coro::loop(for (b in dl) {
      optim$zero_grad()
      params <- net(b$x)
      nll    <- gamma_nll_loss(params, b$y)
      
      # L1 reg on hidden layer weights (Eq. 3 in Marcjasz et al.)
      l1_pen <- torch_tensor(0.0, device = device)
      if (l1_hidden > 0) {
        for (nm in names(net$parameters)) {
          if (grepl("^[0-9]+\\.weight$", nm) && !grepl("^[56789]", nm)) {
            l1_pen <- l1_pen + l1_hidden * torch_sum(torch_abs(net$parameters[[nm]]))
          }
        }
      }
      # Separate L1 per output parameter (Eq. 4 in Marcjasz et al.)
      # Last layer weight rows: row 1 → log_shape, row 2 → log_rate
      last_w_nm <- tail(names(net$parameters)[grepl("weight", names(net$parameters))], 1)
      if (!is.null(last_w_nm) && length(last_w_nm) > 0) {
        last_w <- net$parameters[[last_w_nm]]
        if (l1_shape > 0) l1_pen <- l1_pen + l1_shape * torch_sum(torch_abs(last_w[1, ]))
        if (l1_rate  > 0) l1_pen <- l1_pen + l1_rate  * torch_sum(torch_abs(last_w[2, ]))
      }
      
      loss <- nll + l1_pen
      loss$backward(); optim$step()
    })
    
    # Early stopping: evaluate val NLL
    net$eval()
    with_no_grad({
      va_params <- net(Xva_t)
      va_nll    <- as.numeric(gamma_nll_loss(va_params, yva_t))
    })
    net$train()
    
    if (va_nll < best_val_nll - 1e-6) {
      best_val_nll  <- va_nll
      best_state    <- lapply(net$state_dict(), function(p) p$clone())
      patience_cnt  <- 0
    } else {
      patience_cnt <- patience_cnt + 1
      if (patience_cnt >= patience) {
        cat(sprintf("    Early stop at epoch %d (best val NLL=%.4f)\n", ep, best_val_nll))
        break
      }
    }
  }
  # Restore best weights
  if (!is.null(best_state)) net$load_state_dict(best_state)
  net$eval()
  net
}

# HP grid (Marcjasz et al. tune neurons ∈[16,1024], LR ∈[1e-5,1e-1], dropout, L1)
# We use a reduced grid for tractability, with key findings from paper:
# - softplus best for Layer 1; relu/elu for Layer 2
# - wide hidden layers often selected (500-1000)
# - dropout almost never chosen; L1 regularisation sometimes
ddnn_hp_grid <- expand.grid(
  h1           = c(256, 512),
  h2           = c(128, 256),
  lr           = c(1e-3, 5e-4),
  dropout_rate = c(0.0),           # paper: dropout almost never chosen
  l1_hidden    = c(0.0, 1e-4),
  stringsAsFactors = FALSE
)

N_runs_ddnn <- 4  # 4 independent HP optimisation trials (Marcjasz et al.)
ddnn_runs   <- vector("list", N_runs_ddnn)
cat(sprintf("Running %d independent DDNN HP optimisation trials...\n", N_runs_ddnn))

for (run in seq_len(N_runs_ddnn)) {
  cat(sprintf("\n--- DDNN Run %d/%d ---\n", run, N_runs_ddnn))
  set.seed(2026 + run * 31)
  
  # For each run, do a mini CV over the HP grid (2048 iterations in paper;
  # we use full grid with early stopping as proxy)
  run_cv <- sapply(seq_len(nrow(ddnn_hp_grid)), function(i) {
    hp <- ddnn_hp_grid[i, ]
    tryCatch({
      net <- train_ddnn(X_tr_cv, y_tr_cv, X_va_cv, y_va_cv,
                        h1 = hp$h1, h2 = hp$h2, lr = hp$lr,
                        dropout_rate = hp$dropout_rate,
                        l1_hidden    = hp$l1_hidden,
                        max_epochs = 150, patience = 20,
                        batch_size = 32, seed = 2026 + run * 31 + i)
      p <- get_gamma_params(net, X_va_cv)
      pm <- gamma_qgrid(p$shape, p$rate, qs)
      mean(scoringutils::wis(y_va_cv, pm, qs, na.rm = TRUE), na.rm = TRUE)
    }, error = function(e) Inf)
  })
  
  best_i <- which.min(run_cv)
  best_hp_run <- as.list(ddnn_hp_grid[best_i, ])
  cat(sprintf("  Best: h1=%d h2=%d lr=%.4g l1=%.4g (WIS=%.4f)\n",
              best_hp_run$h1, best_hp_run$h2, best_hp_run$lr,
              best_hp_run$l1_hidden, run_cv[best_i]))
  
  # Retrain on full train set with best HP for this run
  cat(sprintf("  Retraining run %d on full training set...\n", run))
  ddnn_runs[[run]] <- list(
    net    = train_ddnn(Xtr_ens_sc, y_tr,
                        X_va_cv, y_va_cv,   # still use for early stopping
                        h1 = best_hp_run$h1, h2 = best_hp_run$h2,
                        lr = best_hp_run$lr,
                        dropout_rate = best_hp_run$dropout_rate,
                        l1_hidden    = best_hp_run$l1_hidden,
                        max_epochs = 300, patience = 30,
                        batch_size = 32, seed = 2026 + run * 31),
    best_hp = best_hp_run,
    val_wis = run_cv[best_i]
  )
}

# HORIZONTAL (quantile) averaging — qEns scheme (Marcjasz et al. Section 4.2.2)
# "a quantile of an ensemble is computed as an arithmetic mean of
#  the same quantiles from all distributions considered"
cat("\nComputing DDNN qEns (horizontal quantile averaging)...\n")
ddnn_qgrid_runs <- lapply(ddnn_runs, function(r) {
  p <- get_gamma_params(r$net, Xte_ens_sc)
  gamma_qgrid(p$shape, p$rate, q_grid)
})

# qEns: element-wise mean across N_runs quantile grids
ddnn_qgrid_mat <- Reduce(`+`, ddnn_qgrid_runs) / N_runs_ddnn

ddnn_q <- data.frame(
  q05 = ddnn_qgrid_mat[, q05_idx],
  q50 = ddnn_qgrid_mat[, q50_idx],
  q95 = ddnn_qgrid_mat[, q95_idx]
)
ddnn_qgrid <- as.data.frame(ddnn_qgrid_mat)
colnames(ddnn_qgrid) <- paste0("q", sprintf("%02d", round(100 * q_grid)))

# NLL: average across runs (each run gives a Gamma distribution; use mean params)
ddnn_params_all <- lapply(ddnn_runs, function(r) get_gamma_params(r$net, Xte_ens_sc))
ddnn_shape_avg  <- Reduce(`+`, lapply(ddnn_params_all, `[[`, "shape")) / N_runs_ddnn
ddnn_rate_avg   <- Reduce(`+`, lapply(ddnn_params_all, `[[`, "rate"))  / N_runs_ddnn
nll_ddnn <- -mean(dgamma(y_te, shape = ddnn_shape_avg, rate = ddnn_rate_avg, log = TRUE))

best_ddnn_hp <- lapply(ddnn_runs, `[[`, "best_hp")   # list of per-run HPs
cat(sprintf("DDNN (qEns, %d runs) complete. Mean val WIS=%.4f\n\n",
            N_runs_ddnn, mean(sapply(ddnn_runs, `[[`, "val_wis"))))


# ============================================================
# MODEL 9: Ensemble DRF — Stacked Generalisation
# Following Papacharalampous et al. (2025, Machine Learning with Applications):
#   Algorithm (Section 2.3 / Figure 1):
#   Step 1: Split train into Set1 (tr1) and Set2 (tr2)
#   Step 2: Train K=2 DRF base learners on Set1 (different mtry seeds)
#   Step 3: Predict quantiles for Set2 from each base learner
#   Step 4: Train linear quantile regression (QR) combiner on Set2
#           using base-learner quantile predictions as predictors
#           — minimise quantile loss (Koenker & Bassett 1978)
#   Step 5: Retrain K base learners on Set1 ∪ Set2 (full train)
#   Step 6: Extract quantile predictions from updated base learners on test
#   Step 7: Apply trained QR combiner to produce final test quantile predictions
#
# Base learners: DRF variants (ZAGA-style: full conditional distribution)
# Combiner:      Linear quantile regression (quantreg package)
# Evaluation vs mean combiner benchmark (also included)
# ============================================================
cat("========================================\n")
cat("MODEL 9: Ensemble DRF (Stacking, Papacharalampous et al. 2025)\n")
cat("========================================\n")

library(quantreg)

# ---- Step 1: Split train into Set1 / Set2 (50/50, Papacharalampous et al.) ----
set.seed(2026 + 900)
n_tr     <- nrow(Xtr_tree)
idx_set1 <- sample(n_tr, floor(n_tr / 2), replace = FALSE)
idx_set2 <- setdiff(seq_len(n_tr), idx_set1)
cat(sprintf("Stacking split: Set1=%d, Set2=%d\n", length(idx_set1), length(idx_set2)))

X_set1   <- Xtr_tree[idx_set1, , drop = FALSE]
y_set1   <- y_tr[idx_set1]
X_set2   <- Xtr_tree[idx_set2, , drop = FALSE]
y_set2   <- y_tr[idx_set2]

# ---- Step 2: Train K base DRF learners on Set1 ----
# K=2 DRF variants (analogous to DRF-ZAIG and DRF-ZAGA in paper;
# here: DRF with different mtry to introduce learner diversity)
K_drf <- 2
cat(sprintf("Training %d DRF base learners on Set1...\n", K_drf))

drf_base_s1 <- lapply(seq_len(K_drf), function(k) {
  mtry_k <- if (k == 1) floor(sqrt(ncol(Xtr_tree))) else floor(ncol(Xtr_tree) / 3)
  cat(sprintf("  Base learner %d: mtry=%d\n", k, mtry_k))
  drf(
    X               = X_set1,
    Y               = y_set1,
    num.trees       = num_trees_drf,
    min.node.size   = min_node_size_drf,
    mtry            = mtry_k,
    sample.fraction = 0.5,
    seed            = 2026 + k * 7
  )
})

# ---- Step 3: Predict quantiles for Set2 from each base learner ----
cat("Predicting quantiles for Set2 (combiner training data)...\n")
# Full quantile grid on Set2 (for combiner training)
set2_qpreds <- lapply(drf_base_s1, function(fit) {
  pr <- predict(fit, X_set2)
  wquant_matrix(as.numeric(pr$y), pr$weights, q_grid)  # n_set2 x n_quantiles
})

# ---- Step 4: Fit linear QR combiner separately for each quantile level ----
# (Eq. 4 in Papacharalampous et al.: tau-quantile combiner uses base quantile preds as covariates)
cat("Fitting QR combiners for all quantile levels (Set2)...\n")
qr_combiners <- vector("list", length(q_grid))

for (qi in seq_along(q_grid)) {
  tau_i <- q_grid[qi]
  # Predictor matrix: [n_set2 x K] — base-learner quantile predictions at tau_i
  X_comb <- do.call(cbind, lapply(set2_qpreds, function(qm) qm[, qi]))
  # Response: actual y_set2
  df_comb <- as.data.frame(X_comb)
  colnames(df_comb) <- paste0("bl", seq_len(K_drf))
  df_comb$y <- y_set2
  # Linear quantile regression combiner (quantreg::rq)
  fmla <- as.formula(paste("y ~", paste(paste0("bl", seq_len(K_drf)), collapse = " + ")))
  qr_combiners[[qi]] <- tryCatch(
    rq(fmla, tau = tau_i, data = df_comb, method = "br"),
    error = function(e) NULL
  )
}
cat(sprintf("  Fitted QR combiners for %d quantile levels.\n", sum(!sapply(qr_combiners, is.null))))

# ---- Step 5: Retrain base learners on full train (Set1 ∪ Set2) ----
cat("Retraining DRF base learners on full training set...\n")
drf_base_full <- lapply(seq_len(K_drf), function(k) {
  mtry_k <- if (k == 1) floor(sqrt(ncol(Xtr_tree))) else floor(ncol(Xtr_tree) / 3)
  cat(sprintf("  Base learner %d: mtry=%d (full train n=%d)\n", k, mtry_k, nrow(Xtr_tree)))
  drf(
    X               = Xtr_tree,
    Y               = y_tr,
    num.trees       = num_trees_drf,
    min.node.size   = min_node_size_drf,
    mtry            = mtry_k,
    sample.fraction = 0.5,
    seed            = 2026 + k * 7 + 100
  )
})

# ---- Step 6: Quantile predictions from updated base learners on test ----
cat("Predicting test quantiles from updated base learners...\n")
te_qpreds <- lapply(drf_base_full, function(fit) {
  pr <- predict(fit, Xte_tree)
  wquant_matrix(as.numeric(pr$y), pr$weights, q_grid)  # n_te x n_quantiles
})

# ---- Step 7: Apply QR combiner to produce stacked test predictions ----
cat("Applying stacking combiner to test predictions...\n")
edrf_qgrid_mat <- matrix(NA_real_, nrow(Xte_tree), length(q_grid))

for (qi in seq_along(q_grid)) {
  X_te_comb <- do.call(cbind, lapply(te_qpreds, function(qm) qm[, qi]))
  df_te_comb <- as.data.frame(X_te_comb)
  colnames(df_te_comb) <- paste0("bl", seq_len(K_drf))
  
  if (!is.null(qr_combiners[[qi]])) {
    preds <- predict(qr_combiners[[qi]], newdata = df_te_comb)
    edrf_qgrid_mat[, qi] <- pmax(preds, 0)  # enforce non-negativity (LOS >= 0)
  } else {
    # Fallback: mean combiner
    edrf_qgrid_mat[, qi] <- rowMeans(X_te_comb)
  }
}

# Enforce quantile monotonicity (isotonic regression per patient)
for (i in seq_len(nrow(edrf_qgrid_mat))) {
  edrf_qgrid_mat[i, ] <- sort(edrf_qgrid_mat[i, ])
}

# ---- Also compute mean combiner (benchmark, cf. Papacharalampous et al. Table/Fig 5) ----
cat("Computing mean combiner benchmark...\n")
edrf_meancomb_mat <- Reduce(`+`, te_qpreds) / K_drf
for (i in seq_len(nrow(edrf_meancomb_mat))) {
  edrf_meancomb_mat[i, ] <- sort(edrf_meancomb_mat[i, ])
}

# Use stacked predictions as primary Ensemble_DRF output
edrf_q <- data.frame(
  q05 = edrf_qgrid_mat[, q05_idx],
  q50 = edrf_qgrid_mat[, q50_idx],
  q95 = edrf_qgrid_mat[, q95_idx]
)
edrf_qgrid <- as.data.frame(edrf_qgrid_mat)
colnames(edrf_qgrid) <- paste0("q", sprintf("%02d", round(100 * q_grid)))

# Store mean combiner results separately for comparison
edrf_meancomb_q <- data.frame(
  q05 = edrf_meancomb_mat[, q05_idx],
  q50 = edrf_meancomb_mat[, q50_idx],
  q95 = edrf_meancomb_mat[, q95_idx]
)

cat("Ensemble DRF (stacking) complete.\n\n")


# ============================================================
# MODEL 10: BNN — Bayes by Backprop (Blundell et al., ICML 2015)
# Exact implementation following the paper:
#
# 1. GAUSSIAN VARIATIONAL POSTERIOR (Section 3.2):
#    w = mu + sigma * epsilon, sigma = softplus(rho) = log(1+exp(rho))
#    Gradients: Delta_mu = df/dw + df/dmu
#               Delta_rho = df/dw * epsilon/(1+exp(-rho)) + df/drho
#
# 2. SCALE MIXTURE PRIOR (Section 3.3, Eq. 7):
#    P(w) = pi * N(0, sigma1^2) + (1-pi) * N(0, sigma2^2)
#    with sigma1 >> sigma2 (heavy + spike components)
#    Best in paper: -log(sigma1)∈{0,1,2}, -log(sigma2)∈{6,7,8}
#
# 3. MONTE CARLO ELBO ESTIMATE (Section 3.1, Eq. 2):
#    F(D,theta) ≈ Σ_i [log q(w^i|theta) - log P(w^i) - log P(D|w^i)]
#    where all terms use the SAME w^i (common random numbers)
#
# 4. KL RE-WEIGHTING (Section 3.4, Eq. 9):
#    Per minibatch: F_i = pi_i * KL - E[log P(D_i|w)]
#    with pi_i = (2^(M-i)) / (2^M - 1)  (paper's geometric schedule)
#
# 5. INFERENCE: Thompson sampling — draw w~q(w|theta), predict
#    Average over M_pass stochastic forward passes
#
# 6. OUTPUT: Gamma parametric (log_shape, log_rate) — aligns with
#    regression setting in Section 5.2 (Gaussian replaced by Gamma)
# ============================================================
cat("========================================\n")
cat("MODEL 10: BNN (Bayes by Backprop, Blundell et al. 2015)\n")
cat("========================================\n")

# ---- Scale mixture prior (Section 3.3, Eq. 7) ----
log_scale_mixture_prior <- function(w, pi_mix, log_sigma1, log_sigma2) {
  # log[ pi*N(w;0,exp(2*log_sigma1)) + (1-pi)*N(w;0,exp(2*log_sigma2)) ]
  # Numerically stable via log-sum-exp
  log_p1 <- log(pi_mix)  + dnorm(w, mean = 0, sd = exp(log_sigma1), log = TRUE)
  log_p2 <- log(1 - pi_mix) + dnorm(w, mean = 0, sd = exp(log_sigma2), log = TRUE)
  # log-sum-exp
  m  <- pmax(log_p1, log_p2)
  m + log(exp(log_p1 - m) + exp(log_p2 - m))
}

# ---- Bayesian linear layer (Section 3.2) ----
# Uses local reparameterisation: w = mu + softplus(rho) * epsilon
bnn_linear <- nn_module(
  classname  = "bnn_linear_bbb",
  initialize = function(in_f, out_f, pi_mix, log_sigma1, log_sigma2) {
    self$in_f      <- in_f
    self$out_f     <- out_f
    self$pi_mix    <- pi_mix
    self$log_sig1  <- log_sigma1
    self$log_sig2  <- log_sigma2
    # Variational posterior params (Eq. before Eq. 3 in paper)
    self$w_mu   <- nn_parameter(torch_randn(out_f, in_f)  * 0.01)
    self$w_rho  <- nn_parameter(torch_full(c(out_f, in_f), -3.0))
    self$b_mu   <- nn_parameter(torch_zeros(out_f))
    self$b_rho  <- nn_parameter(torch_full(c(out_f), -3.0))
  },
  forward = function(x) {
    # sigma = log(1 + exp(rho))  — Eq. after Section 3.2 heading
    w_sigma   <- torch_log1p(torch_exp(self$w_rho))
    b_sigma   <- torch_log1p(torch_exp(self$b_rho))
    # Reparameterisation: w = mu + sigma * epsilon, epsilon~N(0,I)
    w_epsilon <- torch_randn_like(w_sigma)
    b_epsilon <- torch_randn_like(b_sigma)
    self$w_sample <- self$w_mu + w_sigma * w_epsilon
    self$b_sample <- self$b_mu + b_sigma * b_epsilon
    nnf_linear(x, self$w_sample, self$b_sample)
  },
  # KL divergence term: log q(w|theta) - log P(w)
  # using the SAME weight samples drawn in forward() (common random numbers)
  kl_term = function() {
    w_sigma <- torch_log1p(torch_exp(self$w_rho))
    b_sigma <- torch_log1p(torch_exp(self$b_rho))
    
    # log q(w|mu, sigma): diagonal Gaussian
    log_qw <- torch_sum(
      -0.5 * log(2 * pi) - torch_log(w_sigma) -
        0.5 * ((self$w_sample - self$w_mu) / w_sigma)^2
    )
    log_qb <- torch_sum(
      -0.5 * log(2 * pi) - torch_log(b_sigma) -
        0.5 * ((self$b_sample - self$b_mu) / b_sigma)^2
    )
    log_q  <- log_qw + log_qb
    
    # log P(w): scale mixture prior (Eq. 7)
    w_vec   <- as.numeric(self$w_sample$to(device = "cpu"))
    b_vec   <- as.numeric(self$b_sample$to(device = "cpu"))
    log_pw  <- sum(log_scale_mixture_prior(w_vec, self$pi_mix, self$log_sig1, self$log_sig2))
    log_pb  <- sum(log_scale_mixture_prior(b_vec, self$pi_mix, self$log_sig1, self$log_sig2))
    log_p   <- torch_tensor(log_pw + log_pb, dtype = torch_float(), device = device)
    
    log_q - log_p   # KL contribution for this layer's weights
  }
)

bnn_gamma_net <- nn_module(
  classname  = "bnn_gamma_bbb",
  initialize = function(n_in, h1, h2, pi_mix, log_sigma1, log_sigma2) {
    self$l1 <- bnn_linear(n_in, h1, pi_mix, log_sigma1, log_sigma2)
    self$l2 <- bnn_linear(h1,   h2, pi_mix, log_sigma1, log_sigma2)
    self$l3 <- bnn_linear(h2,    2, pi_mix, log_sigma1, log_sigma2)
  },
  forward = function(x) {
    x <- nnf_relu(self$l1(x))
    x <- nnf_relu(self$l2(x))
    self$l3(x)
  },
  kl = function() { self$l1$kl_term() + self$l2$kl_term() + self$l3$kl_term() }
)

# ---- Training with KL re-weighting (Section 3.4, Eq. 9) ----
# pi_i = (2^(M-i)) / (2^M - 1)  — geometric schedule over M minibatches
train_bnn_bbb <- function(X_sc, y_obs, h1, h2, lr,
                          pi_mix, log_sigma1, log_sigma2,
                          epochs = 100, batch_size = 128, seed = 42) {
  set.seed(seed); torch_manual_seed(seed)
  N   <- nrow(X_sc)
  net <- bnn_gamma_net(n_input, h1, h2, pi_mix, log_sigma1, log_sigma2)$to(device = device)
  opt <- optim_adam(net$parameters, lr = lr)
  dl  <- make_dl(X_sc, y_obs, batch_size)
  
  # Pre-compute geometric KL weights for each minibatch position
  # (paper uses M minibatches per epoch; we re-index each epoch)
  n_batches <- ceiling(N / batch_size)
  kl_weights <- sapply(seq_len(n_batches), function(i) {
    (2^(n_batches - i)) / (2^n_batches - 1)
  })
  
  net$train()
  for (ep in seq_len(epochs)) {
    batch_cnt <- 0L
    coro::loop(for (b in dl) {
      batch_cnt  <- batch_cnt + 1L
      pi_i       <- kl_weights[min(batch_cnt, n_batches)]  # Eq. 9
      opt$zero_grad()
      params     <- net(b$x)
      nll        <- gamma_nll_loss(params, b$y)
      kl_contrib <- net$kl()
      # F_i = pi_i * KL - E[log P(D_i|w)]  (Eq. 9)
      loss <- pi_i * kl_contrib + nll
      loss$backward(); opt$step()
    })
    if (ep %% 25 == 0)
      cat(sprintf("    Epoch %d/%d\n", ep, epochs))
  }
  net$eval()
  net
}

# Inference: Thompson sampling (Section 4.1) —
# sample M_pass weight configurations, average predictions
bnn_avg_params_bbb <- function(net, X_sc, M_pass = 100) {
  net$train()  # keep dropout-style stochasticity ON for sampling
  X_t   <- torch_tensor(X_sc, dtype = torch_float(), device = device)
  acc_s <- matrix(0, nrow(X_sc), 1)
  acc_r <- matrix(0, nrow(X_sc), 1)
  for (p_i in seq_len(M_pass)) {
    with_no_grad({
      pm    <- as.matrix(net(X_t)$to(device = "cpu"))
    })
    acc_s <- acc_s + exp(pm[, 1, drop = FALSE])
    acc_r <- acc_r + exp(pm[, 2, drop = FALSE])
  }
  list(shape = as.vector(acc_s / M_pass), rate = as.vector(acc_r / M_pass))
}

# ---- HP tuning ----
# Paper (Table 1): compare Gaussian vs Scale-mixture prior
# Best: Scale mixture with -log(sigma1)∈{0,1,2}, -log(sigma2)∈{6,7,8}
# pi_mix (denoted pi in paper) ∈ {0.25, 0.5, 0.75}
cat("Tuning BNN (Bayes-by-Backprop) hyperparameters...\n")
bnn_hp_grid <- expand.grid(
  h1          = c(64, 128),
  h2          = c(32, 64),
  lr          = c(5e-4, 1e-3),
  pi_mix      = c(0.5, 0.75),       # Section 3.3: pi ∈ {1/4, 1/2, 3/4}
  neg_log_s1  = c(0, 1),            # -log(sigma1) ∈ {0,1,2}
  neg_log_s2  = c(6, 7),            # -log(sigma2) ∈ {6,7,8}
  stringsAsFactors = FALSE
)

bnn_cv_wis <- sapply(seq_len(nrow(bnn_hp_grid)), function(i) {
  hp <- bnn_hp_grid[i, ]
  cat(sprintf("  [%d/%d] h1=%d h2=%d lr=%.4g pi=%.2f sig1=e^%.0f sig2=e^%.0f ...",
              i, nrow(bnn_hp_grid),
              hp$h1, hp$h2, hp$lr, hp$pi_mix, -hp$neg_log_s1, -hp$neg_log_s2))
  tryCatch({
    net <- train_bnn_bbb(
      X_tr_cv, y_tr_cv,
      h1 = hp$h1, h2 = hp$h2, lr = hp$lr,
      pi_mix      = hp$pi_mix,
      log_sigma1  = -hp$neg_log_s1,  # sigma1 = exp(-neg_log_s1)
      log_sigma2  = -hp$neg_log_s2,
      epochs      = 50, batch_size = 128, seed = 2026 + i
    )
    p   <- bnn_avg_params_bbb(net, X_va_cv, M_pass = 20)
    pm  <- gamma_qgrid(p$shape, p$rate, qs)
    w   <- mean(scoringutils::wis(y_va_cv, pm, qs, na.rm = TRUE), na.rm = TRUE)
    cat(sprintf(" WIS=%.4f\n", w)); w
  }, error = function(e) { cat(sprintf(" ERROR: %s\n", conditionMessage(e))); Inf })
})

best_bnn_i  <- which.min(bnn_cv_wis)
best_bnn_hp <- as.list(bnn_hp_grid[best_bnn_i, ])
cat(sprintf("\n✓ Best BNN: h1=%d h2=%d lr=%.4g pi=%.2f -log(s1)=%g -log(s2)=%g (WIS=%.4f)\n",
            best_bnn_hp$h1, best_bnn_hp$h2, best_bnn_hp$lr,
            best_bnn_hp$pi_mix, best_bnn_hp$neg_log_s1, best_bnn_hp$neg_log_s2,
            bnn_cv_wis[best_bnn_i]))

# ---- Train final BNN (Bayes by Backprop) ----
set.seed(2026 + 77)
cat("Training final BNN (150 epochs, full train set)...\n")
bnn_net <- train_bnn_bbb(
  Xtr_ens_sc, y_tr,
  h1          = best_bnn_hp$h1,
  h2          = best_bnn_hp$h2,
  lr          = best_bnn_hp$lr,
  pi_mix      = best_bnn_hp$pi_mix,
  log_sigma1  = -best_bnn_hp$neg_log_s1,
  log_sigma2  = -best_bnn_hp$neg_log_s2,
  epochs      = 150, batch_size = 128, seed = 2026 + 77
)

# ---- Inference: average over M_pass=100 weight samples ----
cat("BNN inference (M=100 stochastic forward passes)...\n")
bnn_params_te  <- bnn_avg_params_bbb(bnn_net, Xte_ens_sc, M_pass = 100)
bnn_qgrid_mat  <- gamma_qgrid(bnn_params_te$shape, bnn_params_te$rate)
bnn_q          <- data.frame(q05 = bnn_qgrid_mat[, q05_idx],
                             q50 = bnn_qgrid_mat[, q50_idx],
                             q95 = bnn_qgrid_mat[, q95_idx])
bnn_qgrid      <- as.data.frame(bnn_qgrid_mat)
colnames(bnn_qgrid) <- paste0("q", sprintf("%02d", round(100 * q_grid)))

nll_bnn <- -mean(dgamma(y_te, shape = bnn_params_te$shape,
                        rate = bnn_params_te$rate, log = TRUE))
cat("BNN (Bayes by Backprop) complete.\n\n")


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
  # DDNN: per-run best HPs (N_runs_ddnn independent trials, Marcjasz et al. 2023)
  DDNN_Gamma   = best_ddnn_hp,
  # Ensemble_DRF: stacking with QR combiner (Papacharalampous et al. 2025)
  Ensemble_DRF = list(K = K_drf, num.trees = num_trees_drf,
                      min.node.size = min_node_size_drf, combiner = "linear_QR"),
  # BNN: Bayes-by-Backprop, scale mixture prior (Blundell et al. 2015)
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
  # DDNN: sample from mixture of N_runs Gamma distributions (qEns corresponds to
  # averaging quantile predictions; for simulation draw randomly from one run's dist)
  DDNN_Gamma   = {
    run_i <- sample(N_runs_ddnn, length(y_te) * n_spp, replace = TRUE)
    samp_mat <- matrix(NA_real_, length(y_te), n_spp)
    for (r in seq_len(N_runs_ddnn)) {
      idx_r <- which(run_i == r)
      if (length(idx_r) == 0) next
      pat_r <- ((idx_r - 1) %% length(y_te)) + 1
      samp_mat[cbind(pat_r, ceiling(idx_r / length(y_te)))] <-
        rgamma(length(idx_r),
               shape = ddnn_params_all[[r]]$shape[pat_r],
               rate  = ddnn_params_all[[r]]$rate[pat_r])
    }
    # Fallback: use mean params for any remaining NAs
    na_idx <- which(is.na(samp_mat))
    if (length(na_idx) > 0) {
      pat_na <- ((na_idx - 1) %% length(y_te)) + 1
      samp_mat[na_idx] <- rgamma(length(na_idx),
                                 shape = ddnn_shape_avg[pat_na],
                                 rate  = ddnn_rate_avg[pat_na])
    }
    samp_mat
  },
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
