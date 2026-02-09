library(dplyr)
library(readr)
library(lubridate)


df <- read.csv("data/final_master_dataset_9Dec2025 1(in).csv") %>%
  filter(!is.na(los))

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

df2 <- df %>%
  mutate(
    race = trimws(as.character(race)),
    race_clean = dplyr::recode(race, !!!race_mapping, .default = "Other"),
    race_clean = factor(race_clean,
                        levels = c("White", "Black", "Asian",
                                   "Hispanic/Latino", "Other", "Unknown")),
    y = los
  )

drop_cols <- c(
  "stay_id",
  "race",
  "admittime","dischtime","icu_intime_first","icu_outtime_first","intime",
  "primary_icd_long_title","primary_icd_code",
  "readmit_30d","readmit_90d","readmit_180d",
  "los"
)

df2 <- df2 %>% select(-any_of(drop_cols))

# -----------------------
# 2) labs：只保留你指定的 labs（优先 *_latest），并手动删除多余亚型
# -----------------------

# 2.1 先手动删掉你不想要的亚型（含其 *_latest）
df2 <- df2 %>% select(
  -Albumin_Blood_latest,
  
  -Potassium_Ascites, -Potassium_Ascites_latest,
  -Potassium_BodyFluid, -Potassium_BodyFluid_latest,
  -Potassium_Pleural, -Potassium_Pleural_latest,
  -Potassium_Stool, -Potassium_Stool_latest,
  -Potassium_Urine, -Potassium_Urine_latest,
  -Potassium_WholeBlood, -Potassium_WholeBlood_latest,
  -Potassium_CSF_latest,
  -Potassium_JointFluid_latest,
  
  -Sodium_Ascites, -Sodium_Ascites_latest,
  -Sodium_BodyFluid, -Sodium_BodyFluid_latest,
  -Sodium_Pleural, -Sodium_Pleural_latest,
  -Sodium_Stool, -Sodium_Stool_latest,
  -Sodium_Urine, -Sodium_Urine_latest,
  -Sodium_WholeBlood, -Sodium_WholeBlood_latest,
  -Sodium_CSF_latest,
  -Sodium_JointFluid_latest,

  -Triglycerides_Ascites, -Triglycerides_Ascites_latest,
  -Triglycerides_Pleural, -Triglycerides_Pleural_latest,
  -Triglycerides_CSF_latest,
  -Triglycerides_JointFluid_latest,
  -Triglycerides_Stool_latest,
  
  -Urea.Nitrogen_Ascites, -Urea.Nitrogen_Ascites_latest,
  -Urea.Nitrogen_Body.Fluid, -Urea.Nitrogen_Body.Fluid_latest,
  -Urea.Nitrogen_Pleural, -Urea.Nitrogen_Pleural_latest,
  -Urea.Nitrogen_Urine, -Urea.Nitrogen_Urine_latest,
  -Urea.Nitrogen_CSF_latest,
  -Urea.Nitrogen_Joint.Fluid_latest
  )

# 2.2 只对你指定的“主 labs”做 latest -> base（无循环版，逐条写清楚）
#     规则：如果 *_latest 存在，用 *_latest 替代 base；否则保留 base（如果存在）
df2 <- df2 %>%
  mutate(
    Albumin            = coalesce(Albumin_latest, Albumin),
    Creatinine         = coalesce(Creatinine_latest, Creatinine),
    HDL                = coalesce(HDL_latest, HDL),
    INR                = coalesce(INR_latest, INR),
    LDL                = coalesce(LDL_latest, LDL),
    PT                 = coalesce(PT_latest, PT),
    PTT                = coalesce(PTT_latest, PTT),
    Phosphate          = coalesce(Phosphate_latest, Phosphate),
    Potassium_Blood    = coalesce(Potassium_Blood_latest, Potassium_Blood),
    Sodium_Blood       = coalesce(Sodium_Blood_latest, Sodium_Blood),
    Temperature        = coalesce(Temperature_latest, Temperature),
    TotalChol          = coalesce(TotalChol_latest, TotalChol),
    Triglycerides_Blood= coalesce(Triglycerides_Blood_latest, Triglycerides_Blood),
    Urea.Nitrogen      = coalesce(Urea.Nitrogen_latest, Urea.Nitrogen),
    WBC                = coalesce(WBC_latest, WBC),
    eGFR               = coalesce(eGFR_latest, eGFR)
  ) %>%
  # 2.3 再把这些 *_latest 列删掉（避免残留）
  select(
    -Albumin_latest,
    -Creatinine_latest,
    -HDL_latest,
    -INR_latest,
    -LDL_latest,
    -PT_latest,
    -PTT_latest,
    -Phosphate_latest,
    -Potassium_Blood_latest,
    -Sodium_Blood_latest,
    -Temperature_latest,
    -TotalChol_latest,
    -Triglycerides_Blood_latest,
    -Urea.Nitrogen_latest,
    -WBC_latest,
    -eGFR_latest
  )

# -----------------------
# 3) 分类变量转 factor
# -----------------------
cat_cols <- intersect(c("race_clean","marital_status","insurance","gender","first_careunit"), names(df2))
df2 <- df2 %>% mutate(across(all_of(cat_cols), ~as.factor(trimws(.x))))

# -----------------------
# 4) 缺失指示器：对数值列生成 *_miss
#    (不填补也行，但 indicator 让比较更公平)
# -----------------------
num_cols <- names(df2)[sapply(df2, is.numeric)]
num_cols <- setdiff(num_cols, "y")

for (v in num_cols) {
  df2[[paste0(v, "_miss")]] <- as.integer(is.na(df2[[v]]))
}

# -----------------------
# 5) 给非树模型准备一个“能训练”的填补版本（保留 miss 指示器）
#    (树模型可直接用原 df2; 但 baseline 一般需要这个)
# -----------------------

df_time <- df %>%
  mutate(admit_dt = mdy_hm(admittime)) %>%
  select(subject_id, hadm_id, admit_dt)

df2 <- df2 %>%
  left_join(df_time, by = c("subject_id","hadm_id"))
df2 <- df2 %>% select(-subject_id, -hadm_id, -subject_id_miss, -hadm_id_miss)

df_imp <- df2
for (v in num_cols) {
  med <- median(df_imp[[v]], na.rm = TRUE)
  df_imp[[v]][is.na(df_imp[[v]])] <- med
}
# 分类缺失：显式一个水平
for (v in cat_cols) {
  df_imp[[v]] <- addNA(df_imp[[v]])
  levels(df_imp[[v]])[is.na(levels(df_imp[[v]]))] <- "Unknown"
}


# 最终：
y <- df2$y
X_tree <- df2 %>% select(-y)     # DRF / 树模型用（允许 NA）
X_all  <- df_imp %>% select(-y)  # 需要无 NA 的 baseline 用


#-----------------------------------
# Model training part
#-----------------------------------


# 1-data split
library(lubridate)


# ---- time split (用 admit_dt 只做划分) ----
cutoff <- quantile(df2$admit_dt, 0.8, na.rm = TRUE)
idx_train <- which(df2$admit_dt <= cutoff)
idx_test  <- which(df2$admit_dt >  cutoff)

# ---- 进入模型前，删掉 admit_dt（不当特征）----
df2_model <- df2 %>% select(-admit_dt)

# 最终：
y      <- df2_model$y
X_tree <- df2_model %>% select(-y)      # DRF/tree 用（允许 NA）

# ⚠️ baseline 的 df_imp 也要同步删掉 id + admit_dt（见下一段）
df_imp2 <- df_imp %>%
  select(-any_of(c("subject_id","hadm_id","subject_id_miss","hadm_id_miss","admit_dt")))  # admit_dt 不存在也没关系
X_all <- df_imp2 %>% select(-y)

# split
Xtr_tree <- X_tree[idx_train, ]
Xte_tree <- X_tree[idx_test, ]
y_tr     <- y[idx_train]
y_te     <- y[idx_test]

Xtr_all  <- X_all[idx_train, ]
Xte_all  <- X_all[idx_test, ]

# 2-fit drf model
library(drf)

set.seed(2026)

drf_fit <- drf(
  X = Xtr_tree,
  Y = y_tr,
  num.trees = 1000,
  min.node.size = 50,
  mtry = floor(sqrt(ncol(Xtr_tree))),
  sample.fraction = 0.5
)


q_levels <- c(0.05, 0.5, 0.95)

drf_pred <- predict(drf_fit, Xte_tree, quantiles = q_levels)

library(Matrix)

q_levels <- c(0.05, 0.5, 0.95)

# drf_pred <- predict(drf_fit, Xte_tree)  # 你已经有了
W <- drf_pred$weights      # n_test x n_train, dgCMatrix
y_train <- as.numeric(drf_pred$y)  # 训练集 y（LOS）

# 加权分位数：输入 y_train 和一条权重向量 w
wquant <- function(y, w, probs) {
  # w 是 numeric 向量（长度 n_train）
  o <- order(y)
  y <- y[o]
  w <- w[o]
  cw <- cumsum(w) / sum(w)
  sapply(probs, function(p) y[which(cw >= p)[1]])
}

# 从稀疏矩阵取每一行的权重并计算分位数
drf_q_mat <- t(sapply(1:nrow(W), function(i) {
  w <- as.numeric(W[i, ])
  wquant(y_train, w, q_levels)
}))

drf_q <- as.data.frame(drf_q_mat)
colnames(drf_q) <- c("q05","q50","q95")
# 3-fit QRF

library(quantregForest)

qrf_fit <- quantregForest(
  x = Xtr_all,
  y = y_tr,
  ntree = 1000,
  nodesize = 50
)

qrf_q <- as.data.frame(
  predict(qrf_fit, Xte_all, what = q_levels)
)
colnames(qrf_q) <- c("q05","q50","q95")


# 4-fit ranger

library(ranger)

rf_q_fit <- ranger(
  x = Xtr_all,
  y = y_tr,
  num.trees = 1000,
  min.node.size = 50,
  quantreg = TRUE,
  keep.inbag = TRUE
)

rf_q <- as.data.frame(
  predict(rf_q_fit, Xte_all, type = "quantiles", quantiles = q_levels)$predictions
)
colnames(rf_q) <- c("q05","q50","q95")


# 5-XGBoost + Conformal prediction

set.seed(2026)
n_tr <- length(y_tr)
cal_idx <- sample(seq_len(n_tr), size = floor(0.2 * n_tr))

X_cal <- Xtr_all[cal_idx, ]
y_cal <- y_tr[cal_idx]

X_tr2 <- Xtr_all[-cal_idx, ]
y_tr2 <- y_tr[-cal_idx]


library(xgboost)

# 构建设计矩阵（自动 one-hot）
X_tr2_mm <- model.matrix(~ . - 1, data = X_tr2)
X_cal_mm <- model.matrix(~ . - 1, data = X_cal)
X_te_mm  <- model.matrix(~ . - 1, data = Xte_all)

dtrain <- xgb.DMatrix(X_tr2_mm, label = y_tr2)
dcal   <- xgb.DMatrix(X_cal_mm, label = y_cal)
dtest  <- xgb.DMatrix(X_te_mm)

params <- list(
  objective = "reg:squarederror",
  max_depth = 6,
  eta = 0.05,
  subsample = 0.8,
  colsample_bytree = 0.8
)

xgb_fit <- xgb.train(
  params = params,
  data   = dtrain,
  nrounds = 300,
  verbose = 0
)

pred_cal <- predict(xgb_fit, dcal)
resid <- abs(y_cal - pred_cal)

alpha <- 0.1
q_hat <- quantile(resid, 1 - alpha)

pred_test <- predict(xgb_fit, dtest)

xgb_q <- data.frame(
  q05 = pred_test - q_hat,
  q50 = pred_test,
  q95 = pred_test + q_hat
)

# evaluation

eval_interval <- function(q, y) {
  covered <- (y >= q$q05) & (y <= q$q95)
  list(
    coverage = mean(covered),
    width    = mean(q$q95 - q$q05)
  )
}

res_drf <- eval_interval(drf_q, y_te)
res_qrf <- eval_interval(qrf_q, y_te)
res_rf  <- eval_interval(rf_q,  y_te)
res_xgb <- eval_interval(xgb_q, y_te)

rbind(
  DRF = unlist(res_drf),
  QRF = unlist(res_qrf),
  RF  = unlist(res_rf),
  XGB = unlist(res_xgb)
)


install.packages("scoringutils")
library(scoringutils)

wis_score <- function(q, y, na.rm = TRUE) {
  pred <- as.matrix(q[, c("q05", "q50", "q95")])
  scoringutils::wis(
    observed = y,
    predicted = pred,
    quantile_level = c(0.05, 0.5, 0.95),
    na.rm = na.rm
  ) |> mean(na.rm = na.rm)
}

c(
  DRF = wis_score(drf_q, y_te),
  QRF = wis_score(qrf_q, y_te),
  RF  = wis_score(rf_q,  y_te),
  XGB = wis_score(xgb_q, y_te)
)

# --------- Subgroup analysis ----------------#

eval_df <- data.frame(
  y = y_te,
  
  drf_q05 = drf_q$q05,
  drf_q50 = drf_q$q50,
  drf_q95 = drf_q$q95,
  
  qrf_q05 = qrf_q$q05,
  qrf_q50 = qrf_q$q50,
  qrf_q95 = qrf_q$q95,
  
  rf_q05  = rf_q$q05,
  rf_q50  = rf_q$q50,
  rf_q95  = rf_q$q95,
  
  xgb_q05 = xgb_q$q05,
  xgb_q50 = xgb_q$q50,
  xgb_q95 = xgb_q$q95
)

eval_df$los_group <- cut(
  eval_df$y,
  breaks = c(-Inf, 3, 7, Inf),
  labels = c("Short (≤3d)", "Medium (3–7d)", "Long (>7d)")
)

# 用 Xte_tree（保留 NA 的版本）
miss_mat <- is.na(Xte_tree)

eval_df$miss_rate <- rowMeans(miss_mat)

eval_df$miss_group <- cut(
  eval_df$miss_rate,
  breaks = quantile(eval_df$miss_rate, probs = c(0, 1/3, 2/3, 1)),
  labels = c("Low missing", "Mid missing", "High missing"),
  include.lowest = TRUE
)

eval_df$vent_group <- factor(
  Xte_tree$vent_any_flag,
  levels = c(0, 1),
  labels = c("No ventilation", "Ventilated")
)

eval_df$vaso_group <- factor(
  Xte_tree$vasopressors,
  levels = c(0, 1),
  labels = c("No vasopressors", "Vasopressors")
)

library(dplyr)
library(tidyr)

eval_metrics_vec <- function(q05, q95, y) {
  covered <- (y >= q05) & (y <= q95)
  tibble(
    coverage = mean(covered, na.rm = TRUE),
    width    = mean(q95 - q05, na.rm = TRUE),
    n_used   = sum(!is.na(covered))
  )
}

out_miss_long <- eval_df %>%
  group_by(miss_group) %>%
  summarise(
    n_group = n(),
    
    DRF = list(eval_metrics_vec(.data$drf_q05, .data$drf_q95, .data$y)),
    QRF = list(eval_metrics_vec(.data$qrf_q05, .data$qrf_q95, .data$y)),
    RF  = list(eval_metrics_vec(.data$rf_q05,  .data$rf_q95,  .data$y)),
    XGB = list(eval_metrics_vec(.data$xgb_q05, .data$xgb_q95, .data$y)),
    
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(DRF, QRF, RF, XGB), names_to = "model", values_to = "metrics") %>%
  unnest(metrics)

out_miss_long


library(dplyr)

wis_one <- function(y, q05, q50, q95) {
  pred <- cbind(q05, q50, q95)
  qs   <- c(0.05, 0.5, 0.95)
  mean(scoringutils::wis(
    observed = y,
    predicted = pred,
    quantile_level = qs,
    na.rm = TRUE
  ))
}

out_miss_wis <- eval_df %>%
  group_by(miss_group) %>%
  summarise(
    DRF = wis_one(y, drf_q05, drf_q50, drf_q95),
    QRF = wis_one(y, qrf_q05, qrf_q50, qrf_q95),
    RF  = wis_one(y, rf_q05,  rf_q50,  rf_q95),
    XGB = wis_one(y, xgb_q05, xgb_q50, xgb_q95),
    .groups = "drop"
  ) %>%
  pivot_longer(-miss_group, names_to = "model", values_to = "WIS")

out_miss_wis

# group by ventilation / vasopressors

out_vent_long <- eval_df %>%
  group_by(vent_group) %>%
  summarise(
    n_group = n(),
    
    DRF = list(eval_metrics_vec(drf_q05, drf_q95, y)),
    QRF = list(eval_metrics_vec(qrf_q05, qrf_q95, y)),
    RF  = list(eval_metrics_vec(rf_q05,  rf_q95,  y)),
    XGB = list(eval_metrics_vec(xgb_q05, xgb_q95, y)),
    
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(DRF, QRF, RF, XGB),
               names_to = "model",
               values_to = "metrics") %>%
  unnest(metrics)

out_vent_long

out_vent_wis <- eval_df %>%
  group_by(vent_group) %>%
  summarise(
    DRF = wis_one(y, drf_q05, drf_q50, drf_q95),
    QRF = wis_one(y, qrf_q05, qrf_q50, qrf_q95),
    RF  = wis_one(y, rf_q05,  rf_q50,  rf_q95),
    XGB = wis_one(y, xgb_q05, xgb_q50, xgb_q95),
    .groups = "drop"
  ) %>%
  pivot_longer(-vent_group, names_to = "model", values_to = "WIS")

out_vent_wis


out_vaso_wis <- eval_df %>%
  group_by(vaso_group) %>%
  summarise(
    DRF = wis_one(y, drf_q05, drf_q50, drf_q95),
    QRF = wis_one(y, qrf_q05, qrf_q50, qrf_q95),
    RF  = wis_one(y, rf_q05,  rf_q50,  rf_q95),
    XGB = wis_one(y, xgb_q05, xgb_q50, xgb_q95),
    .groups = "drop"
  ) %>%
  pivot_longer(-vaso_group, names_to = "model", values_to = "WIS")

out_vaso_wis
