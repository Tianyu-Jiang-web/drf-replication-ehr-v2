library(dplyr)
library(readr)
library(lubridate)

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
df2 <- df2 %>% select(-subject_id, -hadm_id, -subject_id_miss, -hadm_id_miss)

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


# 1-data split


# split data based on admission time
cutoff <- quantile(df2$admit_dt, 0.8, na.rm = TRUE)
# select data with 80% previous admission time as training set
idx_train <- which(df2$admit_dt <= cutoff)
# select data with 20% later admission time as test set
idx_test  <- which(df2$admit_dt >  cutoff)

# delete variable admit_dt
df2_model <- df2 %>% select(-admit_dt)

# update y and X_tree
y      <- df2_model$y
X_tree <- df2_model %>% select(-y)

# delete admit_dt from df_imp2 and X_all
df_imp2 <- df_imp %>%
  select(-any_of(c("admit_dt")))
X_all <- df_imp2 %>% select(-y)

# split X_tree training set and test set based on idx_train and idx_test
Xtr_tree <- X_tree[idx_train, ]
Xte_tree <- X_tree[idx_test, ]
# split y training set and test set based on idx_train and idx_test
y_tr     <- y[idx_train]
y_te     <- y[idx_test]
# split X_all training set and test set based on idx_train and idx_test
Xtr_all  <- X_all[idx_train, ]
Xte_all  <- X_all[idx_test, ]

# 2-fit drf model
library(drf)

set.seed(2026)

# fit DRF
drf_fit <- drf(
  X = Xtr_tree,
  Y = y_tr,
  num.trees = 1000,
  min.node.size = 50,
  mtry = floor(sqrt(ncol(Xtr_tree))),
  sample.fraction = 0.5
)

# set 3 quantile levels for calculating WIS
q_levels <- c(0.05, 0.5, 0.95)

# get the prediction outcome
# the outcome includes weights align with y and y
drf_pred <- predict(drf_fit, Xte_tree)

library(Matrix)

# set a range of quantiles level
q_grid <- seq(0.05, 0.95, by = 0.05)   # 19个分位数

# get weight from the outcome
W <- drf_pred$weights  # n_test x n_train, dgCMatrix
y_train <- as.numeric(drf_pred$y)  # get y_train from drf_pred to have the same sequence as weight

# build a function to get the los for selected quantile
wquant <- function(y, w, probs) {
  # order by los
  o <- order(y)
  y <- y[o]
  # select the weight which is matched with y's order
  w <- w[o]
  cw <- cumsum(w) / sum(w)
  # to get the exact y
  sapply(probs, function(p) y[which(cw >= p)[1]])
}

# calculate predicted LOS quantiles for each test observation
drf_q_mat <- t(sapply(1:nrow(W), function(i) {
  w <- as.numeric(W[i, ])
  wquant(y_train, w, q_levels)
}))

# transfer drf_q_mat as dataframe and name 3 columns (q05, q50, q95)
drf_q <- as.data.frame(drf_q_mat)
colnames(drf_q) <- c("q05","q50","q95")

# ---- DRF quantile grid predictions (NEW) ----
drf_qgrid_mat <- t(sapply(1:nrow(W), function(i) {
  w <- as.numeric(W[i, ])
  wquant(y_train, w, q_grid)
}))

# transfer drf_q_mat as dataframe
drf_qgrid <- as.data.frame(drf_qgrid_mat)
colnames(drf_qgrid) <- paste0("q", sprintf("%02d", round(100*q_grid)))


# 3-fit QRF
# Load qrf package
library(quantregForest)

# fit qrf model
# qrf keeps all training responses in each terminal node
qrf_fit <- quantregForest(
  x = Xtr_all,
  y = y_tr,
  ntree = 1000,
  nodesize = 50
)

# predict the outcome and store in dataframe format
qrf_q <- as.data.frame(
  predict(qrf_fit, Xte_all, what = q_levels)
)
colnames(qrf_q) <- c("q05","q50","q95")

# ---- QRF quantile grid predictions (NEW) ----
qrf_qgrid <- as.data.frame(predict(qrf_fit, Xte_all, what = q_grid))
colnames(qrf_qgrid) <- paste0("q", sprintf("%02d", round(100*q_grid)))


# 4-fit ranger
# loading the package
library(ranger)

# fit the random forest model
rf_q_fit <- ranger(
  x = Xtr_all,
  y = y_tr,
  num.trees = 1000,
  min.node.size = 50,
  quantreg = TRUE,
  keep.inbag = TRUE
)

# get the results and store in dataframe format
rf_q <- as.data.frame(
  predict(rf_q_fit, Xte_all, type = "quantiles", quantiles = q_levels)$predictions
)
colnames(rf_q) <- c("q05","q50","q95")

# ---- Ranger quantile grid predictions (NEW) ----
rf_qgrid <- as.data.frame(
  predict(rf_q_fit, Xte_all, type = "quantiles", quantiles = q_grid)$predictions
)
colnames(rf_qgrid) <- paste0("q", sprintf("%02d", round(100*q_grid)))


# 5-XGBoost + Conformal prediction

set.seed(2026)
# extract calibration set based on training set
n_tr <- length(y_tr)
cal_idx <- sample(seq_len(n_tr), size = floor(0.2 * n_tr))

# build calibration set
X_cal <- Xtr_all[cal_idx, ]
y_cal <- y_tr[cal_idx]

# build training set
X_tr2 <- Xtr_all[-cal_idx, ]
y_tr2 <- y_tr[-cal_idx]

# load the package
library(xgboost)

# apply one-hot code as XGBoost can't deal with factor
X_tr2_mm <- model.matrix(~ . - 1, data = X_tr2)
X_cal_mm <- model.matrix(~ . - 1, data = X_cal)
X_te_mm  <- model.matrix(~ . - 1, data = Xte_all)

# build DMatrix
dtrain <- xgb.DMatrix(X_tr2_mm, label = y_tr2)
dcal   <- xgb.DMatrix(X_cal_mm, label = y_cal)
dtest  <- xgb.DMatrix(X_te_mm)

# parameters setting
params <- list(
  objective = "reg:squarederror",
  max_depth = 6,
  eta = 0.05,
  subsample = 0.8,
  colsample_bytree = 0.8
)

# model training
xgb_fit <- xgb.train(
  params = params,
  data   = dtrain,
  nrounds = 300,
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

# combine the results as a table
rbind(
  DRF = unlist(res_drf),
  QRF = unlist(res_qrf),
  RF  = unlist(res_rf),
  XGB = unlist(res_xgb)
)

# library scoringutils to use wis function
install.packages("scoringutils")
library(scoringutils)

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
  XGB = wis_score(xgb_q, y_te)
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
  xgb_q95 = xgb_q$q95
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
    
    .groups = "drop"
  ) %>%
  # Convert wide model columns into long format
  pivot_longer(cols = c(DRF, QRF, RF, XGB), names_to = "model", values_to = "metrics") %>%
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
    
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(DRF, QRF, RF, XGB),
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
    xgb_w = xgb_q95 - xgb_q05
  ) %>%
  # Split each model's predictions into high vs low uncertainty 
  # based on interval width (median split)
  mutate(
    drf_uncert = ifelse(ntile(drf_w, 2) == 2, "High uncertainty", "Low uncertainty"),
    qrf_uncert = ifelse(ntile(qrf_w, 2) == 2, "High uncertainty", "Low uncertainty"),
    rf_uncert  = ifelse(ntile(rf_w,  2) == 2, "High uncertainty", "Low uncertainty"),
    xgb_uncert = ifelse(ntile(xgb_w, 2) == 2, "High uncertainty", "Low uncertainty")
  )

# calculate the sample siz
table(eval_hm$drf_uncert)
table(eval_hm$qrf_uncert)
table(eval_hm$rf_uncert)
table(eval_hm$xgb_uncert)

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
    XGB_high = wis_subset(y, xgb_q05, xgb_q50, xgb_q95, xgb_uncert=="High uncertainty")
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
  model = c("DRF","QRF","RF","XGB"),
  low_n  = c(sum(eval_hm$drf_uncert=="Low uncertainty"),
             sum(eval_hm$qrf_uncert=="Low uncertainty"),
             sum(eval_hm$rf_uncert=="Low uncertainty"),
             sum(eval_hm$xgb_uncert=="Low uncertainty")),
  high_n = c(sum(eval_hm$drf_uncert=="High uncertainty"),
             sum(eval_hm$qrf_uncert=="High uncertainty"),
             sum(eval_hm$rf_uncert=="High uncertainty"),
             sum(eval_hm$xgb_uncert=="High uncertainty"))
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
    mutate(model = "XGB", uncert = "High")
  
)

out_uncert_cov

# draw the plot for the results
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
  XGB = crps_from_quantiles(eval_df$y, as.matrix(xgb_qgrid), q_grid)
)

crps_all_grid



# ----------
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
    xgb_q05, xgb_q95
  ) %>%
  mutate(
    drf_w = drf_q95 - drf_q05,
    qrf_w = qrf_q95 - qrf_q05,
    rf_w  = rf_q95  - rf_q05,
    xgb_w = xgb_q95 - xgb_q05,
    
    drf_cov = as.integer(y >= drf_q05 & y <= drf_q95),
    qrf_cov = as.integer(y >= qrf_q05 & y <= qrf_q95),
    rf_cov  = as.integer(y >= rf_q05  & y <= rf_q95),
    xgb_cov = as.integer(y >= xgb_q05 & y <= xgb_q95)
  ) %>%
  select(
    y,
    drf_w, drf_cov,
    qrf_w, qrf_cov,
    rf_w,  rf_cov,
    xgb_w, xgb_cov
  ) %>%
  pivot_longer(
    cols = -y,
    names_to = c("model", ".value"),
    names_pattern = "^(drf|qrf|rf|xgb)_(w|cov)$"
  ) %>%
  mutate(
    model = recode(model,
                   drf = "DRF", qrf = "QRF", rf = "RF", xgb = "XGB")
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
    xgb_q05, xgb_q50, xgb_q95
  ) %>%
  # Compute interval width (uncertainty proxy)
  mutate(
    drf_w = drf_q95 - drf_q05,
    qrf_w = qrf_q95 - qrf_q05,
    rf_w  = rf_q95  - rf_q05,
    xgb_w = xgb_q95 - xgb_q05,
    # Compute coverage indicator (1 = covered, 0 = not covered)
    drf_cov = as.integer(y >= drf_q05 & y <= drf_q95),
    qrf_cov = as.integer(y >= qrf_q05 & y <= qrf_q95),
    rf_cov  = as.integer(y >= rf_q05  & y <= rf_q95),
    xgb_cov = as.integer(y >= xgb_q05 & y <= xgb_q95)
  ) %>%
  select(
    y,
    drf_q50, drf_w, drf_cov,
    qrf_q50, qrf_w, qrf_cov,
    rf_q50,  rf_w,  rf_cov,
    xgb_q50, xgb_w, xgb_cov
  ) %>%
  pivot_longer(
    cols = -y,
    names_to = c("model", ".value"),
    names_pattern = "^(drf|qrf|rf|xgb)_(q50|w|cov)$"
  ) %>%
  mutate(
    model = recode(model, drf="DRF", qrf="QRF", rf="RF", xgb="XGB")
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



