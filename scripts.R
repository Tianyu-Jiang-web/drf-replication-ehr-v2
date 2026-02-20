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
df2 <- df2 %>% select(-subject_id, -hadm_id, -subject_id_miss, -hadm_id_first_miss)

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

# 6-Deep Ensembles (Lakshminarayanan et al., NIPS 2017)
# Each network outputs mean mu(x) and variance sigma^2(x)
# Trained with Gaussian NLL as the proper scoring rule
# Ensemble prediction: mixture of Gaussians -> quantiles via qnorm

library(torch)

set.seed(2026)

# Number of networks in the ensemble (paper recommends M=5)
M_ens <- 5

# Helper: enforce softplus positivity on variance output
softplus <- function(x) log(1 + exp(x))

# One-hot encode categorical variables (same as XGBoost section)
# Deep Ensembles requires fully numeric input matrix
Xtr_ens_mm <- model.matrix(~ . - 1, data = Xtr_all)
Xte_ens_mm <- model.matrix(~ . - 1, data = Xte_all)

# Standardise inputs (improves NN training stability)
x_mean <- colMeans(Xtr_ens_mm)
x_sd   <- apply(Xtr_ens_mm, 2, sd)
x_sd[x_sd == 0] <- 1   # avoid division by zero for constant columns

Xtr_ens_sc <- scale(Xtr_ens_mm, center = x_mean, scale = x_sd)
Xte_ens_sc <- scale(Xte_ens_mm, center = x_mean, scale = x_sd)

# Standardise outcome (helps NLL training; predictions are back-transformed)
y_mean_ens <- mean(y_tr)
y_sd_ens   <- sd(y_tr)
y_tr_sc    <- (y_tr - y_mean_ens) / y_sd_ens

n_input <- ncol(Xtr_ens_sc)

# Convert to torch tensors
X_tr_t <- torch_tensor(Xtr_ens_sc, dtype = torch_float())
y_tr_t <- torch_tensor(matrix(y_tr_sc, ncol = 1), dtype = torch_float())
X_te_t <- torch_tensor(Xte_ens_sc, dtype = torch_float())

# Define a single probabilistic NN: outputs (mu, log_var)
# Architecture: input -> 128 -> 64 -> 2  (with ReLU activations)
make_prob_net <- function(n_in) {
  nn_sequential(
    nn_linear(n_in, 128),
    nn_relu(),
    nn_linear(128, 64),
    nn_relu(),
    nn_linear(64, 2)   # output: [mu, raw_var (pre-softplus)]
  )
}

# Gaussian NLL loss (proper scoring rule, Eq.1 in paper)
gauss_nll <- function(pred, y_true) {
  mu      <- pred[, 1, drop = FALSE]
  log_var <- pred[, 2, drop = FALSE]
  # softplus to ensure variance > 0, add small epsilon for stability
  var     <- torch_log1p(torch_exp(log_var)) + 1e-6
  loss    <- torch_mean(log_var + (y_true - mu)^2 / var)
  loss
}

# Train M independent networks
ensemble_nets <- vector("list", M_ens)

for (m in seq_len(M_ens)) {
  cat(sprintf("Training ensemble member %d / %d ...\n", m, M_ens))
  
  net   <- make_prob_net(n_input)
  optim <- optim_adam(net$parameters, lr = 0.001)
  
  net$train()
  for (epoch in seq_len(100)) {
    optim$zero_grad()
    pred <- net(X_tr_t)
    loss <- gauss_nll(pred, y_tr_t)
    loss$backward()
    optim$step()
  }
  
  net$eval()
  ensemble_nets[[m]] <- net
}

# Predict: collect (mu_m, sigma2_m) from each network on the test set
with_no_grad({
  preds_list <- lapply(ensemble_nets, function(net) {
    out    <- net(X_te_t)
    mu_sc  <- as.numeric(out[, 1])
    var_sc <- as.numeric(softplus(as.numeric(out[, 2]))) + 1e-6
    list(mu = mu_sc, var = var_sc)
  })
})

# Mixture-of-Gaussians aggregation (Section 2.4 in paper):
#   mu*  = mean of mu_m
#   var* = mean(var_m + mu_m^2) - mu*^2
mu_mat  <- sapply(preds_list, `[[`, "mu")   # n_test x M
var_mat <- sapply(preds_list, `[[`, "var")

mu_ens_sc  <- rowMeans(mu_mat)
var_ens_sc <- rowMeans(var_mat + mu_mat^2) - mu_ens_sc^2
var_ens_sc <- pmax(var_ens_sc, 1e-6)       # numerical safety
sd_ens_sc  <- sqrt(var_ens_sc)

# Back-transform to original LOS scale
mu_ens  <- mu_ens_sc  * y_sd_ens + y_mean_ens
sd_ens  <- sd_ens_sc  * y_sd_ens

# Extract quantiles from the mixture-Gaussian (Gaussian approximation)
lower_bound_ens <- min(y_tr, na.rm = TRUE)
ens_q <- data.frame(
  q05 = pmax(qnorm(0.05, mean = mu_ens, sd = sd_ens), lower_bound_ens),
  q50 = qnorm(0.50, mean = mu_ens, sd = sd_ens),
  q95 = qnorm(0.95, mean = mu_ens, sd = sd_ens)
)

# Dense quantile grid for CRPS
ens_qgrid_mat <- sapply(q_grid, function(p)
  pmax(qnorm(p, mean = mu_ens, sd = sd_ens), lower_bound_ens))
ens_qgrid <- as.data.frame(ens_qgrid_mat)
colnames(ens_qgrid) <- paste0("q", sprintf("%02d", round(100 * q_grid)))

# 7-MC Dropout (Gal & Ghahramani, 2016)
# Approximate Bayesian inference via dropout at test time
# Keep dropout enabled during prediction and sample M times
# Aggregate predictions to estimate predictive uncertainty

cat("\nTraining MC Dropout model...\n")

set.seed(2026)

# Number of forward passes at test time (paper recommends 50-100)
M_dropout <- 100

# Dropout rate (common choices: 0.1 for regression, 0.5 for classification)
dropout_rate <- 0.1

# Define network with dropout layers
# Architecture matches Deep Ensembles for fair comparison
mc_dropout_net <- nn_sequential(
  nn_linear(n_input, 128),
  nn_relu(),
  nn_dropout(p = dropout_rate),
  nn_linear(128, 64),
  nn_relu(),
  nn_dropout(p = dropout_rate),
  nn_linear(64, 2)   # output: [mu, raw_var]
)

# Train with dropout enabled (standard training)
optim_mc <- optim_adam(mc_dropout_net$parameters, lr = 0.001)

mc_dropout_net$train()
for (epoch in seq_len(100)) {
  optim_mc$zero_grad()
  pred <- mc_dropout_net(X_tr_t)
  loss <- gauss_nll(pred, y_tr_t)
  loss$backward()
  optim_mc$step()
  
  if (epoch %% 20 == 0) {
    cat(sprintf("  Epoch %d, Loss: %.4f\n", epoch, as.numeric(loss)))
  }
}

# CRITICAL: Keep dropout ENABLED at test time (this is the MC part)
# Each forward pass samples a different dropout mask
mc_dropout_net$train()  # NOT eval()! This keeps dropout active

cat(sprintf("Generating %d MC samples...\n", M_dropout))

# Collect M stochastic forward passes
mc_mu_samples  <- matrix(0, nrow = nrow(Xte_ens_sc), ncol = M_dropout)
mc_var_samples <- matrix(0, nrow = nrow(Xte_ens_sc), ncol = M_dropout)

with_no_grad({
  for (i in seq_len(M_dropout)) {
    pred_i <- mc_dropout_net(X_te_t)
    mc_mu_samples[, i]  <- as.numeric(pred_i[, 1])
    mc_var_samples[, i] <- as.numeric(softplus(as.numeric(pred_i[, 2]))) + 1e-6
  }
})

# Aggregate MC samples: mean of means, and total variance
# Total variance = epistemic (variance of means) + aleatoric (mean of variances)
mu_mc_sc  <- rowMeans(mc_mu_samples)
var_epistemic <- apply(mc_mu_samples, 1, var)         # model uncertainty
var_aleatoric <- rowMeans(mc_var_samples)             # data uncertainty
var_mc_sc <- var_epistemic + var_aleatoric
var_mc_sc <- pmax(var_mc_sc, 1e-6)
sd_mc_sc  <- sqrt(var_mc_sc)

# Back-transform to original LOS scale
mu_mc  <- mu_mc_sc  * y_sd_ens + y_mean_ens
sd_mc  <- sd_mc_sc  * y_sd_ens

# Extract quantiles (assuming Gaussian predictive distribution)
lower_bound_mc <- min(y_tr, na.rm = TRUE)
mcd_q <- data.frame(
  q05 = pmax(qnorm(0.05, mean = mu_mc, sd = sd_mc), lower_bound_mc),
  q50 = qnorm(0.50, mean = mu_mc, sd = sd_mc),
  q95 = qnorm(0.95, mean = mu_mc, sd = sd_mc)
)

# Dense quantile grid for CRPS
mcd_qgrid_mat <- sapply(q_grid, function(p)
  pmax(qnorm(p, mean = mu_mc, sd = sd_mc), lower_bound_mc))
mcd_qgrid <- as.data.frame(mcd_qgrid_mat)
colnames(mcd_qgrid) <- paste0("q", sprintf("%02d", round(100 * q_grid)))

cat("MC Dropout training complete.\n\n")

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
# For DRF/QRF/RF: approximate density from quantile grid via numerical differentiation
# For ENS/MCD: use analytical Gaussian likelihood
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

# ENS: analytical Gaussian NLL
# NLL = 0.5 * log(2π) + 0.5 * log(σ²) + 0.5 * (y - μ)² / σ²
nll_ens <- mean(0.5 * log(2 * pi) + log(sd_ens) + 0.5 * ((y_te - mu_ens) / sd_ens)^2)

# MCD: analytical Gaussian NLL
nll_mcd <- mean(0.5 * log(2 * pi) + log(sd_mc) + 0.5 * ((y_te - mu_mc) / sd_mc)^2)

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

# For Gaussian models: sample from N(μ, σ²)
ens_samples <- matrix(rnorm(length(y_te) * n_samples_per_patient, 
                            mean = rep(mu_ens, each = n_samples_per_patient),
                            sd = rep(sd_ens, each = n_samples_per_patient)),
                      nrow = length(y_te), ncol = n_samples_per_patient, byrow = TRUE)

mcd_samples <- matrix(rnorm(length(y_te) * n_samples_per_patient,
                            mean = rep(mu_mc, each = n_samples_per_patient),
                            sd = rep(sd_mc, each = n_samples_per_patient)),
                      nrow = length(y_te), ncol = n_samples_per_patient, byrow = TRUE)

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
    c("Actual", "DRF", "QRF", "RF", "ENS (Gaussian)", "MCD (Gaussian)"),
    times = c(length(y_te), rep(length(y_te) * n_samples_per_patient, 5))
  )
)

# Remove extreme outliers for better visualization (optional)
df_dist <- df_dist %>% filter(LOS >= 0, LOS <= quantile(y_te, 0.99) * 1.5)

cat(sprintf("Total samples: %d\n", nrow(df_dist)))


# ---- 最终学术期刊风格对比图：分面+背景真值对比 ----

# 1. 数据预处理（复用你之前的 df_dist）
# 确保 Model 是有序因子，方便排序展示
df_dist$Model <- factor(df_dist$Model, levels = c("Actual", "DRF", "QRF", "RF", "ENS (Gaussian)", "MCD (Gaussian)"))

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
    "ENS (Gaussian)" = "#984EA3", 
    "MCD (Gaussian)" = "#FF7F00"
  )) +
  
  scale_color_manual(values = c(
    "DRF" = "#D65C5C",        # 中等红
    "QRF" = "#5A9BD4",        # 中等蓝
    "RF"  = "#66BB66",        # 中等绿
    "ENS (Gaussian)" = "#7A3E82", 
    "MCD (Gaussian)" = "#CC6600"
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



