library(dplyr)
library(readr)
library(lubridate)
library(tidyr)
# read the dataset
df <- read.csv("data/FINAL_master_dataset_9Feb2026(in).csv") %>%
  filter(!is.na(icu_los_days))

# ---- fix impossible RR=0 (treat as missing) ----
if ("icu_rr_last24h_final" %in% names(df)) {
  df$icu_rr_last24h_final[df$icu_rr_last24h_final == 0] <- NA
} else if ("icu_rr_last24h" %in% names(df)) {
  df$icu_rr_last24h[df$icu_rr_last24h == 0] <- NA
}

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
# ---- drop no-information / unwanted cols ----
extra_drop <- c("arni", "mask_vent_or_intub", "icd_flag")
df2 <- df2 %>% select(-any_of(extra_drop))

names(df2) <- sub("_final$", "", names(df2))


# ---- fix pseudo-missing: "" -> NA for categorical columns ----
cat_cols_raw <- intersect(c("race_clean","marital_status","insurance","gender","first_careunit"), names(df2))

df2 <- df2 %>%
  mutate(across(all_of(cat_cols_raw), ~ {
    x <- trimws(as.character(.x))
    x[x == ""] <- NA_character_
    x
  }))

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


library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(scales)
library(ggridges)

# ── 主题设置（原版可运行）────────────────────────────────────────────────────
theme_los <- function() {
  theme_minimal(base_family = "sans") +
    theme(
      plot.title         = element_text(size = 13, face = "bold", color = "#1F3864", margin = margin(b = 6)),
      plot.subtitle      = element_text(size = 9,  color = "#595959", margin = margin(b = 8)),
      axis.title         = element_text(size = 9,  color = "#404040"),
      axis.text          = element_text(size = 8,  color = "#404040"),
      axis.text.x        = element_text(angle = 25, hjust = 1),
      strip.text         = element_text(size = 8,  face = "bold", color = "#1F3864"),
      panel.grid.major.y = element_line(color = "#E8E8E8", linewidth = 0.4),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      legend.position    = "none",
      plot.background    = element_rect(fill = "white", color = NA),
      panel.background   = element_rect(fill = "#FAFAFA", color = NA),
      plot.margin        = margin(12, 12, 8, 12)
    )
}

# 调色板（色盲友好）
pal_cat <- c("#2E75B6","#ED7D31","#70AD47","#FFC000","#9B59B6",
             "#E74C3C","#1ABC9C","#95A5A6","#F39C12","#2980B9",
             "#8E44AD","#16A085","#D35400","#2C3E50","#27AE60","#C0392B")
COL_1 <- "#2E75B6"
COL_0 <- "#ED7D31"

# ── 全量数据 ─────────────────────────────────────────────────────────────────
df_vis <- X_tree %>% mutate(y = y)
y_cap  <- quantile(df_vis$y, 0.99, na.rm = TRUE)
df_vis <- df_vis %>% mutate(y_plot = pmin(y, y_cap))

N_lab    <- scales::comma(nrow(df_vis))
cap_note <- paste0("n = ", N_lab, "  |  LOS capped at 99th pct (",
                   round(y_cap, 1), " days)")

# ════════════════════════════════════════════════════════════════════════════
# HELPER 1: violin + boxplot  (Fig1/2 — 原版不变)
# ════════════════════════════════════════════════════════════════════════════
make_violin <- function(data, var, title) {
  d <- data %>%
    filter(!is.na(.data[[var]])) %>%
    mutate(grp = as.character(.data[[var]])) %>%
    group_by(grp) %>%
    mutate(n     = n(),
           med   = median(y_plot),
           label = paste0(grp, "\n(n=", scales::comma(n), ")")) %>%
    ungroup() %>%
    mutate(label = reorder(label, med))
  
  cols <- pal_cat[seq_len(n_distinct(d$label))]
  
  ggplot(d, aes(x = label, y = y_plot, fill = label, color = label)) +
    geom_violin(alpha = 0.30, linewidth = 0.3, trim = TRUE) +
    geom_boxplot(width = 0.18, alpha = 0.85, linewidth = 0.45,
                 outlier.size = 0.5, outlier.alpha = 0.25, color = "grey35") +
    stat_summary(fun = median, geom = "point", shape = 18, size = 2.8, color = "#1F3864") +
    scale_fill_manual(values = cols) +
    scale_color_manual(values = cols) +
    scale_y_continuous(labels = label_number(accuracy = 0.1),
                       expand = expansion(mult = c(0.02, 0.05))) +
    labs(title = title, x = NULL, y = "ICU LOS (days)") +
    theme_los()
}

# ════════════════════════════════════════════════════════════════════════════
# HELPER 2: ridgeline for BINARY variables  (Fig3/4/5)
# ════════════════════════════════════════════════════════════════════════════
make_ridge_binary <- function(data, vars, title, label_map = NULL, min_n = 50) {
  rows <- lapply(vars, function(v) {
    if (!v %in% names(data)) return(NULL)
    d  <- data %>% filter(!is.na(.data[[v]]))
    n1 <- sum(d[[v]] == 1, na.rm = TRUE)
    n0 <- sum(d[[v]] == 0, na.rm = TRUE)
    if (n1 < min_n || n0 < min_n) return(NULL)
    nm <- if (!is.null(label_map) && v %in% names(label_map)) label_map[[v]] else v
    bind_rows(
      d %>% filter(.data[[v]] == 1) %>%
        transmute(y_plot, variable = nm, gtype = "1",
                  n1 = n1, n0 = n0),
      d %>% filter(.data[[v]] == 0) %>%
        transmute(y_plot, variable = nm, gtype = "0",
                  n1 = n1, n0 = n0)
    )
  }) %>% bind_rows()
  
  if (is.null(rows) || nrow(rows) == 0) return(NULL)
  
  # Sort by gap (largest at top)
  var_order <- rows %>%
    group_by(variable, gtype) %>%
    summarise(med = median(y_plot), .groups = "drop") %>%
    pivot_wider(names_from = gtype, values_from = med) %>%
    mutate(gap = abs(`1` - `0`)) %>%
    arrange(gap) %>%
    pull(variable)
  
  rows <- rows %>% mutate(variable = factor(variable, levels = var_order))
  
  # y-axis labels with n
  n_labels <- rows %>%
    group_by(variable, gtype) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(variable) %>%
    summarise(
      lab = paste0(first(variable),
                   "  Yes=", scales::comma(n[gtype=="1"]),
                   " / No=", scales::comma(n[gtype=="0"])),
      .groups = "drop"
    )
  lev_map <- setNames(n_labels$lab, n_labels$variable)
  rows <- rows %>% mutate(var_lab = lev_map[as.character(variable)],
                          var_lab = factor(var_lab, levels = lev_map[levels(variable)]))
  
  ggplot(rows, aes(x = y_plot, y = var_lab, fill = gtype, color = gtype)) +
    geom_density_ridges(alpha = 0.45, linewidth = 0.35,
                        quantile_lines = TRUE, quantiles = 0.5,
                        scale = 0.88, rel_min_height = 0.01) +
    scale_fill_manual(values  = c("1" = COL_1, "0" = COL_0)) +
    scale_color_manual(values = c("1" = COL_1, "0" = COL_0)) +
    scale_x_continuous(labels = label_number(accuracy = 0.1),
                       expand = expansion(mult = c(0.01, 0.05))) +
    labs(title    = title,
         subtitle = paste0(cap_note,
                           "  |  Blue = Yes (=1)   Orange = No (=0)   ",
                           "vertical line = median  |  sorted by LOS gap"),
         x = "ICU LOS (days)", y = NULL) +
    theme_los() +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
          axis.text.y = element_text(size = 7.5))
}

# ════════════════════════════════════════════════════════════════════════════
# HELPER 3: ridgeline for MISSINGNESS  (Fig6)
# ════════════════════════════════════════════════════════════════════════════
make_ridge_miss <- function(data, top_n = 20) {
  miss_vars <- grep("_miss$", names(data), value = TRUE)
  
  gaps <- lapply(miss_vars, function(v) {
    d      <- data %>% filter(!is.na(.data[[v]]))
    n_miss <- sum(d[[v]] == 1, na.rm = TRUE)
    n_obs  <- sum(d[[v]] == 0, na.rm = TRUE)
    if (n_miss < 30 || n_obs < 30) return(NULL)
    tibble(v = v, nm = sub("_miss$", "", v),
           gap  = abs(median(d$y_plot[d[[v]] == 1], na.rm = TRUE) -
                        median(d$y_plot[d[[v]] == 0], na.rm = TRUE)),
           n_miss = n_miss, n_obs = n_obs)
  }) %>% bind_rows() %>% arrange(desc(gap)) %>% slice_head(n = top_n)
  
  rows <- lapply(seq_len(nrow(gaps)), function(i) {
    v  <- gaps$v[i]; nm <- gaps$nm[i]
    n0 <- gaps$n_obs[i]; n1 <- gaps$n_miss[i]
    d  <- data %>% filter(!is.na(.data[[v]]))
    bind_rows(
      d %>% filter(.data[[v]] == 0) %>%
        transmute(y_plot, variable = nm, gtype = "obs",
                  lab = paste0(nm, "  obs=", scales::comma(n0),
                               " / miss=", scales::comma(n1))),
      d %>% filter(.data[[v]] == 1) %>%
        transmute(y_plot, variable = nm, gtype = "mis",
                  lab = paste0(nm, "  obs=", scales::comma(n0),
                               " / miss=", scales::comma(n1)))
    )
  }) %>% bind_rows()
  
  var_order <- gaps %>% arrange(gap) %>% pull(nm)
  lab_order <- lapply(var_order, function(nm) {
    rows %>% filter(variable == nm) %>% pull(lab) %>% first()
  }) %>% unlist()
  
  rows <- rows %>% mutate(lab = factor(lab, levels = lab_order))
  
  ggplot(rows, aes(x = y_plot, y = lab, fill = gtype, color = gtype)) +
    geom_density_ridges(alpha = 0.45, linewidth = 0.35,
                        quantile_lines = TRUE, quantiles = 0.5,
                        scale = 0.88, rel_min_height = 0.01) +
    scale_fill_manual(values  = c("obs" = COL_1, "mis" = COL_0)) +
    scale_color_manual(values = c("obs" = COL_1, "mis" = COL_0)) +
    scale_x_continuous(labels = label_number(accuracy = 0.1),
                       expand = expansion(mult = c(0.01, 0.05))) +
    labs(title    = paste0("ICU LOS Distribution by Missingness  (Top ", top_n, " by gap)"),
         subtitle = paste0(cap_note,
                           "  |  Blue = observed (=0)   Orange = missing (=1)   ",
                           "vertical line = median  |  sorted by LOS gap"),
         x = "ICU LOS (days)", y = NULL) +
    theme_los() +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
          axis.text.y = element_text(size = 7.5))
}

# ════════════════════════════════════════════════════════════════════════════
# BUILD FIGURES
# ════════════════════════════════════════════════════════════════════════════

# Fig1/2: 原版 violin（不变）
p_gender    <- make_violin(df_vis, "gender",        "Gender")
p_race      <- make_violin(df_vis, "race_clean",    "Race / Ethnicity")
p_insurance <- make_violin(df_vis, "insurance",     "Insurance Type")
p_marital   <- make_violin(df_vis, "marital_status","Marital Status")
p_careunit  <- make_violin(df_vis, "first_careunit","First Care Unit")

fig1 <- (p_gender | p_insurance) / (p_marital | p_race) +
  plot_annotation(
    title    = "ICU LOS by Categorical Variables",
    subtitle = cap_note,
    theme = theme(
      plot.title      = element_text(size=15, face="bold", color="#1F3864"),
      plot.subtitle   = element_text(size=9,  color="#595959"),
      plot.background = element_rect(fill="white", color=NA))
  )

fig2 <- p_careunit +
  labs(title = "ICU LOS by First Care Unit", subtitle = cap_note)

# Fig3: Comorbidities
fig3 <- make_ridge_binary(df_vis,
                          vars = c("hypertension","copd_comorb","sepsis","diabetes","ckd","pad",
                                   "obesity","smoking","alcohol_use",
                                   "primary_af","primary_ami","primary_hf_cm",
                                   "primary_renal_failure","primary_copd_bronchitis","primary_stroke_cerebro"),
                          title = "ICU LOS Distribution by Comorbidity / Primary Diagnosis",
                          label_map = c(
                            hypertension             = "Hypertension",
                            copd_comorb              = "COPD",
                            sepsis                   = "Sepsis",
                            diabetes                 = "Diabetes",
                            ckd                      = "CKD",
                            pad                      = "PAD",
                            obesity                  = "Obesity",
                            smoking                  = "Smoking",
                            alcohol_use              = "Alcohol Use",
                            primary_af               = "Primary Dx: AF",
                            primary_ami              = "Primary Dx: AMI",
                            primary_hf_cm            = "Primary Dx: HF/CM",
                            primary_renal_failure    = "Primary Dx: Renal Failure",
                            primary_copd_bronchitis  = "Primary Dx: COPD/Bronchitis",
                            primary_stroke_cerebro   = "Primary Dx: Stroke/Cerebro"
                          )
)

# Fig4: Medications
fig4 <- make_ridge_binary(df_vis,
                          vars = c("acei_arb","anticoagulants","antiplatelets",
                                   "beta_blockers","long_acting_bronchodilator","nephrotoxic","statins"),
                          title = "ICU LOS Distribution by Medication Use",
                          label_map = c(
                            acei_arb                   = "ACEi / ARB",
                            anticoagulants             = "Anticoagulants",
                            antiplatelets              = "Antiplatelets",
                            beta_blockers              = "Beta-Blockers",
                            long_acting_bronchodilator = "Long-Acting Bronchodilator",
                            nephrotoxic                = "Nephrotoxic Agents",
                            statins                    = "Statins"
                          )
)

# Fig5: Interventions
fig5 <- make_ridge_binary(df_vis,
                          vars = c("vent_any","vasopressors","norepinephrine",
                                   "epinephrine","dopamine","vasopressin","CCU_flag","CVICU_flag"),
                          title = "ICU LOS Distribution by Interventions & Unit Type",
                          label_map = c(
                            vent_any       = "Mechanical Ventilation",
                            vasopressors   = "Any Vasopressor",
                            norepinephrine = "Norepinephrine",
                            epinephrine    = "Epinephrine",
                            dopamine       = "Dopamine",
                            vasopressin    = "Vasopressin",
                            CCU_flag       = "CCU",
                            CVICU_flag     = "CVICU"
                          )
)

# Fig6: Missingness
fig6 <- make_ridge_miss(df_vis, top_n = 20)

# ════════════════════════════════════════════════════════════════════════════
# SAVE
# ════════════════════════════════════════════════════════════════════════════
ggsave("los_fig1_categorical.png",   fig1, width=14, height=11, dpi=180, bg="white")
ggsave("los_fig2_careunit.png",      fig2, width=12, height=7,  dpi=180, bg="white")
ggsave("los_fig3_comorbidities.png", fig3, width=12, height=10, dpi=180, bg="white")
ggsave("los_fig4_medications.png",   fig4, width=12, height=7,  dpi=180, bg="white")
ggsave("los_fig5_interventions.png", fig5, width=12, height=7,  dpi=180, bg="white")
ggsave("los_fig6_missingness.png",   fig6, width=12, height=10, dpi=180, bg="white")

cat("✓ los_fig1_categorical.png\n")
cat("✓ los_fig2_careunit.png\n")
cat("✓ los_fig3_comorbidities.png\n")
cat("✓ los_fig4_medications.png\n")
cat("✓ los_fig5_interventions.png\n")
cat("✓ los_fig6_missingness.png\n")




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

# --- optimized core function ---
wquant_matrix <- function(y_train, W_sparse, probs) {
  ord <- order(y_train)
  y_sorted <- y_train[ord]
  W_sorted <- W_sparse[, ord, drop = FALSE]
  n_test <- nrow(W_sorted)
  n_probs <- length(probs)
  res <- matrix(NA, nrow = n_test, ncol = n_probs)
  W_r <- as(W_sorted, "RsparseMatrix")
  for (i in 1:n_test) {
    row_start <- W_r@p[i] + 1L
    row_end   <- W_r@p[i + 1L]
    if (row_start > row_end) next
    nz_idx <- W_r@j[row_start:row_end] + 1L
    nz_w   <- W_r@x[row_start:row_end]
    nz_y   <- y_sorted[nz_idx]
    cw <- cumsum(nz_w) / sum(nz_w)
    for (p_idx in 1:n_probs) {
      idx <- which(cw >= probs[p_idx])[1]
      res[i, p_idx] <- nz_y[idx]
    }
  }
  return(res)
}

# --- Hyperparameter tuning ---
# HP tuning result: node=5 is best (WIS=0.8321), node increases monotonically worsen
# WIS range: 0.8321-0.8430 (small range = stable)
# Decision: use node=5 (best & consistent with no overfitting signal since DRF is non-parametric)
cat("\nDRF: Using fixed best HP from prior tuning (node=5, trees=500)\n")
num_trees_drf <- 500
min_node_size_drf <- 5

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

# --- compute WIS quantiles ---
drf_q_mat <- wquant_matrix(y_train, W, c(0.05, 0.5, 0.95))
drf_q <- as.data.frame(drf_q_mat)
colnames(drf_q) <- c("q05", "q50", "q95")

# --- compute full quantile grid ---
q_grid <- seq(0.05, 0.95, by = 0.05)
drf_qgrid_mat <- wquant_matrix(y_train, W, q_grid)
drf_qgrid <- as.data.frame(drf_qgrid_mat)
colnames(drf_qgrid) <- paste0("q", sprintf("%02d", round(100*q_grid)))

cat("\n✓ DRF complete.\n")


# 3-fit QRF

library(quantregForest)

# HP tuning result: ntree=500, nodesize=20 is best (WIS=0.7873)
# WIS gap between ntree=500 and ntree=1000 at nodesize=20: 0.7873 vs 0.7874 (negligible)
# Decision: use ntree=500 (same performance, faster) and nodesize=20
cat("\nQRF: Using fixed best HP from prior tuning (ntree=500, nodesize=20)\n")
ntree_qrf <- 500
nodesize_qrf <- 20

qs <- c(0.05, 0.5, 0.95)

# fit qrf model
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
cat("✓ QRF complete.\n")


# 4-fit ranger
library(ranger)

# HP tuning result: trees=500, node=10 is best (WIS=0.803), trees=1000 node=10 is 0.8006
# WIS diff trees=500 vs 1000 at node=10: 0.803 vs 0.8006 (small: 0.0024)
# Decision: use trees=1000, node=10 (marginal improvement, still reasonable runtime)
cat("\nRanger: Using fixed best HP from prior tuning (trees=1000, node=10)\n")
num_trees_ranger <- 1000
min_node_size_ranger <- 10

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

# ---- Ranger quantile grid predictions ----
rf_qgrid <- as.data.frame(
  predict(rf_q_fit, Xte_all, type = "quantiles", quantiles = q_grid)$predictions
)
colnames(rf_qgrid) <- paste0("q", sprintf("%02d", round(100*q_grid)))
cat("✓ Ranger RF complete.\n")


# 5-XGBoost + Conformal prediction

set.seed(2026)

stopifnot(exists("Xtr_all"), exists("Xte_all"), exists("y_tr"))
stopifnot(nrow(Xtr_all) == length(y_tr))

qs <- c(0.05, 0.5, 0.95)
alpha <- 0.10

# One-hot encoding for XGBoost
Xtr_all_mm_hp <- model.matrix(~ . - 1, data = Xtr_all)
Xte_all_mm    <- model.matrix(~ . - 1, data = Xte_all)

# Align columns
common_cols <- union(colnames(Xtr_all_mm_hp), colnames(Xte_all_mm))
Xtr_all_mm_hp <- Xtr_all_mm_hp[, common_cols, drop = FALSE]
Xte_all_mm    <- Xte_all_mm[,    common_cols, drop = FALSE]
Xtr_all_mm_hp[is.na(Xtr_all_mm_hp)] <- 0
Xte_all_mm[is.na(Xte_all_mm)]       <- 0

# HP tuning result: depth=6, eta=0.03, nrounds=400 is best (WIS=0.8901)
# Note: nrounds=400 + eta=0.03 is clearly best; higher eta with more rounds does NOT help
# Decision: use the best config directly (no further fine-tuning needed given stable landscape)
cat("\nXGBoost: Using fixed best HP from prior tuning (depth=6, eta=0.03, nrounds=400)\n")
max_depth_xgb <- 6
eta_xgb <- 0.03
nrounds_xgb <- 400

params <- list(
  objective = "reg:squarederror",
  max_depth = max_depth_xgb,
  eta = eta_xgb
)

# Build final DMatrices with proper train/cal/test split
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

# ---- XGB conformal quantile grid predictions ----
scale_tau2 <- (q_grid - 0.5) / 0.45
xgb_qgrid_mat <- sapply(scale_tau2, function(s) pred_test + s * q_hat)
xgb_qgrid <- as.data.frame(xgb_qgrid_mat)
colnames(xgb_qgrid) <- paste0("q", sprintf("%02d", round(100*q_grid)))
cat("✓ XGBoost complete.\n")


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

Xtr_ens_sc[Xtr_ens_sc > 10] <- 10
Xtr_ens_sc[Xtr_ens_sc < -10] <- -10
Xte_ens_sc[Xte_ens_sc > 10] <- 10
Xte_ens_sc[Xte_ens_sc < -10] <- -10

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
# SHARED: Pinball (quantile) loss — NON-PARAMETRIC
# All neural models now use pinball loss and output a quantile grid
# directly, making NO distributional assumptions (no Gamma).
# ============================================================
pinball_loss_grid <- function(pred, y_true, tau_t) {
  # pred: [batch x n_quantiles], y_true: [batch x 1], tau_t: [1 x n_quantiles]
  err  <- y_true - pred                        # [batch x n_quantiles]
  loss <- torch_where(err >= 0, tau_t * err, (tau_t - 1) * err)
  torch_mean(loss)
}

# ============================================================
# SHARED: Dataset / DataLoader builder
# ============================================================
make_dl <- function(X_sc, y_obs, batch_size = 512) {
  ds_def <- dataset(
    name       = "qr_ds",
    initialize = function(X, y) { self$X <- X; self$y <- y },
    .getitem   = function(i)    list(x = self$X[i, ], y = self$y[i]),
    .length    = function()     nrow(self$X)
  )
  X_t <- torch_tensor(X_sc,                    dtype = torch_float(), device = device)
  y_t <- torch_tensor(matrix(y_obs, ncol = 1), dtype = torch_float(), device = device)
  dataloader(ds_def(X_t, y_t), batch_size = batch_size, shuffle = TRUE)
}

# ============================================================
# SHARED: Quantile head — output n_quantiles directly
# ============================================================
tau_t <- torch_tensor(matrix(q_grid, nrow = 1), dtype = torch_float(), device = device)

# WIS helper for non-parametric net (pinball output)
qnet_wis <- function(net, X_sc, y_obs) {
  net$eval()
  Xv   <- torch_tensor(X_sc, dtype = torch_float(), device = device)
  with_no_grad({ pm <- as.matrix(net(Xv)$cpu()) })
  net$train()
  mean(scoringutils::wis(y_obs, pm, q_grid, na.rm = TRUE), na.rm = TRUE)
}

# Extract quantile grid from non-parametric net
get_qgrid <- function(net, X_sc) {
  net$eval()
  Xv <- torch_tensor(X_sc, dtype = torch_float(), device = device)
  with_no_grad({ pm <- as.matrix(net(Xv)$cpu()) })
  net$train()
  pm
}

# ============================================================
# MODEL 6: Deep Ensemble (Non-parametric, Pinball loss)
# M=5 independent DNNs, each trained with pinball loss on q_grid.
# Prediction = quantile averaging of M members (qEns scheme).
# ============================================================
cat("\n========================================\n")
cat("MODEL 6: Deep Ensemble (Non-parametric QR-DNN)\n")
cat("========================================\n")

make_qr_dnn <- function(n_in, h1, h2) {
  nn_sequential(
    nn_linear(n_in, h1), nn_relu(),
    nn_linear(h1,   h2), nn_relu(),
    nn_linear(h2,   n_quantiles)   # direct quantile outputs
  )
}

train_qr_dnn <- function(X_sc, y_obs, h1, h2, lr, epochs = 60, batch_size = 512) {
  net   <- make_qr_dnn(n_input, h1, h2)$to(device = device)
  optim <- optim_adam(net$parameters, lr = lr)
  dl    <- make_dl(X_sc, y_obs, batch_size)
  net$train()
  for (ep in seq_len(epochs)) {
    coro::loop(for (b in dl) {
      optim$zero_grad()
      pred <- net(b$x)
      loss <- pinball_loss_grid(pred, b$y, tau_t)
      loss$backward(); optim$step()
    })
  }
  net$eval(); net
}

# HP tuning result: h1=128, h2=32, lr=1e-4 is best (WIS=0.8055)
# WIS spread is narrow (0.8055-0.8238), no overfitting risk
# Decision: use best config directly
cat("ENS: Using fixed best HP from prior tuning (h1=128, h2=32, lr=1e-4)\n")
best_ens_hp <- list(h1 = 128, h2 = 32, lr = 1e-4)

# Train M members
M_ens         <- 5
ens_members   <- vector("list", M_ens)
cat(sprintf("Training %d ENS members (150 epochs each)...\n", M_ens))
for (m in seq_len(M_ens)) {
  set.seed(2026 + m)
  cat(sprintf("  Member %d/%d\n", m, M_ens))
  ens_members[[m]] <- train_qr_dnn(Xtr_ens_sc, y_tr,
                                   best_ens_hp$h1, best_ens_hp$h2, best_ens_hp$lr,
                                   epochs = 150)
}

# Predict: quantile averaging of M members (qEns)
cat("Computing ENS predictions (quantile averaging)...\n")
ens_qgrid_runs <- lapply(ens_members, function(net) get_qgrid(net, Xte_ens_sc))
ens_qgrid_mat  <- Reduce(`+`, ens_qgrid_runs) / M_ens

ens_q <- data.frame(q05 = ens_qgrid_mat[, q05_idx],
                    q50 = ens_qgrid_mat[, q50_idx],
                    q95 = ens_qgrid_mat[, q95_idx])
ens_qgrid <- as.data.frame(ens_qgrid_mat)
colnames(ens_qgrid) <- paste0("q", sprintf("%02d", round(100 * q_grid)))

# NLL: quantile-approx (non-parametric, uniform treatment with other NP models)
cat("ENS (Non-parametric) complete.\n\n")


# ============================================================
# MODEL 7: MC Dropout (Non-parametric, Pinball loss)
# Single DNN with dropout; at test time keep dropout ON and
# average quantile predictions over M forward passes.
# ============================================================
cat("========================================\n")
cat("MODEL 7: MC Dropout (Non-parametric QR-DNN)\n")
cat("========================================\n")

make_qr_dropout_dnn <- function(n_in, h1 = 128, h2 = 64, p_drop = 0.1) {
  nn_sequential(
    nn_linear(n_in, h1), nn_relu(), nn_dropout(p_drop),
    nn_linear(h1,   h2), nn_relu(), nn_dropout(p_drop),
    nn_linear(h2,   n_quantiles)
  )
}

train_qr_dropout_dnn <- function(X_sc, y_obs, p_drop, lr,
                                 h1 = 128, h2 = 64, epochs = 60, batch_size = 512) {
  net   <- make_qr_dropout_dnn(n_input, h1, h2, p_drop)$to(device = device)
  optim <- optim_adam(net$parameters, lr = lr)
  dl    <- make_dl(X_sc, y_obs, batch_size)
  net$train()
  for (ep in seq_len(epochs)) {
    coro::loop(for (b in dl) {
      optim$zero_grad()
      pred <- net(b$x)
      loss <- pinball_loss_grid(pred, b$y, tau_t)
      loss$backward(); optim$step()
    })
  }
  net  # leave in train() mode so dropout stays active at inference
}

mcd_avg_quantiles <- function(net, X_sc, M_pass = 50) {
  net$train()  # keep dropout ON
  X_t    <- torch_tensor(X_sc, dtype = torch_float(), device = device)
  acc_q  <- matrix(0, nrow(X_sc), n_quantiles)
  with_no_grad({
    for (p in seq_len(M_pass)) {
      acc_q <- acc_q + as.matrix(net(X_t)$cpu())
    }
  })
  acc_q / M_pass
}

# HP tuning result: dropout=0.20, lr=5e-4 is best (WIS=0.8263)
# Note: lr=1e-3 with low dropout causes instability (WIS=4.28 at config #4)
# Decision: use dropout=0.20, lr=5e-4 (stable and best performing)
cat("MCD: Using fixed best HP from prior tuning (dropout=0.20, lr=5e-4)\n")
best_mcd_hp <- list(p_drop = 0.20, lr = 5e-4)

set.seed(2026)
cat("Training final MCD model (150 epochs)...\n")
mc_dropout_net <- train_qr_dropout_dnn(Xtr_ens_sc, y_tr,
                                       best_mcd_hp$p_drop, best_mcd_hp$lr,
                                       epochs = 150)

M_dropout     <- 100
cat(sprintf("Generating %d MC passes on test set...\n", M_dropout))
mcd_qgrid_mat <- mcd_avg_quantiles(mc_dropout_net, Xte_ens_sc, M_pass = M_dropout)

mcd_q <- data.frame(q05 = mcd_qgrid_mat[, q05_idx],
                    q50 = mcd_qgrid_mat[, q50_idx],
                    q95 = mcd_qgrid_mat[, q95_idx])
mcd_qgrid <- as.data.frame(mcd_qgrid_mat)
colnames(mcd_qgrid) <- paste0("q", sprintf("%02d", round(100 * q_grid)))
cat("MCD (Non-parametric) complete.\n\n")


# ============================================================
# MODEL 8: DDNN — Distributional DNN (Non-parametric, Pinball loss)
# Following Marcjasz et al. (2023, Energy Economics):
#   - N_runs=4 independent HP optimisation trials (counteract local minima)
#   - Per-run: train on tr_cv, evaluate on va_cv, pick best HP
#   - Final: retrain each run on full train set with its own best HP
#   - Ensemble via HORIZONTAL (quantile) averaging of N_runs predictions
#     (qEns scheme: mean of same-quantile predictions, cf. Section 4.2.2)
#   - Separate L1 regularisation on hidden layers
#   - Activation: softplus (most frequently selected in paper)
#   - Early stopping on validation WIS (patience = 20 epochs)
#
# NOTE on Marcjasz et al. ensemble:
#   The paper does use ensemble (qEns = quantile averaging across independent
#   runs), as clearly stated in Section 4.2.2. Each run selects its own best
#   HP independently, then all runs' quantile predictions are averaged.
#   This is equivalent to what we implement here.
# ============================================================
cat("========================================\n")
cat("MODEL 8: DDNN (Non-parametric, Marcjasz et al. 2023)\n")
cat("========================================\n")

make_ddnn_net_np <- function(n_in, h1, h2, dropout_rate = 0.0) {
  nn_sequential(
    nn_linear(n_in, h1), nn_softplus(),
    if (dropout_rate > 0) nn_dropout(dropout_rate) else nn_identity(),
    nn_linear(h1,   h2), nn_softplus(),
    nn_linear(h2,   n_quantiles)   # direct quantile outputs
  )
}

train_ddnn_np <- function(X_tr, y_tr_in, X_va, y_va, h1, h2, lr,
                          dropout_rate = 0.0, l1_hidden = 0.0,
                          max_epochs = 200, patience = 20,
                          batch_size = 32, seed = 42) {
  set.seed(seed)
  torch_manual_seed(seed)
  net   <- make_ddnn_net_np(n_input, h1, h2, dropout_rate)$to(device = device)
  optim <- optim_adam(net$parameters, lr = lr)
  dl    <- make_dl(X_tr, y_tr_in, batch_size)
  
  Xva_t <- torch_tensor(X_va, dtype = torch_float(), device = device)
  yva_t <- torch_tensor(matrix(y_va, ncol = 1), dtype = torch_float(), device = device)
  
  best_val_wis <- Inf; best_state <- NULL; patience_cnt <- 0
  
  net$train()
  for (ep in seq_len(max_epochs)) {
    coro::loop(for (b in dl) {
      optim$zero_grad()
      pred <- net(b$x)
      loss_pb <- pinball_loss_grid(pred, b$y, tau_t)
      
      # L1 reg on hidden layer weights
      l1_pen <- torch_tensor(0.0, device = device)
      if (l1_hidden > 0) {
        for (nm in names(net$parameters)) {
          if (grepl("^[0-9]+\\.weight$", nm) && !grepl("^[56789]", nm)) {
            l1_pen <- l1_pen + l1_hidden * torch_sum(torch_abs(net$parameters[[nm]]))
          }
        }
      }
      loss <- loss_pb + l1_pen
      loss$backward(); optim$step()
    })
    
    # Early stopping on val WIS
    net$eval()
    with_no_grad({
      va_pred <- as.matrix(net(Xva_t)$cpu())
    })
    va_wis <- mean(scoringutils::wis(y_va, va_pred, q_grid, na.rm = TRUE), na.rm = TRUE)
    net$train()
    
    if (va_wis < best_val_wis - 1e-6) {
      best_val_wis  <- va_wis
      best_state    <- lapply(net$state_dict(), function(p) p$clone())
      patience_cnt  <- 0
    } else {
      patience_cnt <- patience_cnt + 1
      if (patience_cnt >= patience) {
        cat(sprintf("    Early stop at epoch %d (best val WIS=%.4f)\n", ep, best_val_wis))
        break
      }
    }
  }
  if (!is.null(best_state)) net$load_state_dict(best_state)
  net$eval()
  net
}

# HP tuning results:
# Run 1: h1=512, h2=256, lr=5e-4, l1=0      WIS=0.7949
# Run 2: h1=256, h2=128, lr=1e-3, l1=1e-4   WIS=0.7931
# Run 3: h1=512, h2=256, lr=5e-4, l1=0      WIS=0.7928 (best overall)
# Run 4: h1=512, h2=128, lr=1e-3, l1=1e-4   WIS=0.7947
# WIS range is very narrow (0.7928-0.7949), suggesting stable HP landscape.
# Decision: use each run's best HP from prior tuning (no additional re-tuning needed).
# Using NLL alongside WIS confirmed correct: both metrics agree on same best configs.

ddnn_hp_fixed <- list(
  list(h1=512, h2=256, lr=5e-4, dropout_rate=0.0, l1_hidden=0.0),   # Run 1 best
  list(h1=256, h2=128, lr=1e-3, dropout_rate=0.0, l1_hidden=1e-4),  # Run 2 best
  list(h1=512, h2=256, lr=5e-4, dropout_rate=0.0, l1_hidden=0.0),   # Run 3 best (overall best)
  list(h1=512, h2=128, lr=1e-3, dropout_rate=0.0, l1_hidden=1e-4)   # Run 4 best
)

N_runs_ddnn <- 4
ddnn_runs   <- vector("list", N_runs_ddnn)
cat(sprintf("Training %d DDNN runs with fixed best HPs (from prior tuning)...\n", N_runs_ddnn))

for (run in seq_len(N_runs_ddnn)) {
  hp <- ddnn_hp_fixed[[run]]
  cat(sprintf("\n--- DDNN Run %d/%d: h1=%d h2=%d lr=%.4g l1=%.4g ---\n",
              run, N_runs_ddnn, hp$h1, hp$h2, hp$lr, hp$l1_hidden))
  set.seed(2026 + run * 31)
  ddnn_runs[[run]] <- list(
    net = train_ddnn_np(Xtr_ens_sc, y_tr,
                        X_va_cv, y_va_cv,
                        h1 = hp$h1, h2 = hp$h2, lr = hp$lr,
                        dropout_rate = hp$dropout_rate,
                        l1_hidden    = hp$l1_hidden,
                        max_epochs = 300, patience = 30,
                        batch_size = 32, seed = 2026 + run * 31),
    best_hp = hp
  )
}

# HORIZONTAL (quantile) averaging — qEns scheme (Marcjasz et al. Section 4.2.2)
cat("\nComputing DDNN qEns (horizontal quantile averaging across 4 runs)...\n")
ddnn_qgrid_runs <- lapply(ddnn_runs, function(r) get_qgrid(r$net, Xte_ens_sc))
ddnn_qgrid_mat  <- Reduce(`+`, ddnn_qgrid_runs) / N_runs_ddnn

ddnn_q <- data.frame(
  q05 = ddnn_qgrid_mat[, q05_idx],
  q50 = ddnn_qgrid_mat[, q50_idx],
  q95 = ddnn_qgrid_mat[, q95_idx]
)
ddnn_qgrid <- as.data.frame(ddnn_qgrid_mat)
colnames(ddnn_qgrid) <- paste0("q", sprintf("%02d", round(100 * q_grid)))

best_ddnn_hp <- lapply(ddnn_runs, `[[`, "best_hp")
cat("DDNN (Non-parametric, qEns) complete.\n\n")


# ============================================================
# MODEL 9: Ensemble DRF — Stacked Generalisation
# (unchanged from original)
# ============================================================
cat("========================================\n")
cat("MODEL 9: Ensemble DRF (Stacking, Papacharalampous et al. 2025)\n")
cat("========================================\n")

library(quantreg)

set.seed(2026 + 900)
n_tr     <- nrow(Xtr_tree)
idx_set1 <- sample(n_tr, floor(n_tr / 2), replace = FALSE)
idx_set2 <- setdiff(seq_len(n_tr), idx_set1)
cat(sprintf("Stacking split: Set1=%d, Set2=%d\n", length(idx_set1), length(idx_set2)))

X_set1   <- Xtr_tree[idx_set1, , drop = FALSE]
y_set1   <- y_tr[idx_set1]
X_set2   <- Xtr_tree[idx_set2, , drop = FALSE]
y_set2   <- y_tr[idx_set2]

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

cat("Predicting quantiles for Set2 (combiner training data)...\n")
set2_qpreds <- lapply(drf_base_s1, function(fit) {
  pr <- predict(fit, X_set2)
  wquant_matrix(as.numeric(pr$y), pr$weights, q_grid)
})

cat("Fitting QR combiners for all quantile levels (Set2)...\n")
qr_combiners <- vector("list", length(q_grid))

for (qi in seq_along(q_grid)) {
  tau_i <- q_grid[qi]
  X_comb <- do.call(cbind, lapply(set2_qpreds, function(qm) qm[, qi]))
  df_comb <- as.data.frame(X_comb)
  colnames(df_comb) <- paste0("bl", seq_len(K_drf))
  df_comb$y <- y_set2
  fmla <- as.formula(paste("y ~", paste(paste0("bl", seq_len(K_drf)), collapse = " + ")))
  qr_combiners[[qi]] <- tryCatch(
    rq(fmla, tau = tau_i, data = df_comb, method = "br"),
    error = function(e) NULL
  )
}
cat(sprintf("  Fitted QR combiners for %d quantile levels.\n", sum(!sapply(qr_combiners, is.null))))

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

cat("Predicting test quantiles from updated base learners...\n")
te_qpreds <- lapply(drf_base_full, function(fit) {
  pr <- predict(fit, Xte_tree)
  wquant_matrix(as.numeric(pr$y), pr$weights, q_grid)
})

cat("Applying stacking combiner to test predictions...\n")
edrf_qgrid_mat <- matrix(NA_real_, nrow(Xte_tree), length(q_grid))

for (qi in seq_along(q_grid)) {
  X_te_comb <- do.call(cbind, lapply(te_qpreds, function(qm) qm[, qi]))
  df_te_comb <- as.data.frame(X_te_comb)
  colnames(df_te_comb) <- paste0("bl", seq_len(K_drf))
  
  if (!is.null(qr_combiners[[qi]])) {
    preds <- predict(qr_combiners[[qi]], newdata = df_te_comb)
    edrf_qgrid_mat[, qi] <- pmax(preds, 0)
  } else {
    edrf_qgrid_mat[, qi] <- rowMeans(X_te_comb)
  }
}

for (i in seq_len(nrow(edrf_qgrid_mat))) {
  edrf_qgrid_mat[i, ] <- sort(edrf_qgrid_mat[i, ])
}

cat("Computing mean combiner benchmark...\n")
edrf_meancomb_mat <- Reduce(`+`, te_qpreds) / K_drf
for (i in seq_len(nrow(edrf_meancomb_mat))) {
  edrf_meancomb_mat[i, ] <- sort(edrf_meancomb_mat[i, ])
}

edrf_q <- data.frame(
  q05 = edrf_qgrid_mat[, q05_idx],
  q50 = edrf_qgrid_mat[, q50_idx],
  q95 = edrf_qgrid_mat[, q95_idx]
)
edrf_qgrid <- as.data.frame(edrf_qgrid_mat)
colnames(edrf_qgrid) <- paste0("q", sprintf("%02d", round(100 * q_grid)))

edrf_meancomb_q <- data.frame(
  q05 = edrf_meancomb_mat[, q05_idx],
  q50 = edrf_meancomb_mat[, q50_idx],
  q95 = edrf_meancomb_mat[, q95_idx]
)

cat("Ensemble DRF (stacking) complete.\n\n")


# ============================================================
# MODEL 10: BNN — Bayes by Backprop (Non-parametric, Pinball loss)
# Architecture: same BBB structure (Blundell et al. 2015),
# but output layer produces n_quantiles instead of Gamma params.
# ============================================================
cat("========================================\n")
cat("MODEL 10: BNN (Bayes by Backprop, Non-parametric)\n")
cat("========================================\n")

# ---- Scale mixture prior — unchanged from original ----
torch_log_scale_mixture_prior <- function(w, pi_mix, log_sigma1, log_sigma2) {
  half_log2pi <- 0.5 * log(2 * pi)
  log_p1 <- log(pi_mix)       - half_log2pi - log_sigma1 -
    0.5 * (w / exp(log_sigma1))^2
  log_p2 <- log(1 - pi_mix)   - half_log2pi - log_sigma2 -
    0.5 * (w / exp(log_sigma2))^2
  m      <- torch_maximum(log_p1, log_p2)
  torch_sum(m + torch_log(torch_exp(log_p1 - m) + torch_exp(log_p2 - m)))
}

# ---- Bayesian linear layer — unchanged ----
bnn_linear <- nn_module(
  classname  = "bnn_linear_bbb",
  initialize = function(in_f, out_f, pi_mix, log_sigma1, log_sigma2) {
    self$in_f      <- in_f
    self$out_f     <- out_f
    self$pi_mix    <- pi_mix
    self$log_sig1  <- log_sigma1
    self$log_sig2  <- log_sigma2
    self$w_mu   <- nn_parameter(torch_randn(out_f, in_f)  * 0.01)
    self$w_rho  <- nn_parameter(torch_full(c(out_f, in_f), -3.0))
    self$b_mu   <- nn_parameter(torch_zeros(out_f))
    self$b_rho  <- nn_parameter(torch_full(c(out_f), -3.0))
  },
  forward = function(x) {
    w_sigma        <- torch_log1p(torch_exp(self$w_rho))
    b_sigma        <- torch_log1p(torch_exp(self$b_rho))
    self$w_sample  <- self$w_mu + w_sigma * torch_randn_like(w_sigma)
    self$b_sample  <- self$b_mu + b_sigma * torch_randn_like(b_sigma)
    nnf_linear(x, self$w_sample, self$b_sample)
  },
  kl_term = function() {
    w_sigma <- torch_log1p(torch_exp(self$w_rho))
    b_sigma <- torch_log1p(torch_exp(self$b_rho))
    log_qw <- torch_sum(
      -0.5 * log(2 * pi) - torch_log(w_sigma) -
        0.5 * ((self$w_sample - self$w_mu) / w_sigma)^2
    )
    log_qb <- torch_sum(
      -0.5 * log(2 * pi) - torch_log(b_sigma) -
        0.5 * ((self$b_sample - self$b_mu) / b_sigma)^2
    )
    log_q  <- log_qw + log_qb
    log_pw <- torch_log_scale_mixture_prior(
      self$w_sample$detach(), self$pi_mix, self$log_sig1, self$log_sig2)
    log_pb <- torch_log_scale_mixture_prior(
      self$b_sample$detach(), self$pi_mix, self$log_sig1, self$log_sig2)
    log_q - (log_pw + log_pb)
  }
)

# ---- BNN net: now outputs n_quantiles (non-parametric) ----
bnn_qr_net <- nn_module(
  classname  = "bnn_qr_bbb",
  initialize = function(n_in, h1, h2, pi_mix, log_sigma1, log_sigma2) {
    self$l1 <- bnn_linear(n_in, h1, pi_mix, log_sigma1, log_sigma2)
    self$l2 <- bnn_linear(h1,   h2, pi_mix, log_sigma1, log_sigma2)
    self$l3 <- bnn_linear(h2,   n_quantiles, pi_mix, log_sigma1, log_sigma2)
  },
  forward = function(x) {
    x <- nnf_relu(self$l1(x))
    x <- nnf_relu(self$l2(x))
    self$l3(x)
  },
  kl = function() { self$l1$kl_term() + self$l2$kl_term() + self$l3$kl_term() }
)

# ---- Training with KL re-weighting (Eq. 9) + pinball loss ----
train_bnn_bbb_np <- function(X_sc, y_obs, h1, h2, lr,
                             pi_mix, log_sigma1, log_sigma2,
                             epochs = 100, batch_size = 128, seed = 42) {
  set.seed(seed); torch_manual_seed(seed)
  N   <- nrow(X_sc)
  net <- bnn_qr_net(n_input, h1, h2, pi_mix, log_sigma1, log_sigma2)$to(device = device)
  opt <- optim_adam(net$parameters, lr = lr)
  dl  <- make_dl(X_sc, y_obs, batch_size)
  
  n_batches <- ceiling(N / batch_size)
  kl_weights <- sapply(seq_len(n_batches), function(i) {
    (2^(n_batches - i)) / (2^n_batches - 1)
  })
  
  net$train()
  for (ep in seq_len(epochs)) {
    batch_cnt <- 0L
    coro::loop(for (b in dl) {
      batch_cnt  <- batch_cnt + 1L
      pi_i       <- kl_weights[min(batch_cnt, n_batches)]
      opt$zero_grad()
      pred       <- net(b$x)
      pb_loss    <- pinball_loss_grid(pred, b$y, tau_t)
      kl_contrib <- net$kl()
      loss <- pi_i * kl_contrib + pb_loss
      loss$backward(); opt$step()
    })
    if (ep %% 25 == 0)
      cat(sprintf("    Epoch %d/%d\n", ep, epochs))
  }
  net$eval()
  net
}

# Thompson sampling for BNN inference (quantile predictions)
bnn_avg_quantiles <- function(net, X_sc, M_pass = 100) {
  net$train()  # keep stochasticity ON
  X_t    <- torch_tensor(X_sc, dtype = torch_float(), device = device)
  acc_q  <- matrix(0, nrow(X_sc), n_quantiles)
  for (p_i in seq_len(M_pass)) {
    with_no_grad({
      pm    <- as.matrix(net(X_t)$cpu())
    })
    acc_q <- acc_q + pm
  }
  acc_q / M_pass
}

# HP tuning result analysis:
# Best: h1=128, h2=32, lr=5e-4, pi=0.75, -log(s1)=0, -log(s2)=6  (WIS=0.8024)
# BNN WIS landscape is relatively flat (0.8024-0.8509), indicating architecture
# matters less than prior specification. Large WIS spread is mostly from unstable
# configs (lr=1e-3 with low dropout). Conservative lr=5e-4 is clearly safer.
# Decision: Use top-3 configs by WIS and reduce grid to 12 configs (from 64).
# We fix the stable combinations: pi=0.75, -log(s1)=0, lr=5e-4 as anchors.

cat("Tuning BNN hyperparameters (reduced grid: 12 configs)...\n")
# Reduced grid based on HP tuning insights:
# - pi=0.75 consistently better than 0.5 (more weight on narrow prior component)
# - lr=5e-4 more stable than 1e-3 for BNN
# - -log(s2)=6 vs 7: comparable, include both
# - Architecture: h1=128 marginally better; include both sizes for robustness
bnn_hp_grid <- expand.grid(
  h1          = c(64, 128),
  h2          = c(32, 64),
  lr          = c(5e-4),             # fixed to stable lr only
  pi_mix      = c(0.75),             # fixed to best performing pi
  neg_log_s1  = c(0),               # fixed to best -log(s1)
  neg_log_s2  = c(6, 7),            # keep both
  stringsAsFactors = FALSE
)
# 2 x 2 x 1 x 1 x 1 x 2 = 8 configs (down from 64)

bnn_cv_wis <- sapply(seq_len(nrow(bnn_hp_grid)), function(i) {
  hp <- bnn_hp_grid[i, ]
  cat(sprintf("  [%d/%d] h1=%d h2=%d lr=%.4g pi=%.2f sig1=e^%.0f sig2=e^%.0f ...",
              i, nrow(bnn_hp_grid),
              hp$h1, hp$h2, hp$lr, hp$pi_mix, -hp$neg_log_s1, -hp$neg_log_s2))
  tryCatch({
    net <- train_bnn_bbb_np(
      X_tr_cv, y_tr_cv,
      h1 = hp$h1, h2 = hp$h2, lr = hp$lr,
      pi_mix      = hp$pi_mix,
      log_sigma1  = -hp$neg_log_s1,
      log_sigma2  = -hp$neg_log_s2,
      epochs      = 50, batch_size = 128, seed = 2026 + i
    )
    pm  <- bnn_avg_quantiles(net, X_va_cv, M_pass = 20)
    w   <- mean(scoringutils::wis(y_va_cv, pm, q_grid, na.rm = TRUE), na.rm = TRUE)
    cat(sprintf(" WIS=%.4f\n", w)); w
  }, error = function(e) { cat(sprintf(" ERROR: %s\n", conditionMessage(e))); Inf })
})

best_bnn_i  <- which.min(bnn_cv_wis)
best_bnn_hp <- as.list(bnn_hp_grid[best_bnn_i, ])
cat(sprintf("\n✓ Best BNN: h1=%d h2=%d lr=%.4g pi=%.2f -log(s1)=%g -log(s2)=%g (WIS=%.4f)\n",
            best_bnn_hp$h1, best_bnn_hp$h2, best_bnn_hp$lr,
            best_bnn_hp$pi_mix, best_bnn_hp$neg_log_s1, best_bnn_hp$neg_log_s2,
            bnn_cv_wis[best_bnn_i]))

# ---- Train final BNN ----
set.seed(2026 + 77)
cat("Training final BNN (150 epochs, full train set)...\n")
bnn_net <- train_bnn_bbb_np(
  Xtr_ens_sc, y_tr,
  h1          = best_bnn_hp$h1,
  h2          = best_bnn_hp$h2,
  lr          = best_bnn_hp$lr,
  pi_mix      = best_bnn_hp$pi_mix,
  log_sigma1  = -best_bnn_hp$neg_log_s1,
  log_sigma2  = -best_bnn_hp$neg_log_s2,
  epochs      = 150, batch_size = 128, seed = 2026 + 77
)

cat("BNN inference (M=100 stochastic forward passes)...\n")
bnn_qgrid_mat  <- bnn_avg_quantiles(bnn_net, Xte_ens_sc, M_pass = 100)
bnn_q          <- data.frame(q05 = bnn_qgrid_mat[, q05_idx],
                             q50 = bnn_qgrid_mat[, q50_idx],
                             q95 = bnn_qgrid_mat[, q95_idx])
bnn_qgrid      <- as.data.frame(bnn_qgrid_mat)
colnames(bnn_qgrid) <- paste0("q", sprintf("%02d", round(100 * q_grid)))
cat("BNN (Non-parametric) complete.\n\n")


# ============================================================
# MODEL 11: Hybrid NN + DRF (unchanged)
# ============================================================
cat("========================================\n")
cat("MODEL 11: Hybrid NN + DRF\n")
cat("========================================\n")

cat("Stage 1: Training neural feature extractor (QRNN, 150 epochs)...\n")
set.seed(2026)

y_mean_ens  <- mean(y_tr);  y_sd_ens <- sd(y_tr)
y_tr_sc     <- (y_tr - y_mean_ens) / y_sd_ens

pinball_loss <- function(pred, y_true, tau) {
  err  <- y_true - pred
  loss <- torch_where(err >= 0, tau * err, (tau - 1) * err)
  torch_mean(loss)
}

nn_feat_extractor <- nn_sequential(
  nn_linear(n_input, 128), nn_relu(),
  nn_linear(128, 32),      nn_relu()
)
nn_out_head <- nn_linear(32, n_quantiles)

nn_feat_extractor$to(device = device)
nn_out_head$to(device = device)

opt_hyb <- optim_adam(c(nn_feat_extractor$parameters, nn_out_head$parameters), lr = 5e-5)

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

cat("Stage 2: Extracting 32-dim neural features...\n")
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
  ENS_NP       = best_ens_hp,
  MCD_NP       = best_mcd_hp,
  DDNN_NP      = best_ddnn_hp,
  Ensemble_DRF = list(K = K_drf, num.trees = num_trees_drf,
                      min.node.size = min_node_size_drf, combiner = "linear_QR"),
  BNN_NP       = best_bnn_hp
)


# ============================================================
# UNIFIED EVALUATION (all 10 models)
# ============================================================
cat("\n========================================\n")
cat("UNIFIED EVALUATION — ALL MODELS\n")
cat("========================================\n\n")

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
  ENS_NP       = ens_q,
  MCD_NP       = mcd_q,
  DDNN_NP      = ddnn_q,
  Ensemble_DRF = edrf_q,
  BNN_NP       = bnn_q,
  Hybrid_NN_DRF = hybrid_q
)
all_qgrid <- list(
  DRF          = drf_qgrid,
  QRF          = qrf_qgrid,
  RF           = rf_qgrid,
  XGB          = xgb_qgrid,
  ENS_NP       = ens_qgrid,
  MCD_NP       = mcd_qgrid,
  DDNN_NP      = ddnn_qgrid,
  Ensemble_DRF = edrf_qgrid,
  BNN_NP       = bnn_qgrid,
  Hybrid_NN_DRF = hybrid_qgrid
)
model_names <- names(all_q)

# Coverage & width
cov_width <- do.call(rbind, lapply(model_names, function(m) {
  r <- eval_interval(all_q[[m]], y_te)
  data.frame(Model = m, Coverage = r$coverage, Width = r$width)
}))

cat("Coverage & Width:\n")
cov_width[, c("Coverage", "Width")] <- round(cov_width[, c("Coverage", "Width")], 4)
print(cov_width)

# WIS
wis_vec <- sapply(model_names, function(m) wis_score(all_q[[m]], y_te))
cat("\nWIS (lower = better):\n"); print(round(wis_vec, 4))

# CRPS
crps_vec <- sapply(model_names, function(m)
  crps_from_quantiles(y_te, as.matrix(all_qgrid[[m]]), q_grid))
cat("\nCRPS (lower = better):\n"); print(round(crps_vec, 4))

# NLL: quantile-approx for all non-parametric models (unified treatment)
nll_vec <- sapply(model_names, function(m) {
  if (m == "XGB") return(NA_real_)
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
eval_df <- do.call(data.frame, lapply(names(all_q), function(m) {
  mn <- tolower(gsub("[^a-zA-Z0-9]","_",m))
  q  <- all_q[[m]]
  setNames(q, paste0(mn, c("_q05","_q50","_q95")))
}))
eval_df$y <- y_te

eval_df$los_group  <- cut(y_te, c(-Inf,3,7,Inf), labels=c("Short(≤3d)","Medium(3-7d)","Long(>7d)"))

# ---- WIS helper for subgroup ----
wis_one <- function(y, q05, q50, q95) {
  pred <- cbind(q05, q50, q95)
  mean(scoringutils::wis(y, pred, c(0.05,0.5,0.95), na.rm=TRUE), na.rm=TRUE)
}

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


# ============================================================
# HETEROGENEITY ANALYSIS
#
# Subgroup selection is directly driven by the EDA figures (Fig1–Fig6).
# Only groups showing VISIBLE distributional differences in LOS are included.
# Groups with near-identical distributions are excluded to avoid noise.
#
# SELECTION RATIONALE (based on visual inspection of EDA figures):
#
# INCLUDED:
#   1. Vasopressin use (Fig5, top): LARGEST LOS gap of all binary vars.
#      Vasopressin=Yes has a strongly right-shifted, flatter distribution.
#      Ideal test case — DRF's distribution-free approach should model
#      this heavy right tail more faithfully than parametric competitors.
#
#   2. Any Vasopressor / Norepinephrine (Fig5): Clear distributional shift.
#      Yes group peaks earlier but has heavier tail (bimodal-like shape).
#      Clinically: vasopressor use marks haemodynamic instability.
#
#   3. Mechanical Ventilation (Fig5): Yes group notably more spread out,
#      though gap smaller than vasopressor. Large balanced subgroups
#      (16,800 vs 21,381) — statistically reliable comparison.
#
#   4. Sepsis (Fig3, top row): Largest LOS gap in comorbidity figure.
#      Sepsis=Yes distribution is flatter, more right-skewed.
#      DRF hypothesis: better captures the multi-modal/heavy-tail shape.
#
#   5. Primary Dx: Stroke/Cerebro (Fig3, 2nd row): Yes group clearly
#      left-shifted (shorter LOS) with sharper peak vs broader No group.
#      Distinct distributional SHAPE (not just shift) — ideal for DRF.
#
#   6. ICU Care Unit — 3-group (Fig2): CVICU has a concentrated short-LOS
#      distribution; MICU/TSICU have heavy right tails; Neuro Intermediate/
#      Surgery-Vascular have much higher medians. Three meaningful groups.
#
#   7. LOS severity (Short ≤3d / Medium 3–7d / Long >7d): Tests model
#      calibration across the full outcome range. DRF should excel at
#      Long stays (heavy tail) relative to parametric models.
#
# MISSING VARIABLE SUBGROUP — PER-VARIABLE APPROACH (not overall rate):
#   Rationale for per-variable over overall missing rate:
#   (a) Fig6 visualises per-variable missingness — subgroup analysis should
#       match the EDA framing for interpretive consistency.
#   (b) Overall missing rate conflates clinically distinct patterns
#       (e.g., missing vitals ≠ missing lipid panel).
#   (c) Informative missingness is variable-specific: each variable's
#       missingness carries a different clinical signal.
#
#   Selected variables from Fig6 (balanced groups + clear LOS gap):
#     - temp: obs=9,375 / miss=28,806. Missing temp suggests patient
#       was not in ICU monitoring mode; missing group has shorter LOS.
#       Large gap, very large n in both groups.
#     - icu_sbp_last24h: obs=10,229 / miss=27,952. Blood pressure missing
#       means no arterial line/telemetry — lower acuity. Visible gap in Fig6.
#     - Albumin: obs=20,939 / miss=17,242. Near-balanced groups. Missing
#       Albumin associated with shorter LOS (routine labs not ordered).
#
# EXCLUDED (with reasons):
#   - Gender, Marital Status, Race/Ethnicity, Insurance (Fig1):
#     All groups show near-identical violin/box shapes. No meaningful gap.
#   - Hypertension, Diabetes, Smoking (Fig3): Heavy overlap, median gap ~0.
#   - Nephrotoxic Agents (Fig4): Yes group slightly flatter but overlap large.
#   - Medication variables (Fig4): All showing mostly overlapping distributions.
#   - icu_hr_last24h missingness (Fig6): miss=67 — too few for stable estimate.
#   - GCS variables (Fig6): miss=107–120 — too few.
# ============================================================

cat("\n========================================\n")
cat("HETEROGENEITY ANALYSIS\n")
cat("========================================\n\n")

# ---- Subgroup 1: Vasopressin use (largest LOS gap in Fig5) ----
if ("vasopressin" %in% names(Xte_tree)) {
  eval_df$vasopressin_group <- factor(
    Xte_tree$vasopressin, 0:1, c("No Vasopressin", "Vasopressin")
  )
  out_vasopressin_wis <- subgroup_wis_table(eval_df, "vasopressin_group")
  cat("\n=== WIS by Vasopressin Subgroup ===\n"); print(out_vasopressin_wis)
} else {
  out_vasopressin_wis <- NULL
}

# ---- Subgroup 2: Any Vasopressor use (Fig5) ----
eval_df$vaso_group <- factor(Xte_tree$vasopressors, 0:1, c("No Vasopressors", "Vasopressors"))
out_vaso_wis <- subgroup_wis_table(eval_df, "vaso_group")
cat("\n=== WIS by Any Vasopressor Subgroup ===\n"); print(out_vaso_wis)

# ---- Subgroup 3: Mechanical Ventilation (Fig5) ----
eval_df$vent_group <- factor(Xte_tree$vent_any, 0:1, c("No Ventilation", "Ventilated"))
out_vent_wis <- subgroup_wis_table(eval_df, "vent_group")
cat("\n=== WIS by Mechanical Ventilation Subgroup ===\n"); print(out_vent_wis)

# ---- Subgroup 4: Sepsis (Fig3, largest comorbidity gap) ----
if ("sepsis" %in% names(Xte_tree)) {
  eval_df$sepsis_group <- factor(Xte_tree$sepsis, 0:1, c("No Sepsis", "Sepsis"))
  out_sepsis_wis <- subgroup_wis_table(eval_df, "sepsis_group")
  cat("\n=== WIS by Sepsis Subgroup ===\n"); print(out_sepsis_wis)
} else {
  out_sepsis_wis <- NULL
  cat("\n(sepsis not found in test features)\n")
}

# ---- Subgroup 5: Primary Dx Stroke/Cerebrovascular (Fig3, 2nd row) ----
if ("primary_stroke_cerebro" %in% names(Xte_tree)) {
  eval_df$stroke_group <- factor(
    Xte_tree$primary_stroke_cerebro, 0:1, c("No Stroke/Cerebro", "Stroke/Cerebro")
  )
  out_stroke_wis <- subgroup_wis_table(eval_df, "stroke_group")
  cat("\n=== WIS by Stroke/Cerebrovascular Subgroup ===\n"); print(out_stroke_wis)
} else {
  out_stroke_wis <- NULL
  cat("\n(primary_stroke_cerebro not found in test features)\n")
}

# ---- Subgroup 6: ICU Care Unit — 3 groups (Fig2) ----
# CVICU: concentrated short LOS (n=9,752)
# MICU/TSICU: heavy right tails (n=6,399 + 4,265)
# Neuro/Surgical Intermediate: noticeably higher medians
if ("first_careunit" %in% names(Xte_tree)) {
  unit_vals <- trimws(as.character(Xte_tree$first_careunit))
  eval_df$unit_group3 <- dplyr::case_when(
    unit_vals %in% c("Cardiac Vascular Intensive Care Unit (CVICU)", "CVICU") ~
      "CVICU (cardiac, short LOS)",
    unit_vals %in% c("Medical Intensive Care Unit (MICU)", "MICU",
                     "Trauma SICU (TSICU)", "TSICU",
                     "Medical/Surgical Intensive Care Unit (MICU/SICU)", "MICU/SICU") ~
      "Medical/Trauma ICU (heavy tail)",
    !is.na(unit_vals) ~ "Other ICU (surgical/neuro/step-down)",
    TRUE ~ NA_character_
  )
  eval_df$unit_group3 <- factor(eval_df$unit_group3, levels = c(
    "CVICU (cardiac, short LOS)",
    "Medical/Trauma ICU (heavy tail)",
    "Other ICU (surgical/neuro/step-down)"
  ))
  out_unit_wis <- subgroup_wis_table(
    eval_df %>% filter(!is.na(unit_group3)), "unit_group3"
  )
  cat("\n=== WIS by ICU Care Unit (3-group) ===\n"); print(out_unit_wis)
} else {
  out_unit_wis <- NULL
  cat("\n(first_careunit not found in test features)\n")
}

# ---- Subgroup 7: LOS severity (Short ≤3d / Medium 3–7d / Long >7d) ----
out_los_wis <- subgroup_wis_table(eval_df, "los_group")
cat("\n=== WIS by LOS Severity Subgroup ===\n"); print(out_los_wis)

# ---- Subgroup 8: Per-variable missingness — 3 selected variables from Fig6 ----
# Variables selected: temp, icu_sbp_last24h, Albumin
# Criteria: (1) balanced group sizes (>1,000 in both groups),
#           (2) visible LOS gap in Fig6, (3) clear clinical interpretation.

cat("\n=== WIS by Per-Variable Missingness (Fig6-informed) ===\n")

# Helper: check variable name variants (with/without _final suffix)
get_miss_col <- function(base_name, feature_df) {
  candidates <- c(paste0(base_name, "_miss"),
                  paste0(base_name, "_final_miss"),
                  paste0(base_name, "_miss_final"))
  found <- intersect(candidates, names(feature_df))
  if (length(found) > 0) found[1] else NULL
}

miss_selected <- list(
  list(base = "temp",          label = "Body Temp",
       interpretation = "Missing = not on ICU monitoring; shorter LOS"),
  list(base = "icu_sbp_last24h", label = "SBP last 24h",
       interpretation = "Missing = no arterial line; lower acuity patients"),
  list(base = "Albumin",       label = "Albumin",
       interpretation = "Missing = routine labs not ordered; shorter LOS")
)

out_miss_var_list <- list()
for (ms in miss_selected) {
  miss_col <- get_miss_col(ms$base, Xte_tree)
  if (is.null(miss_col)) {
    cat(sprintf("\n  (%s missingness column not found — skipping)\n", ms$label))
    next
  }
  vals   <- Xte_tree[[miss_col]]
  n_miss <- sum(vals == 1, na.rm = TRUE)
  n_obs  <- sum(vals == 0, na.rm = TRUE)
  if (n_miss < 30 || n_obs < 30) {
    cat(sprintf("\n  (%s: n_miss=%d or n_obs=%d too small — skipping)\n",
                ms$label, n_miss, n_obs))
    next
  }
  grp_col <- paste0("miss_grp_", ms$base)
  eval_df[[grp_col]] <- factor(
    vals, 0:1,
    labels = c(paste0(ms$label, " Observed (n=", n_obs, ")"),
               paste0(ms$label, " Missing (n=",  n_miss, ")"))
  )
  wis_tbl <- subgroup_wis_table(eval_df, grp_col)
  out_miss_var_list[[ms$base]] <- wis_tbl
  cat(sprintf("\n--- WIS by %s missingness  [%s] ---\n",
              ms$label, ms$interpretation))
  print(wis_tbl)
}


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

cat("\nSampling from predictive distributions (all non-parametric)...\n")
samples_list <- lapply(names(all_qgrid), function(m) {
  sample_from_quantiles(as.matrix(all_qgrid[[m]]), q_grid, n_spp)
})
names(samples_list) <- names(all_qgrid)

n_act <- length(y_te); n_mod <- n_act * n_spp

df_dist <- bind_rows(
  data.frame(LOS = y_te, Model = "Actual"),
  bind_rows(lapply(names(samples_list), function(m)
    data.frame(LOS = as.vector(samples_list[[m]]), Model = m)))
) %>% filter(LOS >= 0, LOS <= quantile(y_te, 0.99) * 1.5)

all_levels <- c("Actual", model_names)
df_dist$Model <- factor(df_dist$Model, levels = all_levels)

MODEL_COLOR <- c(
  Actual       = "gray20",
  DRF          = "#D65C5C", QRF       = "#5A9BD4", RF            = "#66BB66",
  XGB          = "#E8A838", ENS_NP    = "#9B59B6", MCD_NP        = "#E67E22",
  DDNN_NP      = "#1ABC9C", Ensemble_DRF = "#C0392B", BNN_NP     = "#2471A3",
  Hybrid_NN_DRF = "#6E2F1A"
)
MODEL_FILL <- c(
  DRF          = "#F4A3A3", QRF       = "#A8C5E5", RF            = "#B7E3B0",
  XGB          = "#FAD7A0", ENS_NP    = "#D7BDE2", MCD_NP        = "#FDEBD0",
  DDNN_NP      = "#A3E4D7", Ensemble_DRF = "#F1948A", BNN_NP    = "#AED6F1",
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

gamma_nn_models  <- c("ENS_NP","MCD_NP","DDNN_NP","BNN_NP")
forest_models    <- c("DRF","QRF","RF","Ensemble_DRF")

best_gamma_wis   <- min(wis_vec[gamma_nn_models], na.rm=TRUE)
best_forest_wis  <- min(wis_vec[forest_models],   na.rm=TRUE)
hybrid_wis_val   <- wis_vec["Hybrid_NN_DRF"]

cat(sprintf("Best NN WIS:         %.4f  (%s)\n", best_gamma_wis,
            names(which.min(wis_vec[gamma_nn_models]))))
cat(sprintf("Best Forest WIS:     %.4f  (%s)\n", best_forest_wis,
            names(which.min(wis_vec[forest_models]))))
cat(sprintf("Hybrid NN+DRF WIS:   %.4f\n", hybrid_wis_val))

beats_nn     <- hybrid_wis_val < best_gamma_wis
beats_forest <- hybrid_wis_val < best_forest_wis
if (beats_nn && beats_forest) {
  cat("\n✓ ✓ ✓  Hybrid beats BOTH NN and forest categories!\n")
  cat(sprintf("  Δ vs best NN:      %.2f%%\n", 100*(best_gamma_wis-hybrid_wis_val)/best_gamma_wis))
  cat(sprintf("  Δ vs best forest:  %.2f%%\n", 100*(best_forest_wis-hybrid_wis_val)/best_forest_wis))
} else if (beats_nn)     cat("\n⚠ Hybrid beats NN but not forest\n")
else if (beats_forest)   cat("\n⚠ Hybrid beats forest but not NN\n")
else                     cat("\n✗ Hybrid does not beat either category\n")


# ---- Save outputs ----
write.csv(summary_all,  "/mnt/user-data/outputs/summary_all_models.csv",       row.names=FALSE)
write.csv(out_vent_wis, "/mnt/user-data/outputs/subgroup_vent_wis.csv",        row.names=FALSE)
write.csv(out_vaso_wis, "/mnt/user-data/outputs/subgroup_vaso_wis.csv",        row.names=FALSE)
write.csv(out_los_wis,  "/mnt/user-data/outputs/subgroup_los_wis.csv",         row.names=FALSE)
if (!is.null(out_vasopressin_wis))
  write.csv(out_vasopressin_wis, "/mnt/user-data/outputs/subgroup_vasopressin_wis.csv", row.names=FALSE)
if (!is.null(out_sepsis_wis))
  write.csv(out_sepsis_wis,      "/mnt/user-data/outputs/subgroup_sepsis_wis.csv",      row.names=FALSE)
if (!is.null(out_stroke_wis))
  write.csv(out_stroke_wis,      "/mnt/user-data/outputs/subgroup_stroke_wis.csv",      row.names=FALSE)
if (!is.null(out_unit_wis))
  write.csv(out_unit_wis,        "/mnt/user-data/outputs/subgroup_unit_wis.csv",        row.names=FALSE)
# Save per-variable missingness subgroups
for (nm in names(out_miss_var_list)) {
  write.csv(out_miss_var_list[[nm]],
            sprintf("/mnt/user-data/outputs/subgroup_miss_%s_wis.csv", nm),
            row.names = FALSE)
}

saveRDS(list(
  summary          = summary_all,
  wis              = wis_vec,
  crps             = crps_vec,
  nll              = nll_vec,
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
  ddnn_runs        = ddnn_runs,
  edrf_members     = drf_base_full,
  bnn_net          = bnn_net,
  hybrid_drf       = hybrid_drf,
  nn_feat_extractor = nn_feat_extractor
), "/mnt/user-data/outputs/all_model_results.rds")

cat("\n✓ All results saved to /mnt/user-data/outputs/\n")
cat("\n========================================\n")
cat("✓ ✓ ✓  ALL 10 MODELS COMPLETE  ✓ ✓ ✓\n")
cat("========================================\n\n")
