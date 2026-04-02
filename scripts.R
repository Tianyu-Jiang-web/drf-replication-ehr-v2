library(dplyr)
library(readr)
library(lubridate)
library(tidyr)


out_dir <- "/Users/ame/MScHDS/Dissertation materials/assignment/reproduction/drf-replication-ehr-v3/figures/final"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)


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

# ── theme settings ────────────────────────────────────────────────────
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

# color palette
pal_cat <- c("#2E75B6","#ED7D31","#70AD47","#FFC000","#9B59B6",
             "#E74C3C","#1ABC9C","#95A5A6","#F39C12","#2980B9",
             "#8E44AD","#16A085","#D35400","#2C3E50","#27AE60","#C0392B")
COL_1 <- "#2E75B6"
COL_0 <- "#ED7D31"

# full dataset for visualization
df_vis <- X_tree %>% mutate(y = y)
y_cap  <- quantile(df_vis$y, 0.99, na.rm = TRUE)
df_vis <- df_vis %>% mutate(y_plot = pmin(y, y_cap))

N_lab    <- scales::comma(nrow(df_vis))
cap_note <- paste0("n = ", N_lab, "  |  LOS capped at 99th pct (",
                   round(y_cap, 1), " days)")

# ════════════════════════════════════════════════════════════════════════════
# HELPER 1: violin + boxplot
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
# HELPER 2: ridgeline for BINARY variables
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
# HELPER 3: ridgeline for MISSINGNESS
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

# Fig1/2: violin
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
ggsave(file.path(out_dir, "los_fig1_categorical.png"),   fig1, width=14, height=11, dpi=180, bg="white")
ggsave(file.path(out_dir, "los_fig2_careunit.png"),      fig2, width=12, height=7,  dpi=180, bg="white")
ggsave(file.path(out_dir, "los_fig3_comorbidities.png"), fig3, width=12, height=10, dpi=180, bg="white")
ggsave(file.path(out_dir, "los_fig4_medications.png"),   fig4, width=12, height=7,  dpi=180, bg="white")
ggsave(file.path(out_dir, "los_fig5_interventions.png"), fig5, width=12, height=7,  dpi=180, bg="white")
ggsave(file.path(out_dir, "los_fig6_missingness.png"),   fig6, width=12, height=10, dpi=180, bg="white")

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

# ---------------------------------------------------------
# 5b) clean all (QRF) features — train-led
# ---------------------------------------------------------
miss_cols_all <- grep("_miss$", names(Xtr_all), value = TRUE)

miss_all_zero_all <- miss_cols_all[
  sapply(miss_cols_all, function(v) sum(Xtr_all[[v]], na.rm = TRUE) == 0)
]

const_cols_all <- names(Xtr_all)[
  sapply(Xtr_all, function(x) dplyr::n_distinct(x, na.rm = TRUE) <= 1)
]

drop_cols_all <- union(miss_all_zero_all, const_cols_all)

Xtr_all <- Xtr_all %>% select(-any_of(drop_cols_all))
Xte_all <- Xte_all %>% select(-any_of(drop_cols_all))

cat("All (QRF) feature cleanup:\n")
cat("  Removed useless *_miss (all-zero in train):", length(miss_all_zero_all), "\n")
cat("  Removed constant columns (in train):", length(const_cols_all), "\n")
cat("  Xtr_all cols:", ncol(Xtr_all), " | Xte_all cols:", ncol(Xte_all), "\n")


# ============================================================
# For hyperparameter tuning
# ============================================================
library(caret)

# Create 3-fold CV splits (balance speed vs robustness)
set.seed(2026)
cv_folds_hp <- createFolds(y_tr, k = 3, list = TRUE, returnTrain = FALSE)

# q_grid defined early — needed by CRPS tuning helpers
q_grid <- seq(0.05, 0.95, by = 0.05)

# ============================================================
# CRPS helper for tuning (per-observation CRPS, then averaged)
# This is the PRIMARY tuning metric for ALL models (req. 2, 7).
#
# CRPS(F, y) = 2 * integral_0^1 L_tau(y, Q(tau)) d(tau)
# Approximated via trapezoidal weights over the quantile grid.
# Each observation gets its own CRPS; the function returns the mean.
# ============================================================
pinball_fn_vec <- function(y, q, tau) {
  u <- y - q
  ifelse(u >= 0, tau * u, (tau - 1) * u)
}

# Compute per-observation CRPS then average (req. 7)
# qmat: [n x K] matrix of predicted quantiles
# taus: vector of K probability levels
# Returns: scalar mean CRPS
crps_from_quantiles <- function(y, qmat, taus) {
  ord   <- order(taus)
  taus  <- taus[ord]
  qmat  <- qmat[, ord, drop = FALSE]
  K     <- length(taus)
  
  # Trapezoidal integration weights
  w <- numeric(K)
  w[1]  <- (taus[2] - taus[1]) / 2
  w[K]  <- (taus[K] - taus[K-1]) / 2
  if (K > 2)
    for (j in 2:(K-1)) w[j] <- (taus[j+1] - taus[j-1]) / 2
  
  # Per-observation CRPS: 2 * sum_k w[k] * L_tau[k](y_i, q_ik)
  # [n x K] pinball matrix, then weighted row sums, then times 2
  n <- length(y)
  crps_per_obs <- numeric(n)
  for (j in seq_len(K)) {
    crps_per_obs <- crps_per_obs + w[j] * pinball_fn_vec(y, qmat[, j], taus[j])
  }
  crps_per_obs <- 2 * crps_per_obs  # per-observation CRPS
  
  mean(crps_per_obs, na.rm = TRUE)  # average CRPS (req. 7)
}

# Scalar CRPS for a non-parametric quantile net (used in tuning loops)
crps_from_qmat <- function(y, qmat) {
  crps_from_quantiles(y, as.matrix(qmat), q_grid)
}


# ============================================================
# 2 - DRF model
# ============================================================
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

# ============================================================
# HP TUNING: DRF — CRPS-based (req. 1 & 2)
# Grid: num.trees x min.node.size = 2 x 4 = 8 configs, 1 CV fold
# ============================================================
cat("\n========================================\n")
cat("DRF: Tuning HPs using CRPS (re-tuned, req. 1 & 2)\n")
cat("========================================\n")

drf_hp_grid <- expand.grid(
  num_trees     = c(500, 1000),
  min_node_size = c(5, 10, 20, 50),
  stringsAsFactors = FALSE
)

val_idx_drf   <- cv_folds_hp[[1]]
train_idx_drf <- setdiff(seq_len(nrow(Xtr_tree)), val_idx_drf)

drf_cv_crps <- sapply(seq_len(nrow(drf_hp_grid)), function(i) {
  hp <- drf_hp_grid[i, ]
  cat(sprintf("  [%d/%d] trees=%d node=%d ... ", i, nrow(drf_hp_grid), hp$num_trees, hp$min_node_size))
  tryCatch({
    fit <- drf(
      X = Xtr_tree[train_idx_drf, , drop = FALSE],
      Y = y_tr[train_idx_drf],
      num.trees = hp$num_trees,
      min.node.size = hp$min_node_size,
      mtry = floor(sqrt(ncol(Xtr_tree))),
      sample.fraction = 0.5
    )
    pr  <- predict(fit, Xtr_tree[val_idx_drf, , drop = FALSE])
    qm  <- wquant_matrix(as.numeric(pr$y), pr$weights, q_grid)
    cr  <- crps_from_qmat(y_tr[val_idx_drf], qm)
    cat(sprintf("CRPS=%.4f\n", cr)); cr
  }, error = function(e) { cat("ERROR\n"); Inf })
})

best_drf_i  <- which.min(drf_cv_crps)
best_drf_hp <- as.list(drf_hp_grid[best_drf_i, ])
cat(sprintf("✓ Best DRF HP: trees=%d node=%d (CRPS=%.4f)\n",
            best_drf_hp$num_trees, best_drf_hp$min_node_size, drf_cv_crps[best_drf_i]))

num_trees_drf     <- best_drf_hp$num_trees
min_node_size_drf <- best_drf_hp$min_node_size

# --- Final Fit ---
set.seed(2026)
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

# compute quantiles
drf_q_mat <- wquant_matrix(y_train, W, c(0.05, 0.5, 0.95))
drf_q <- as.data.frame(drf_q_mat)
colnames(drf_q) <- c("q05", "q50", "q95")

# full quantile grid
drf_qgrid_mat <- wquant_matrix(y_train, W, q_grid)
drf_qgrid <- as.data.frame(drf_qgrid_mat)
colnames(drf_qgrid) <- paste0("q", sprintf("%02d", round(100*q_grid)))

cat("\n✓ DRF complete.\n")


# ============================================================
# 3 - QRF model
# ============================================================
library(quantregForest)

# ============================================================
# HP TUNING: QRF — CRPS-based (req. 1 & 2)
# Grid: ntree x nodesize = 2 x 3 = 6 configs
# ============================================================
cat("\n========================================\n")
cat("QRF: Tuning HPs using CRPS (re-tuned, req. 1 & 2)\n")
cat("========================================\n")

qrf_hp_grid <- expand.grid(
  ntree    = c(500, 1000),
  nodesize = c(10, 20, 50),
  stringsAsFactors = FALSE
)

val_idx_qrf   <- cv_folds_hp[[1]]
train_idx_qrf <- setdiff(seq_len(nrow(Xtr_all)), val_idx_qrf)

qrf_cv_crps <- sapply(seq_len(nrow(qrf_hp_grid)), function(i) {
  hp <- qrf_hp_grid[i, ]
  cat(sprintf("  [%d/%d] ntree=%d nodesize=%d ... ", i, nrow(qrf_hp_grid), hp$ntree, hp$nodesize))
  tryCatch({
    fit <- quantregForest(
      x = Xtr_all[train_idx_qrf, , drop = FALSE],
      y = y_tr[train_idx_qrf],
      ntree = hp$ntree,
      nodesize = hp$nodesize
    )
    qm <- as.matrix(predict(fit, Xtr_all[val_idx_qrf, , drop = FALSE], what = q_grid))
    cr <- crps_from_qmat(y_tr[val_idx_qrf], qm)
    cat(sprintf("CRPS=%.4f\n", cr)); cr
  }, error = function(e) { cat("ERROR\n"); Inf })
})

best_qrf_i  <- which.min(qrf_cv_crps)
best_qrf_hp <- as.list(qrf_hp_grid[best_qrf_i, ])
cat(sprintf("✓ Best QRF HP: ntree=%d nodesize=%d (CRPS=%.4f)\n",
            best_qrf_hp$ntree, best_qrf_hp$nodesize, qrf_cv_crps[best_qrf_i]))

ntree_qrf    <- best_qrf_hp$ntree
nodesize_qrf <- best_qrf_hp$nodesize

qs <- c(0.05, 0.5, 0.95)

# fit qrf model
qrf_fit <- quantregForest(
  x = Xtr_all,
  y = y_tr,
  ntree = ntree_qrf,
  nodesize = nodesize_qrf
)

qrf_q <- as.data.frame(
  predict(qrf_fit, Xte_all, what = qs)
)
colnames(qrf_q) <- c("q05","q50","q95")

qrf_qgrid <- as.data.frame(predict(qrf_fit, Xte_all, what = q_grid))
colnames(qrf_qgrid) <- paste0("q", sprintf("%02d", round(100*q_grid)))
cat("✓ QRF complete.\n")


# ============================================================
# 4 - Ranger (RF quantile regression)
# ============================================================
library(ranger)

# ============================================================
# HP TUNING: Ranger — CRPS-based (req. 1 & 2)
# Grid: num.trees x min.node.size = 2 x 3 = 6 configs
# ============================================================
cat("\n========================================\n")
cat("Ranger RF: Tuning HPs using CRPS (re-tuned, req. 1 & 2)\n")
cat("========================================\n")

ranger_hp_grid <- expand.grid(
  num_trees     = c(500, 1000),
  min_node_size = c(5, 10, 20),
  stringsAsFactors = FALSE
)

val_idx_rng   <- cv_folds_hp[[1]]
train_idx_rng <- setdiff(seq_len(nrow(Xtr_tree)), val_idx_rng)

ranger_cv_crps <- sapply(seq_len(nrow(ranger_hp_grid)), function(i) {
  hp <- ranger_hp_grid[i, ]
  cat(sprintf("  [%d/%d] trees=%d node=%d ... ", i, nrow(ranger_hp_grid), hp$num_trees, hp$min_node_size))
  tryCatch({
    fit <- ranger(
      x = Xtr_tree[train_idx_rng, , drop = FALSE],
      y = y_tr[train_idx_rng],
      num.trees = hp$num_trees,
      min.node.size = hp$min_node_size,
      quantreg = TRUE,
      keep.inbag = TRUE
    )
    qm <- as.matrix(predict(fit, Xtr_tree[val_idx_rng, , drop = FALSE],
                            type = "quantiles", quantiles = q_grid)$predictions)
    cr <- crps_from_qmat(y_tr[val_idx_rng], qm)
    cat(sprintf("CRPS=%.4f\n", cr)); cr
  }, error = function(e) { cat("ERROR\n"); Inf })
})

best_rng_i  <- which.min(ranger_cv_crps)
best_rng_hp <- as.list(ranger_hp_grid[best_rng_i, ])
cat(sprintf("✓ Best Ranger HP: trees=%d node=%d (CRPS=%.4f)\n",
            best_rng_hp$num_trees, best_rng_hp$min_node_size, ranger_cv_crps[best_rng_i]))

num_trees_ranger     <- best_rng_hp$num_trees
min_node_size_ranger <- best_rng_hp$min_node_size

rf_q_fit <- ranger(
  x = Xtr_tree,
  y = y_tr,
  num.trees = num_trees_ranger,
  min.node.size = min_node_size_ranger,
  quantreg = TRUE,
  keep.inbag = TRUE
)

rf_q <- as.data.frame(
  predict(rf_q_fit, Xte_tree, type = "quantiles", quantiles = qs)$predictions
)
colnames(rf_q) <- c("q05","q50","q95")

rf_qgrid <- as.data.frame(
  predict(rf_q_fit, Xte_tree, type = "quantiles", quantiles = q_grid)$predictions
)
colnames(rf_qgrid) <- paste0("q", sprintf("%02d", round(100*q_grid)))
cat("✓ Ranger RF complete.\n")


# ============================================================
# 5 - XGBoost: direct quantile regression via pinball loss (req. 3)
#
# CHANGE from v1: instead of training a single MSE model + conformal
# bands, we now train K separate XGBoost models, one per quantile
# in q_grid, each using "reg:quantileerror" (quantile/pinball loss).
# This directly predicts quantiles without conformal assumptions.
#
# HP TUNING: CRPS-based (req. 1 & 2)
# Grid: max_depth x eta x nrounds = 2 x 2 x 2 = 8 configs
# Tuning is done on the MEAN quantile over all tau (a single model
# per config, then CRPS evaluated on val set predictions at all q_grid).
# For speed we tune a "representative" single-alpha model (alpha=0.5,
# i.e. MAE/median), then use those HPs for all quantiles.
# ============================================================
library(xgboost)
set.seed(2026)

stopifnot(exists("Xtr_all"), exists("Xte_all"), exists("y_tr"))
stopifnot(nrow(Xtr_all) == length(y_tr))

# One-hot encoding for XGBoost
Xtr_all_mm_hp <- model.matrix(~ . - 1, data = Xtr_all)
Xte_all_mm    <- model.matrix(~ . - 1, data = Xte_all)

# Align columns (req. 3: same approach as before for feature alignment)

common_cols <- union(colnames(Xtr_all_mm_hp), colnames(Xte_all_mm))
Xtr_all_mm_hp <- Xtr_all_mm_hp[, common_cols, drop = FALSE]
Xte_all_mm    <- Xte_all_mm[,    common_cols, drop = FALSE]

Xtr_all_mm_hp[is.na(Xtr_all_mm_hp)] <- 0
Xte_all_mm[is.na(Xte_all_mm)]       <- 0

cat("\n========================================\n")
cat("XGBoost: Tuning HPs using CRPS + quantile loss (req. 1, 2, 3)\n")
cat("========================================\n")

# ---- HP tuning: use median regression (alpha=0.5) for speed, then evaluate CRPS ----
val_idx_xgb   <- cv_folds_hp[[1]]
train_idx_xgb <- setdiff(seq_len(nrow(Xtr_all_mm_hp)), val_idx_xgb)

dtrain_cv <- xgb.DMatrix(Xtr_all_mm_hp[train_idx_xgb, , drop = FALSE],
                         label = y_tr[train_idx_xgb])
dval_cv   <- xgb.DMatrix(Xtr_all_mm_hp[val_idx_xgb,   , drop = FALSE],
                         label = y_tr[val_idx_xgb])

xgb_hp_grid <- expand.grid(
  max_depth = c(4, 6),
  eta       = c(0.05, 0.1),
  nrounds   = c(300, 500),
  stringsAsFactors = FALSE
)

xgb_cv_crps <- sapply(seq_len(nrow(xgb_hp_grid)), function(i) {
  hp <- xgb_hp_grid[i, ]
  cat(sprintf("  [%d/%d] depth=%d eta=%.2f nrounds=%d ... ",
              i, nrow(xgb_hp_grid), hp$max_depth, hp$eta, hp$nrounds))
  tryCatch({
    # Train K quantile models on CV train split, predict on val, compute CRPS
    qpreds_val <- matrix(NA_real_, length(val_idx_xgb), length(q_grid))
    for (ki in seq_along(q_grid)) {
      tau_k <- q_grid[ki]
      params_k <- list(
        objective  = "reg:quantileerror",
        quantile_alpha = tau_k,
        max_depth  = hp$max_depth,
        eta        = hp$eta,
        verbosity  = 0
      )
      fit_k <- xgb.train(params = params_k, data = dtrain_cv,
                         nrounds = hp$nrounds, verbose = 0)
      qpreds_val[, ki] <- predict(fit_k, dval_cv)
    }
    # Enforce monotonicity across quantiles
    qpreds_val <- t(apply(qpreds_val, 1, sort))
    cr <- crps_from_qmat(y_tr[val_idx_xgb], qpreds_val)
    cat(sprintf("CRPS=%.4f\n", cr)); cr
  }, error = function(e) { cat(sprintf("ERROR: %s\n", conditionMessage(e))); Inf })
})

best_xgb_i  <- which.min(xgb_cv_crps)
best_xgb_hp <- as.list(xgb_hp_grid[best_xgb_i, ])
cat(sprintf("✓ Best XGBoost HP: depth=%d eta=%.2f nrounds=%d (CRPS=%.4f)\n",
            best_xgb_hp$max_depth, best_xgb_hp$eta, best_xgb_hp$nrounds,
            xgb_cv_crps[best_xgb_i]))

max_depth_xgb <- best_xgb_hp$max_depth
eta_xgb       <- best_xgb_hp$eta
nrounds_xgb   <- best_xgb_hp$nrounds

# ---- Final fit: train one model per quantile on full training set (req. 3) ----
cat(sprintf("\nXGBoost: Training %d quantile models on full train set...\n", length(q_grid)))

dtrain_full <- xgb.DMatrix(Xtr_all_mm_hp, label = y_tr)
dtest_full  <- xgb.DMatrix(Xte_all_mm)

xgb_quantile_models <- vector("list", length(q_grid))
xgb_qgrid_mat       <- matrix(NA_real_, nrow(Xte_all_mm), length(q_grid))

for (ki in seq_along(q_grid)) {
  tau_k  <- q_grid[ki]
  cat(sprintf("  Fitting quantile tau=%.2f (%d/%d)...\n", tau_k, ki, length(q_grid)))
  params_k <- list(
    objective      = "reg:quantileerror",
    quantile_alpha = tau_k,
    max_depth      = max_depth_xgb,
    eta            = eta_xgb,
    verbosity      = 0
  )
  fit_k <- xgb.train(params = params_k, data = dtrain_full,
                     nrounds = nrounds_xgb, verbose = 0)
  xgb_quantile_models[[ki]] <- fit_k
  xgb_qgrid_mat[, ki]       <- predict(fit_k, dtest_full)
}

# Enforce monotonicity and clip to training range lower bound
xgb_qgrid_mat <- t(apply(xgb_qgrid_mat, 1, sort))
xgb_qgrid_mat <- pmax(xgb_qgrid_mat, min(y_tr, na.rm = TRUE))

q05_idx_xgb <- which.min(abs(q_grid - 0.05))
q50_idx_xgb <- which.min(abs(q_grid - 0.50))
q95_idx_xgb <- which.min(abs(q_grid - 0.95))

xgb_q <- data.frame(
  q05 = xgb_qgrid_mat[, q05_idx_xgb],
  q50 = xgb_qgrid_mat[, q50_idx_xgb],
  q95 = xgb_qgrid_mat[, q95_idx_xgb]
)
xgb_qgrid <- as.data.frame(xgb_qgrid_mat)
colnames(xgb_qgrid) <- paste0("q", sprintf("%02d", round(100*q_grid)))
cat("✓ XGBoost (direct quantile regression) complete.\n")


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
# ============================================================
pinball_loss_grid <- function(pred, y_true, tau_t) {
  err  <- y_true - pred
  loss <- torch_where(err >= 0, tau_t * err, (tau_t - 1) * err)
  torch_mean(loss)
}

# ============================================================
# SHARED: Dataset / DataLoader builder
# ============================================================
make_dl <- function(X_sc, y_obs, batch_size = 512) {
  X_t <- torch_tensor(as.matrix(X_sc), dtype = torch_float(), device = device)
  y_t <- torch_tensor(matrix(as.numeric(y_obs), ncol = 1), dtype = torch_float(), device = device)
  ds  <- tensor_dataset(X_t, y_t)
  dataloader(ds, batch_size = batch_size, shuffle = TRUE)
}

# ============================================================
# SHARED: Quantile head — output n_quantiles directly
# ============================================================
tau_t <- torch_tensor(matrix(q_grid, nrow = 1), dtype = torch_float(), device = device)

# CRPS helper for non-parametric net (primary tuning metric, req. 2)
qnet_crps <- function(net, X_sc, y_obs) {
  pm <- get_qgrid(net, X_sc)
  crps_from_qmat(y_obs, pm)
}

# Extract sorted quantile grid from non-parametric net
get_qgrid <- function(net, X_sc) {
  net$eval()
  Xv <- torch_tensor(X_sc, dtype = torch_float(), device = device)
  with_no_grad({ pm <- as.matrix(net(Xv)$cpu()) })
  net$train()
  t(apply(pm, 1, sort))
}


# ============================================================
# MODEL 6: Deep Ensemble (Gamma parametric)
# M=5 independent DNNs, each trained with Gamma NLL.
# Each member outputs (log_shape, log_rate); at prediction time
# Gamma parameters are averaged across members (parameter-space
# ensemble), then converted to quantiles via qgamma().
# This follows the original Lakshminarayanan et al. (2017) spirit
# of parametric output heads — consistent with MCD, DDNN, BNN.
# HP TUNING: CRPS-based (req. 1 & 2)
# ============================================================
# ---- Shared Gamma helpers ----
gamma_nll_loss <- function(params, y_true) {
  log_shape <- params[, 1, drop = FALSE]
  log_rate  <- params[, 2, drop = FALSE]
  shape <- torch_exp(log_shape)
  rate  <- torch_exp(log_rate)
  nll <- -(shape - 1) * torch_log(y_true) +
    y_true * rate -
    shape * torch_log(rate) +
    torch_lgamma(shape)
  torch_mean(nll)
}

get_gamma_params <- function(net, X_sc) {
  X_sc <- as.matrix(X_sc)
  net$eval()
  Xv <- torch_tensor(X_sc, dtype = torch_float(), device = device)
  with_no_grad({ p <- as.matrix(net(Xv)$cpu()) })
  net$train()
  list(shape = exp(p[, 1]), rate = exp(p[, 2]))
}

gamma_qgrid_fn <- function(shape, rate, qs) {
  t(mapply(function(sh, ra) qgamma(qs, shape = sh, rate = ra), shape, rate))
}
cat("\n========================================\n")
cat("MODEL 6: Deep Ensemble (Gamma parametric)\n")
cat("========================================\n")

# Network: outputs 2 Gamma params (log_shape, log_rate)
make_ens_gamma_net <- function(n_in, h1, h2) {
  nn_sequential(
    nn_linear(n_in, h1), nn_relu(),
    nn_linear(h1,   h2), nn_relu(),
    nn_linear(h2,   2)   # log_shape, log_rate
  )
}

train_ens_gamma <- function(X_sc, y_obs, h1, h2, lr,
                            epochs = 60, batch_size = 512) {
  net   <- make_ens_gamma_net(n_input, h1, h2)$to(device = device)
  optim <- optim_adam(net$parameters, lr = lr)
  dl    <- make_dl(X_sc, y_obs, batch_size)
  net$train()
  for (ep in seq_len(epochs)) {
    coro::loop(for (b in dl) {
      x_batch <- b[[1]]
      y_batch <- b[[2]]
      optim$zero_grad()
      params <- net(x_batch)
      loss   <- gamma_nll_loss(params, y_batch)
      loss$backward()
      optim$step()
    })
  }
  net$eval(); net
}

# ---- HP TUNING: ENS Gamma — CRPS-based (req. 2) ----
cat("ENS: Tuning HPs under Gamma NLL using CRPS (12 configs, 1 CV fold)...\n")
ens_hp_grid <- expand.grid(
  h1 = c(128, 256),
  h2 = c(32, 64),
  lr = c(1e-4, 5e-4, 1e-3),
  stringsAsFactors = FALSE
)

ens_cv_crps <- sapply(seq_len(nrow(ens_hp_grid)), function(i) {
  hp <- ens_hp_grid[i, ]
  cat(sprintf("  [%d/12] h1=%d h2=%d lr=%.0e ... ", i, hp$h1, hp$h2, hp$lr))
  tryCatch({
    net <- train_ens_gamma(X_tr_cv, y_tr_cv, hp$h1, hp$h2, hp$lr, epochs = 60)
    p   <- get_gamma_params(net, X_va_cv)
    qm  <- gamma_qgrid_fn(p$shape, p$rate, q_grid)
    cr  <- crps_from_qmat(y_va_cv, qm)
    cat(sprintf("CRPS=%.4f\n", cr)); cr
  }, error = function(e) { cat("ERROR\n"); Inf })
})

best_ens_i  <- which.min(ens_cv_crps)
best_ens_hp <- as.list(ens_hp_grid[best_ens_i, ])
cat(sprintf("✓ Best ENS HP: h1=%d h2=%d lr=%.0e (CRPS=%.4f)\n",
            best_ens_hp$h1, best_ens_hp$h2, best_ens_hp$lr, ens_cv_crps[best_ens_i]))

# Train M members on full training set
M_ens       <- 5
ens_members <- vector("list", M_ens)
cat(sprintf("Training %d ENS members (150 epochs each)...\n", M_ens))
for (m in seq_len(M_ens)) {
  set.seed(2026 + m)
  cat(sprintf("  Member %d/%d\n", m, M_ens))
  ens_members[[m]] <- train_ens_gamma(Xtr_ens_sc, y_tr,
                                      best_ens_hp$h1, best_ens_hp$h2, best_ens_hp$lr,
                                      epochs = 150)
}

# Predict: average Gamma parameters across M members (parameter-space ensemble),
# then convert to quantile grid via qgamma()
cat("Computing ENS predictions (Gamma parameter averaging across members)...\n")
ens_params_runs <- lapply(ens_members, function(net) get_gamma_params(net, Xte_ens_sc))
ens_shape_avg   <- Reduce(`+`, lapply(ens_params_runs, `[[`, "shape")) / M_ens
ens_rate_avg    <- Reduce(`+`, lapply(ens_params_runs, `[[`, "rate"))  / M_ens
ens_qgrid_mat   <- gamma_qgrid_fn(ens_shape_avg, ens_rate_avg, q_grid)
ens_qgrid_mat   <- pmax(ens_qgrid_mat, 1)

ens_q <- data.frame(q05 = ens_qgrid_mat[, q05_idx],
                    q50 = ens_qgrid_mat[, q50_idx],
                    q95 = ens_qgrid_mat[, q95_idx])
ens_qgrid <- as.data.frame(ens_qgrid_mat)
colnames(ens_qgrid) <- paste0("q", sprintf("%02d", round(100 * q_grid)))

# NLL: Gamma parametric (averaged parameters)
nll_ens <- -mean(dgamma(y_te, shape = ens_shape_avg, rate = ens_rate_avg, log = TRUE))

cat("ENS (Gamma) complete.\n\n")


# ============================================================
# MODEL 7: MC Dropout (Gamma parametric assumption, req. 6)
# Single DNN with dropout; outputs Gamma params (log_shape, log_rate).
# At test time keep dropout ON and average Gamma params over M passes,
# then convert to quantiles via Gamma CDF.
# HP TUNING: CRPS-based (req. 2)
# ============================================================
cat("========================================\n")
cat("MODEL 7: MC Dropout (Gamma parametric, req. 6)\n")
cat("========================================\n")



# MCD network architecture: outputs 2 Gamma params
make_mcd_gamma_net <- function(n_in, h1 = 128, h2 = 64, p_drop = 0.1) {
  nn_sequential(
    nn_linear(n_in, h1), nn_relu(), nn_dropout(p_drop),
    nn_linear(h1,   h2), nn_relu(), nn_dropout(p_drop),
    nn_linear(h2,   2)   # log_shape, log_rate
  )
}

train_mcd_gamma <- function(X_sc, y_obs, p_drop, lr,
                            h1 = 128, h2 = 64, epochs = 60, batch_size = 512) {
  net   <- make_mcd_gamma_net(n_input, h1, h2, p_drop)$to(device = device)
  optim <- optim_adam(net$parameters, lr = lr)
  dl    <- make_dl(X_sc, y_obs, batch_size)
  net$train()
  for (ep in seq_len(epochs)) {
    coro::loop(for (b in dl) {
      x_batch <- b[[1]]
      y_batch <- b[[2]]
      optim$zero_grad()
      params <- net(x_batch)
      loss   <- gamma_nll_loss(params, y_batch)
      loss$backward()
      optim$step()
    })
  }
  net  # keep in train mode for MC dropout inference
}

# MC dropout inference: average Gamma parameters over M passes, then quantiles
mcd_gamma_quantiles <- function(net, X_sc, M_pass = 50) {
  X_sc <- as.matrix(X_sc)
  net$train()
  X_t   <- torch_tensor(X_sc, dtype = torch_float(), device = device)
  acc_p <- matrix(0, nrow(X_sc), 2)
  with_no_grad({
    for (p in seq_len(M_pass)) {
      acc_p <- acc_p + as.matrix(net(X_t)$cpu())
    }
  })
  avg_params <- acc_p / M_pass
  shape <- exp(avg_params[, 1])
  rate  <- exp(avg_params[, 2])
  gamma_qgrid_fn(shape, rate, q_grid)
}

# ---- HP TUNING: MCD Gamma — CRPS-based (req. 2) ----
cat("MCD: Tuning HPs under Gamma NLL using CRPS (6 configs)...\n")
mcd_hp_grid <- expand.grid(
  p_drop = c(0.10, 0.20),
  lr     = c(1e-4, 5e-4, 1e-3),
  stringsAsFactors = FALSE
)

mcd_cv_crps <- sapply(seq_len(nrow(mcd_hp_grid)), function(i) {
  hp <- mcd_hp_grid[i, ]
  cat(sprintf("  [%d/6] dropout=%.2f lr=%.0e ... ", i, hp$p_drop, hp$lr))
  tryCatch({
    net <- train_mcd_gamma(X_tr_cv, y_tr_cv, hp$p_drop, hp$lr, epochs = 60)
    qm  <- mcd_gamma_quantiles(net, X_va_cv, M_pass = 20)
    cr  <- crps_from_qmat(y_va_cv, qm)
    cat(sprintf("CRPS=%.4f\n", cr)); cr
  }, error = function(e) { cat("ERROR\n"); Inf })
})

best_mcd_i  <- which.min(mcd_cv_crps)
best_mcd_hp <- as.list(mcd_hp_grid[best_mcd_i, ])
cat(sprintf("✓ Best MCD HP: dropout=%.2f lr=%.0e (CRPS=%.4f)\n",
            best_mcd_hp$p_drop, best_mcd_hp$lr, mcd_cv_crps[best_mcd_i]))

set.seed(2026)
cat("Training final MCD model (150 epochs)...\n")
mc_dropout_net <- train_mcd_gamma(Xtr_ens_sc, y_tr,
                                  best_mcd_hp$p_drop, best_mcd_hp$lr,
                                  epochs = 150)

M_dropout     <- 100
cat(sprintf("Generating %d MC passes on test set...\n", M_dropout))
mcd_qgrid_mat <- mcd_gamma_quantiles(mc_dropout_net, Xte_ens_sc, M_pass = M_dropout)
mcd_qgrid_mat <- pmax(mcd_qgrid_mat, 1)

mcd_q <- data.frame(q05 = mcd_qgrid_mat[, q05_idx],
                    q50 = mcd_qgrid_mat[, q50_idx],
                    q95 = mcd_qgrid_mat[, q95_idx])
mcd_qgrid <- as.data.frame(mcd_qgrid_mat)
colnames(mcd_qgrid) <- paste0("q", sprintf("%02d", round(100 * q_grid)))
cat("MCD (Gamma) complete.\n\n")


# ============================================================
# MODEL 8: DDNN — Distributional DNN (Gamma, req. 6)
# Following Marcjasz et al. (2023):
#   N_runs=4, per-run HP optimisation, qEns (horizontal averaging).
# HP TUNING: CRPS-based (req. 1 & 2)
# Fixes: (1) expanded HP grid with smaller networks;
#         (2) (log_mu, log_var) reparametrisation to avoid variance collapse.
# ============================================================
cat("========================================\n")
cat("MODEL 8: DDNN (Gamma, Marcjasz et al. 2023)\n")
cat("========================================\n")

# ---- DDNN-specific Gamma helpers (log_mu / log_var parametrisation) ----
gamma_nll_loss_ddnn <- function(params, y) {
  log_mu  <- params[, 1, drop = FALSE]
  log_var <- torch_clamp(params[, 2, drop = FALSE], min = -3, max = 4)
  mu    <- torch_exp(log_mu)
  v     <- torch_exp(log_var)
  shape <- mu^2 / v
  rate  <- mu   / v
  nll <- -(shape - 1) * torch_log(y) +
    y * rate -
    shape * torch_log(rate) +
    torch_lgamma(shape)
  torch_mean(nll)
}

get_gamma_params_ddnn <- function(net, X_sc) {
  X_sc <- as.matrix(X_sc)
  net$eval()
  Xv <- torch_tensor(X_sc, dtype = torch_float(), device = device)
  with_no_grad({ p <- as.matrix(net(Xv)$cpu()) })
  net$train()
  log_mu  <- p[, 1]
  log_var <- pmin(pmax(p[, 2], -3), 4)
  mu  <- exp(log_mu)
  v   <- exp(log_var)
  list(shape = mu^2 / v, rate = mu / v)
}

# Network: outputs 2 params (log_mu, log_var)
make_ddnn_net <- function(n_in, h1, h2, dropout_rate = 0.0) {
  nn_sequential(
    nn_linear(n_in, h1), nn_softplus(),
    if (dropout_rate > 0) nn_dropout(dropout_rate) else nn_identity(),
    nn_linear(h1,   h2), nn_softplus(),
    nn_linear(h2,    2)
  )
}

# Training with early stopping on val NLL
train_ddnn <- function(X_tr, y_tr_in, X_va, y_va, h1, h2, lr,
                       dropout_rate = 0.0, l1_hidden = 0.0,
                       max_epochs = 200, patience = 20,
                       batch_size = 512, seed = 42) {
  set.seed(seed)
  torch_manual_seed(seed)
  net   <- make_ddnn_net(n_input, h1, h2, dropout_rate)$to(device = device)
  optim <- optim_adam(net$parameters, lr = lr)
  dl    <- make_dl(X_tr, y_tr_in, batch_size)
  
  Xva_t <- torch_tensor(as.matrix(X_va), dtype = torch_float(), device = device)
  yva_t <- torch_tensor(matrix(as.numeric(y_va), ncol = 1), dtype = torch_float(), device = device)
  
  best_val_nll <- Inf; best_state <- NULL; patience_cnt <- 0
  
  net$train()
  for (ep in seq_len(max_epochs)) {
    coro::loop(for (b in dl) {
      x_batch <- b[[1]]
      y_batch <- b[[2]]
      optim$zero_grad()
      params <- net(x_batch)
      nll    <- gamma_nll_loss_ddnn(params, y_batch)   # 使用新loss
      
      l1_pen <- torch_tensor(0.0, dtype = torch_float(), device = device)
      if (l1_hidden > 0) {
        for (nm in names(net$parameters)) {
          if (grepl("^[0-9]+\\.weight$", nm) && !grepl("^[56789]", nm)) {
            l1_pen <- l1_pen + l1_hidden * torch_sum(torch_abs(net$parameters[[nm]]))
          }
        }
      }
      loss <- nll + l1_pen
      loss$backward()
      optim$step()
    })
    
    net$eval()
    with_no_grad({
      va_params <- net(Xva_t)
      va_nll    <- as.numeric(gamma_nll_loss_ddnn(va_params, yva_t))
    })
    net$train()
    
    if (ep %% 50 == 0 || ep == 1) {
      cat(sprintf("    [ep %d] val NLL=%.4f (best=%.4f, patience=%d/%d)\n",
                  ep, va_nll, best_val_nll, patience_cnt, patience))
    }
    if (va_nll < best_val_nll - 1e-6) {
      best_val_nll <- va_nll
      best_state   <- lapply(net$state_dict(), function(p) p$clone())
      patience_cnt <- 0
    } else {
      patience_cnt <- patience_cnt + 1
      if (patience_cnt >= patience) {
        cat(sprintf("    Early stop at epoch %d (best val NLL=%.4f)\n", ep, best_val_nll))
        break
      }
    }
  }
  if (!is.null(best_state)) net$load_state_dict(best_state)
  net$eval()
  net
}

# ---- HP TUNING: DDNN — CRPS-based (req. 2) ----
# Grid: h1 x h2 (two options each, h2 = h1/2) x lr x l1 = 8 configs per run
cat("DDNN: Tuning HPs per run under Gamma NLL (log_mu/log_var), evaluated by CRPS...\n")
ddnn_hp_search <- expand.grid(
  h1        = c(128, 256),
  h2        = c(64,  128),
  lr        = c(5e-4, 1e-3),
  l1_hidden = c(0.0, 1e-4),
  stringsAsFactors = FALSE
)
ddnn_hp_search <- ddnn_hp_search[ddnn_hp_search$h2 == ddnn_hp_search$h1 / 2, ]
cat(sprintf("  DDNN HP grid: %d configs per run\n", nrow(ddnn_hp_search)))

tune_ddnn_run <- function(run_id) {
  crps_run <- sapply(seq_len(nrow(ddnn_hp_search)), function(i) {
    hp <- ddnn_hp_search[i, ]
    tryCatch({
      net <- train_ddnn(X_tr_cv, y_tr_cv, X_va_cv, y_va_cv,
                        h1 = hp$h1, h2 = hp$h2, lr = hp$lr,
                        l1_hidden = hp$l1_hidden,
                        max_epochs = 150, patience = 20,
                        batch_size = 512, seed = 2026 + run_id * 31 + i)
      p  <- get_gamma_params_ddnn(net, X_va_cv)
      qm <- gamma_qgrid_fn(p$shape, p$rate, q_grid)
      cr <- crps_from_qmat(y_va_cv, qm)
      cr
    }, error = function(e) Inf)
  })
  best_i <- which.min(crps_run)
  cat(sprintf("  Run %d best: h1=%d h2=%d lr=%.0e l1=%.0e (CRPS=%.4f)\n",
              run_id,
              ddnn_hp_search$h1[best_i], ddnn_hp_search$h2[best_i],
              ddnn_hp_search$lr[best_i], ddnn_hp_search$l1_hidden[best_i],
              crps_run[best_i]))
  as.list(ddnn_hp_search[best_i, ])
}

N_runs_ddnn   <- 4
ddnn_best_hps <- lapply(seq_len(N_runs_ddnn), tune_ddnn_run)

# Final training: full training set with each run's best HP
ddnn_runs <- vector("list", N_runs_ddnn)
cat(sprintf("\nTraining %d DDNN runs on full train set with tuned HPs...\n", N_runs_ddnn))

for (run in seq_len(N_runs_ddnn)) {
  hp <- ddnn_best_hps[[run]]
  cat(sprintf("\n--- DDNN Run %d/%d: h1=%d h2=%d lr=%.0e l1=%.0e ---\n",
              run, N_runs_ddnn, hp$h1, hp$h2, hp$lr, hp$l1_hidden))
  set.seed(2026 + run * 31)
  ddnn_runs[[run]] <- list(
    net = train_ddnn(Xtr_ens_sc, y_tr,
                     X_va_cv, y_va_cv,
                     h1 = hp$h1, h2 = hp$h2, lr = hp$lr,
                     dropout_rate = 0.0, l1_hidden = hp$l1_hidden,
                     max_epochs = 300, patience = 30,
                     batch_size = 512, seed = 2026 + run * 31),
    best_hp = hp
  )
}

# qEns: horizontal quantile averaging across 4 runs (Gamma quantiles)
cat("\nComputing DDNN qEns (horizontal quantile averaging across 4 runs)...\n")
ddnn_qgrid_runs <- lapply(ddnn_runs, function(r) {
  p <- get_gamma_params_ddnn(r$net, Xte_ens_sc)
  gamma_qgrid_fn(p$shape, p$rate, q_grid)
})
ddnn_qgrid_mat <- Reduce(`+`, ddnn_qgrid_runs) / N_runs_ddnn
ddnn_qgrid_mat <- pmax(ddnn_qgrid_mat, 1)

ddnn_q <- data.frame(
  q05 = ddnn_qgrid_mat[, q05_idx],
  q50 = ddnn_qgrid_mat[, q50_idx],
  q95 = ddnn_qgrid_mat[, q95_idx]
)
ddnn_qgrid <- as.data.frame(ddnn_qgrid_mat)
colnames(ddnn_qgrid) <- paste0("q", sprintf("%02d", round(100 * q_grid)))

# NLL (Gamma parametric, averaged parameters)
ddnn_params_all <- lapply(ddnn_runs, function(r) get_gamma_params_ddnn(r$net, Xte_ens_sc))
ddnn_shape_avg  <- Reduce(`+`, lapply(ddnn_params_all, `[[`, "shape")) / N_runs_ddnn
ddnn_rate_avg   <- Reduce(`+`, lapply(ddnn_params_all, `[[`, "rate"))  / N_runs_ddnn
nll_ddnn        <- -mean(dgamma(y_te, shape = ddnn_shape_avg, rate = ddnn_rate_avg, log = TRUE))

best_ddnn_hp <- lapply(ddnn_runs, `[[`, "best_hp")
cat(sprintf("DDNN (Gamma, qEns, %d runs) complete.\n\n", N_runs_ddnn))

# ============================================================
# MODEL 9: Ensemble DRF — Stacked Generalisation
# ============================================================
cat("========================================\n")
cat("MODEL 9: Ensemble DRF (Stacking)\n")
cat("========================================\n")

# Stacking design follows Papacharalampous et al. (2025):
# Split training set into Set1 (train base learners) and Set2 (train combiner).
# This avoids predicting on the full training set, keeping weight matrices
# at ~15k × 15k — well within memory limits.
library(quantreg)

set.seed(2026 + 900)
n_tr     <- nrow(Xtr_tree)
idx_set1 <- sample(n_tr, floor(n_tr / 2), replace = FALSE)
idx_set2 <- setdiff(seq_len(n_tr), idx_set1)
cat(sprintf("Stacking split: Set1=%d, Set2=%d\n", length(idx_set1), length(idx_set2)))

X_set1 <- Xtr_tree[idx_set1, , drop = FALSE];  y_set1 <- y_tr[idx_set1]
X_set2 <- Xtr_tree[idx_set2, , drop = FALSE];  y_set2 <- y_tr[idx_set2]

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
  tau_i  <- q_grid[qi]
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
cat(sprintf("  Fitted QR combiners for %d quantile levels.\n",
            sum(!sapply(qr_combiners, is.null))))

cat("Retraining DRF base learners on full training set...\n")
drf_base_full <- lapply(seq_len(K_drf), function(k) {
  mtry_k <- if (k == 1) floor(sqrt(ncol(Xtr_tree))) else floor(ncol(Xtr_tree) / 3)
  cat(sprintf("  Base learner %d: mtry=%d (full train n=%d)\n",
              k, mtry_k, nrow(Xtr_tree)))
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
    edrf_qgrid_mat[, qi] <- pmax(preds, 1)
  } else {
    edrf_qgrid_mat[, qi] <- rowMeans(X_te_comb)
  }
}

for (i in seq_len(nrow(edrf_qgrid_mat))) {
  edrf_qgrid_mat[i, ] <- sort(edrf_qgrid_mat[i, ])
}

edrf_q <- data.frame(
  q05 = edrf_qgrid_mat[, q05_idx],
  q50 = edrf_qgrid_mat[, q50_idx],
  q95 = edrf_qgrid_mat[, q95_idx]
)
edrf_qgrid <- as.data.frame(edrf_qgrid_mat)
colnames(edrf_qgrid) <- paste0("q", sprintf("%02d", round(100 * q_grid)))

cat("Ensemble DRF (stacking) complete.\n\n")


# ============================================================
# MODEL 10: BNN — Bayes by Backprop (Gamma parametric, req. 6)
# Architecture: BBB structure (Blundell et al. 2015).
# Output layer produces 2 Gamma params (log_shape, log_rate).
# HP TUNING: CRPS-based (req. 2)
# ============================================================
cat("========================================\n")
cat("MODEL 10: BNN (Bayes by Backprop, Gamma parametric, req. 6)\n")
cat("========================================\n")

# ---- Scale mixture prior ----
torch_log_scale_mixture_prior <- function(w, pi_mix, log_sigma1, log_sigma2) {
  half_log2pi <- 0.5 * log(2 * pi)
  log_p1 <- log(pi_mix)       - half_log2pi - log_sigma1 -
    0.5 * (w / exp(log_sigma1))^2
  log_p2 <- log(1 - pi_mix)   - half_log2pi - log_sigma2 -
    0.5 * (w / exp(log_sigma2))^2
  m      <- torch_maximum(log_p1, log_p2)
  torch_sum(m + torch_log(torch_exp(log_p1 - m) + torch_exp(log_p2 - m)))
}

# ---- Bayesian linear layer ----
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

# ---- BNN net: outputs 2 Gamma params (log_shape, log_rate) ----
bnn_gamma_net <- nn_module(
  classname  = "bnn_gamma_bbb",
  initialize = function(n_in, h1, h2, pi_mix, log_sigma1, log_sigma2) {
    self$l1 <- bnn_linear(n_in, h1, pi_mix, log_sigma1, log_sigma2)
    self$l2 <- bnn_linear(h1,   h2, pi_mix, log_sigma1, log_sigma2)
    self$l3 <- bnn_linear(h2,   2,  pi_mix, log_sigma1, log_sigma2)
  },
  forward = function(x) {
    x <- nnf_relu(self$l1(x))
    x <- nnf_relu(self$l2(x))
    self$l3(x)
  },
  kl = function() { self$l1$kl_term() + self$l2$kl_term() + self$l3$kl_term() }
)

# ---- Training with KL re-weighting (Eq. 9) + Gamma NLL ----
train_bnn_gamma <- function(X_sc, y_obs, h1, h2, lr,
                            pi_mix, log_sigma1, log_sigma2,
                            epochs = 100, batch_size = 128, seed = 42) {
  set.seed(seed); torch_manual_seed(seed)
  N   <- nrow(X_sc)
  net <- bnn_gamma_net(n_input, h1, h2, pi_mix, log_sigma1, log_sigma2)$to(device = device)
  opt <- optim_adam(net$parameters, lr = lr)
  dl  <- make_dl(X_sc, y_obs, batch_size)
  
  n_batches  <- ceiling(N / batch_size)
  kl_weights <- sapply(seq_len(n_batches), function(i) {
    (2^(n_batches - i)) / (2^n_batches - 1)
  })
  
  net$train()
  for (ep in seq_len(epochs)) {
    batch_cnt <- 0L
    coro::loop(for (b in dl) {
      batch_cnt <- batch_cnt + 1L
      pi_i      <- kl_weights[min(batch_cnt, n_batches)]
      x_batch   <- b[[1]]
      y_batch   <- b[[2]]
      opt$zero_grad()
      params     <- net(x_batch)
      nll_loss   <- gamma_nll_loss(params, y_batch)
      kl_contrib <- net$kl()
      loss       <- pi_i * kl_contrib + nll_loss
      loss$backward()
      opt$step()
    })
    if (ep %% 25 == 0)
      cat(sprintf("    Epoch %d/%d\n", ep, epochs))
  }
  net$eval()
  net
}

# Thompson sampling: average Gamma params over M stochastic forward passes
bnn_gamma_quantiles <- function(net, X_sc, M_pass = 100) {
  X_sc <- as.matrix(X_sc)
  net$train()  # keep stochasticity ON
  X_t   <- torch_tensor(X_sc, dtype = torch_float(), device = device)
  acc_p <- matrix(0, nrow(X_sc), 2)
  for (p_i in seq_len(M_pass)) {
    with_no_grad({ pm <- as.matrix(net(X_t)$cpu()) })
    acc_p <- acc_p + pm
  }
  avg_p <- acc_p / M_pass
  shape <- exp(avg_p[, 1])
  rate  <- exp(avg_p[, 2])
  gamma_qgrid_fn(shape, rate, q_grid)
}

# ---- HP TUNING: BNN — CRPS-based (req. 2) ----
cat("Tuning BNN hyperparameters (12 configs; CRPS-based, req. 2)...\n")
bnn_hp_grid <- expand.grid(
  h1          = c(64, 128),
  h2          = c(32, 64),
  lr          = c(5e-4),
  pi_mix      = c(0.75),
  neg_log_s1  = c(0),
  neg_log_s2  = c(4, 6, 7),
  stringsAsFactors = FALSE
)

bnn_cv_crps <- sapply(seq_len(nrow(bnn_hp_grid)), function(i) {
  hp <- bnn_hp_grid[i, ]
  cat(sprintf("  [%d/%d] h1=%d h2=%d lr=%.4g pi=%.2f -log(s2)=%g ...",
              i, nrow(bnn_hp_grid),
              hp$h1, hp$h2, hp$lr, hp$pi_mix, hp$neg_log_s2))
  tryCatch({
    net <- train_bnn_gamma(
      X_tr_cv, y_tr_cv,
      h1 = hp$h1, h2 = hp$h2, lr = hp$lr,
      pi_mix      = hp$pi_mix,
      log_sigma1  = -hp$neg_log_s1,
      log_sigma2  = -hp$neg_log_s2,
      epochs = 50, batch_size = 128, seed = 2026 + i
    )
    qm  <- bnn_gamma_quantiles(net, X_va_cv, M_pass = 20)
    cr  <- crps_from_qmat(y_va_cv, qm)
    cat(sprintf(" CRPS=%.4f\n", cr)); cr
  }, error = function(e) { cat(sprintf(" ERROR: %s\n", conditionMessage(e))); Inf })
})

best_bnn_i  <- which.min(bnn_cv_crps)
best_bnn_hp <- as.list(bnn_hp_grid[best_bnn_i, ])
cat(sprintf("\n✓ Best BNN: h1=%d h2=%d lr=%.4g pi=%.2f -log(s2)=%g (CRPS=%.4f)\n",
            best_bnn_hp$h1, best_bnn_hp$h2, best_bnn_hp$lr,
            best_bnn_hp$pi_mix, best_bnn_hp$neg_log_s2,
            bnn_cv_crps[best_bnn_i]))

# ---- Train final BNN ----
set.seed(2026 + 77)
cat("Training final BNN (150 epochs, full train set)...\n")
bnn_net <- train_bnn_gamma(
  Xtr_ens_sc, y_tr,
  h1          = best_bnn_hp$h1,
  h2          = best_bnn_hp$h2,
  lr          = best_bnn_hp$lr,
  pi_mix      = best_bnn_hp$pi_mix,
  log_sigma1  = -best_bnn_hp$neg_log_s1,
  log_sigma2  = -best_bnn_hp$neg_log_s2,
  epochs = 150, batch_size = 128, seed = 2026 + 77
)

cat("BNN inference (M=100 stochastic forward passes)...\n")
bnn_qgrid_mat  <- bnn_gamma_quantiles(bnn_net, Xte_ens_sc, M_pass = 100)
bnn_qgrid_mat  <- pmax(bnn_qgrid_mat, 1)
bnn_q          <- data.frame(q05 = bnn_qgrid_mat[, q05_idx],
                             q50 = bnn_qgrid_mat[, q50_idx],
                             q95 = bnn_qgrid_mat[, q95_idx])
bnn_qgrid      <- as.data.frame(bnn_qgrid_mat)
colnames(bnn_qgrid) <- paste0("q", sprintf("%02d", round(100 * q_grid)))
cat("BNN (Gamma) complete.\n\n")


# ============================================================
# MODEL 11: Hybrid NN + DRF (unchanged)
# ============================================================
cat("========================================\n")
cat("MODEL 11: Hybrid NN + DRF\n")
cat("========================================\n")

cat("Stage 1: Training neural feature extractor (QRNN, 150 epochs)...\n")
set.seed(2026)

y_mean_ens  <- mean(y_tr)
y_sd_ens    <- sd(y_tr)
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

opt_hyb <- optim_adam(
  c(nn_feat_extractor$parameters, nn_out_head$parameters),
  lr = 5e-5
)

dl_hyb <- make_dl(Xtr_ens_sc, y_tr_sc, batch_size = 512)

nn_feat_extractor$train()
nn_out_head$train()

for (epoch in seq_len(150)) {
  coro::loop(for (b in dl_hyb) {
    x_batch <- b[[1]]
    y_batch <- b[[2]]
    opt_hyb$zero_grad()
    feats <- nn_feat_extractor(x_batch)
    preds <- nn_out_head(feats)
    loss  <- pinball_loss(preds, y_batch, tau_t)
    loss$backward()
    opt_hyb$step()
  })
  if (epoch %% 50 == 0) {
    cat(sprintf("    Epoch %d, Loss: %.4f\n", epoch, as.numeric(loss$item())))
  }
}
cat("  Feature extractor training complete.\n\n")

cat("Stage 2: Extracting 32-dim neural features...\n")
nn_feat_extractor$eval()

X_tr_t <- torch_tensor(as.matrix(Xtr_ens_sc), dtype = torch_float(), device = device)
X_te_t <- torch_tensor(as.matrix(Xte_ens_sc), dtype = torch_float(), device = device)

with_no_grad({
  neural_tr <- as.matrix(nn_feat_extractor(X_tr_t)$cpu())
  neural_te <- as.matrix(nn_feat_extractor(X_te_t)$cpu())
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
  num.trees       = num_trees_drf,
  min.node.size   = min_node_size_drf,
  mtry            = floor(sqrt(ncol(df_neural_tr))),
  sample.fraction = 0.5,
  seed            = 2026
)

cat("Stage 4: Computing Hybrid predictions (wquant_matrix)...\n")
hybrid_pred      <- predict(hybrid_drf, newdata = df_neural_te)
hybrid_qgrid_mat <- wquant_matrix(as.numeric(hybrid_pred$y), hybrid_pred$weights, q_grid)
lower_hyb        <- min(y_tr)
hybrid_qgrid_mat <- pmax(hybrid_qgrid_mat, lower_hyb)

hybrid_q <- data.frame(
  q05 = hybrid_qgrid_mat[, q05_idx],
  q50 = hybrid_qgrid_mat[, q50_idx],
  q95 = hybrid_qgrid_mat[, q95_idx]
)

hybrid_qgrid <- as.data.frame(hybrid_qgrid_mat)
colnames(hybrid_qgrid) <- paste0("q", sprintf("%02d", round(100 * q_grid)))
cat("Hybrid NN+DRF complete.\n\n")


# ============================================================
# SAVE BEST HYPERPARAMETERS
# ============================================================
best_hyperparameters <- list(
  DRF          = list(num.trees = num_trees_drf,    min.node.size = min_node_size_drf),
  QRF          = list(ntree = ntree_qrf,            nodesize = nodesize_qrf),
  Ranger       = list(num.trees = num_trees_ranger, min.node.size = min_node_size_ranger),
  XGBoost      = list(max_depth = max_depth_xgb,    eta = eta_xgb, nrounds = nrounds_xgb),
  ENS_Gamma    = best_ens_hp,
  MCD_Gamma    = best_mcd_hp,
  DDNN_Gamma   = best_ddnn_hp,
  Ensemble_DRF = list(K = K_drf, num.trees = num_trees_drf,
                      min.node.size = min_node_size_drf, combiner = "linear_QR"),
  BNN_Gamma    = best_bnn_hp
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

# CRPS (primary metric): per-observation, then averaged (req. 7)
crps_score_full <- function(qmat, y, na.rm = TRUE) {
  crps_from_quantiles(y, as.matrix(qmat), q_grid)
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

# MODEL_COLOR palette (req. 4: remove density distribution plot but keep colors for other plots)
MODEL_COLOR <- c(
  Actual        = "gray20",
  DRF           = "#D65C5C", QRF         = "#5A9BD4", RF            = "#66BB66",
  XGB           = "#E8A838", ENS_Gamma   = "#9B59B6", MCD_Gamma     = "#E67E22",
  DDNN_Gamma    = "#1ABC9C", Ensemble_DRF = "#C0392B", BNN_Gamma    = "#2471A3",
  Hybrid_NN_DRF = "#6E2F1A"
)

# Coverage & width
cov_width <- do.call(rbind, lapply(model_names, function(m) {
  r <- eval_interval(all_q[[m]], y_te)
  data.frame(Model = m, Coverage = r$coverage, Width = r$width)
}))

cat("Coverage & Width:\n")
cov_width[, c("Coverage", "Width")] <- round(cov_width[, c("Coverage", "Width")], 4)
print(cov_width)

# ============================================================
# PRIMARY METRIC: CRPS (per-observation then averaged, req. 7)
# ============================================================
crps_vec <- sapply(model_names, function(m)
  crps_score_full(all_qgrid[[m]], y_te))
cat("\nCRPS (lower = better; per-obs then averaged):\n"); print(round(crps_vec, 4))


# MAE & RMSE (based on median prediction q50)
mae_vec <- sapply(model_names, function(m) {
  q50 <- all_q[[m]]$q50
  mean(abs(y_te - q50), na.rm = TRUE)
})
rmse_vec <- sapply(model_names, function(m) {
  q50 <- all_q[[m]]$q50
  sqrt(mean((y_te - q50)^2, na.rm = TRUE))
})
cat("\nMAE (lower = better):\n");  print(round(mae_vec,  4))
cat("\nRMSE (lower = better):\n"); print(round(rmse_vec, 4))

# Summary table (ranked by CRPS — primary metric)
summary_all <- data.frame(
  Model    = model_names,
  Coverage = cov_width$Coverage,
  Width    = cov_width$Width,
  MAE      = mae_vec,
  RMSE     = rmse_vec,
  CRPS     = crps_vec
  )
summary_all$CRPS_Rank <- rank(summary_all$CRPS, na.last = "keep")
cat("\n=== SUMMARY TABLE (ranked by CRPS) ===\n")
tmp <- summary_all[order(summary_all$CRPS_Rank), ]
numeric_cols_tmp <- sapply(tmp, is.numeric)
tmp[, numeric_cols_tmp] <- round(tmp[, numeric_cols_tmp], 4)
print(tmp)


# ---- eval_df: wide format for subgroup analysis ----
eval_df <- do.call(data.frame, lapply(names(all_q), function(m) {
  mn <- tolower(gsub("[^a-zA-Z0-9]","_",m))
  q  <- all_q[[m]]
  setNames(q, paste0(mn, c("_q05","_q50","_q95")))
}))
eval_df$y <- y_te

eval_df$los_group  <- cut(y_te, c(-Inf,3,7,Inf), labels=c("Short(≤3d)","Medium(3-7d)","Long(>7d)"))

# ---- CRPS helper for subgroup (19 quantiles, per-obs averaged) ----
make_idx_list <- function(group_var) {
  grp <- eval_df[[group_var]]
  lvls <- levels(grp)
  idx_list <- lapply(lvls, function(lv) which(grp == lv))
  names(idx_list) <- lvls
  list(idx_list = idx_list, labels = lvls)
}

sg_models <- model_names

# Subgroup CRPS table (replaces previous WIS-based subgroup tables, req. 4)
subgroup_crps_table <- function(df, group_var) {
  res      <- make_idx_list(group_var)
  crps_mat <- sapply(sg_models, function(m) {
    qmat <- as.matrix(all_qgrid[[m]])
    sapply(res$idx_list, function(idx) {
      crps_from_quantiles(y_te[idx], qmat[idx, , drop = FALSE], q_grid)
    })
  })
  df_out <- as.data.frame(crps_mat)
  cbind(Group = res$labels, df_out)
}


# ============================================================
# HETEROGENEITY ANALYSIS (subgroup CRPS)
# Subgroups selected based on clinical prior knowledge:
# interventions (vasopressin, vasopressors, ventilation),
# diagnoses (sepsis, stroke), care unit, and age group.
# LOS severity removed (outcome-based subgrouping = circular).
# Missingness subgroups removed (data quality, not clinical heterogeneity).
# ============================================================
cat("\n========================================\n")
cat("HETEROGENEITY ANALYSIS (CRPS-based subgroups)\n")
cat("========================================\n\n")

# ---- Subgroup 1: Vasopressin use ----
if ("vasopressin" %in% names(Xte_tree)) {
  eval_df$vasopressin_group <- factor(
    Xte_tree$vasopressin, 0:1, c("No Vasopressin", "Vasopressin")
  )
  out_vasopressin_crps <- subgroup_crps_table(eval_df, "vasopressin_group")
  cat("\n=== CRPS by Vasopressin Subgroup ===\n"); print(out_vasopressin_crps)
} else {
  out_vasopressin_crps <- NULL
  cat("\n(vasopressin not found in test features)\n")
}

# ---- Subgroup 2: Any Vasopressor use ----
if ("vasopressors" %in% names(Xte_tree)) {
  eval_df$vaso_group <- factor(Xte_tree$vasopressors, 0:1, c("No Vasopressors", "Vasopressors"))
  out_vaso_crps <- subgroup_crps_table(eval_df, "vaso_group")
  cat("\n=== CRPS by Any Vasopressor Subgroup ===\n"); print(out_vaso_crps)
} else {
  out_vaso_crps <- NULL
  cat("\n(vasopressors not found in test features)\n")
}

# ---- Subgroup 3: Mechanical Ventilation ----
if ("vent_any" %in% names(Xte_tree)) {
  eval_df$vent_group <- factor(Xte_tree$vent_any, 0:1, c("No Ventilation", "Ventilated"))
  out_vent_crps <- subgroup_crps_table(eval_df, "vent_group")
  cat("\n=== CRPS by Mechanical Ventilation Subgroup ===\n"); print(out_vent_crps)
} else {
  out_vent_crps <- NULL
  cat("\n(vent_any not found in test features)\n")
}

# ---- Subgroup 4: Sepsis ----
if ("sepsis" %in% names(Xte_tree)) {
  eval_df$sepsis_group <- factor(Xte_tree$sepsis, 0:1, c("No Sepsis", "Sepsis"))
  out_sepsis_crps <- subgroup_crps_table(eval_df, "sepsis_group")
  cat("\n=== CRPS by Sepsis Subgroup ===\n"); print(out_sepsis_crps)
} else {
  out_sepsis_crps <- NULL
  cat("\n(sepsis not found in test features)\n")
}

# ---- Subgroup 5: Primary Dx Stroke/Cerebrovascular ----
if ("primary_stroke_cerebro" %in% names(Xte_tree)) {
  eval_df$stroke_group <- factor(
    Xte_tree$primary_stroke_cerebro, 0:1, c("No Stroke/Cerebro", "Stroke/Cerebro")
  )
  out_stroke_crps <- subgroup_crps_table(eval_df, "stroke_group")
  cat("\n=== CRPS by Stroke/Cerebrovascular Subgroup ===\n"); print(out_stroke_crps)
} else {
  out_stroke_crps <- NULL
  cat("\n(primary_stroke_cerebro not found in test features)\n")
}

# ---- Subgroup 6: ICU Care Unit ----
if ("first_careunit" %in% names(Xte_tree)) {
  unit_vals_te <- trimws(as.character(Xte_tree$first_careunit))
  unit_counts  <- sort(table(unit_vals_te), decreasing = TRUE)
  units_keep   <- names(unit_counts[unit_counts >= 200])
  cat(sprintf("\nCare units with n >= 200 in test set (%d units):\n", length(units_keep)))
  print(unit_counts[units_keep])
  eval_df$unit_group_raw <- ifelse(
    unit_vals_te %in% units_keep, unit_vals_te, NA_character_
  )
  eval_df$unit_group_raw <- factor(eval_df$unit_group_raw, levels = units_keep)
  out_unit_crps <- subgroup_crps_table(
    eval_df %>% filter(!is.na(unit_group_raw)), "unit_group_raw"
  )
  cat("\n=== CRPS by ICU Care Unit (n>=200 only) ===\n"); print(out_unit_crps)
} else {
  out_unit_crps <- NULL
  cat("\n(first_careunit not found in test features)\n")
}

# ---- Subgroup 7: Age Group ----
# Clinically motivated cut-points: <65 (non-elderly), 65-80 (elderly), >80 (oldest-old)
# Older patients have greater physiological complexity and more variable LOS distributions
if ("anchor_age" %in% names(Xte_tree)) {
  eval_df$age_group <- cut(
    Xte_tree$anchor_age,
    breaks = c(-Inf, 65, 80, Inf),
    labels = c("<65", "65-80", ">80"),
    right  = TRUE
  )
  age_counts <- table(eval_df$age_group)
  cat(sprintf("\nAge group distribution in test set:\n")); print(age_counts)
  out_age_crps <- subgroup_crps_table(eval_df, "age_group")
  cat("\n=== CRPS by Age Group Subgroup ===\n"); print(out_age_crps)
} else {
  out_age_crps <- NULL
  cat("\n(anchor_age not found in test features)\n")
}

# ============================================================
# FAIRNESS ANALYSIS (subgroup CRPS by protected attributes)
# Examines whether UQ performance differs systematically
# across sociodemographic groups (gender, race, insurance).
# Uses the same CRPS subgroup framework as heterogeneity
# analysis but interpreted through a health equity lens.
# ============================================================
cat("\n========================================\n")
cat("FAIRNESS ANALYSIS (CRPS by protected attributes)\n")
cat("========================================\n\n")

# ---- Fairness 1: Gender ----
if ("gender" %in% names(Xte_tree)) {
  gender_vals <- trimws(as.character(Xte_tree$gender))
  gender_vals[gender_vals == "" | gender_vals == "Unknown"] <- NA_character_
  valid_genders <- names(table(gender_vals)[table(gender_vals) >= 50])
  eval_df$gender_group <- factor(
    ifelse(gender_vals %in% valid_genders, gender_vals, NA_character_),
    levels = valid_genders
  )
  cat(sprintf("\nGender distribution in test set:\n"))
  print(table(eval_df$gender_group, useNA = "ifany"))
  out_gender_crps <- subgroup_crps_table(
    eval_df %>% filter(!is.na(gender_group)), "gender_group"
  )
  cat("\n=== CRPS by Gender ===\n"); print(out_gender_crps)
} else {
  out_gender_crps <- NULL
  cat("\n(gender not found in test features)\n")
}

# ---- Fairness 2: Race / Ethnicity ----
if ("race_clean" %in% names(Xte_tree)) {
  race_vals <- trimws(as.character(Xte_tree$race_clean))
  race_vals[race_vals == "" | race_vals == "Unknown"] <- NA_character_
  valid_races <- names(table(race_vals)[table(race_vals) >= 50])
  eval_df$race_group <- factor(
    ifelse(race_vals %in% valid_races, race_vals, NA_character_),
    levels = valid_races
  )
  cat(sprintf("\nRace/Ethnicity distribution in test set:\n"))
  print(table(eval_df$race_group, useNA = "ifany"))
  out_race_crps <- subgroup_crps_table(
    eval_df %>% filter(!is.na(race_group)), "race_group"
  )
  cat("\n=== CRPS by Race/Ethnicity ===\n"); print(out_race_crps)
} else {
  out_race_crps <- NULL
  cat("\n(race_clean not found in test features)\n")
}

# ---- Fairness 3: Insurance Type ----
# Insurance type is a proxy for socioeconomic status in the US healthcare context
if ("insurance" %in% names(Xte_tree)) {
  ins_vals <- trimws(as.character(Xte_tree$insurance))
  ins_vals[ins_vals == "" | ins_vals == "Unknown"] <- NA_character_
  valid_ins <- names(table(ins_vals)[table(ins_vals) >= 50])
  eval_df$insurance_group <- factor(
    ifelse(ins_vals %in% valid_ins, ins_vals, NA_character_),
    levels = valid_ins
  )
  cat(sprintf("\nInsurance type distribution in test set:\n"))
  print(table(eval_df$insurance_group, useNA = "ifany"))
  out_insurance_crps <- subgroup_crps_table(
    eval_df %>% filter(!is.na(insurance_group)), "insurance_group"
  )
  cat("\n=== CRPS by Insurance Type ===\n"); print(out_insurance_crps)
} else {
  out_insurance_crps <- NULL
  cat("\n(insurance not found in test features)\n")
}
# ============================================================
# CALIBRATION PLOTS (req. 4: removed density distribution, WIS, KS, KL)
# ============================================================
library(ggplot2); library(scales)


# PIT plot
# ---- 对每个模型计算 PIT 值 ----
pit_df <- bind_rows(lapply(names(all_qgrid), function(m) {
  qmat <- as.matrix(all_qgrid[[m]])
  
  pit_vals <- sapply(seq_len(nrow(qmat)), function(i) {
    q_vals <- qmat[i, ]
    y_i    <- y_te[i]
    
    if (any(is.na(q_vals)) || any(is.infinite(q_vals))) return(NA_real_)
    
    # 保留你原来的尾部截断逻辑
    if (y_i <= q_vals[1]) return(q_grid[1])
    if (y_i >= q_vals[length(q_vals)]) return(q_grid[length(q_vals)])
    
    idx <- findInterval(y_i, q_vals)
    
    if (idx < 1) idx <- 1
    if (idx >= length(q_vals)) idx <- length(q_vals) - 1
    
    p_lo <- q_grid[idx]
    p_hi <- q_grid[idx + 1]
    q_lo <- q_vals[idx]
    q_hi <- q_vals[idx + 1]
    
    if (q_hi == q_lo) return((p_lo + p_hi) / 2)
    
    p_lo + (y_i - q_lo) / (q_hi - q_lo) * (p_hi - p_lo)
  })
  
  data.frame(
    model = m,
    pit   = pit_vals
  )
}))

# ---- transformed PIT ----
eps <- 1e-6

pit_df <- pit_df %>%
  filter(!is.na(pit)) %>%
  mutate(
    pit_clipped = pmin(pmax(pit, eps), 1 - eps),
    x_trans     = qnorm(pit_clipped)
  )

# ---- 统一模型顺序（可选）----
pit_df$model <- factor(pit_df$model, levels = names(all_qgrid))

# ============================================================
# 图1：PIT histogram
# ============================================================
p_pit <- ggplot(pit_df, aes(x = pit)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 20,
    fill = "grey85",
    color = "grey20",
    linewidth = 0.3
  ) +
  geom_hline(
    yintercept = 1,
    linetype = "dashed",
    color = "firebrick",
    linewidth = 0.5
  ) +
  facet_wrap(~model, ncol = 2) +
  scale_x_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.2),
    expand = c(0.01, 0.01)
  ) +
  labs(
    title = "PIT Histograms",
    subtitle = "A well-calibrated model should produce an approximately uniform PIT distribution",
    x = "PIT",
    y = "Density"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "grey30"),
    strip.background = element_rect(fill = "grey95", color = "grey70"),
    strip.text = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.spacing = unit(0.9, "lines"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  )

p_pit

ggsave(
  file.path(out_dir, "pit_histogram.png"),
  plot   = p_pit,
  width  = 14,
  height = ceiling(length(model_names) / 2) * 3.2,
  dpi    = 300
)
cat("✓ Saved: pit_histogram.png\n")

# ============================================================
# 图2：Transformed PIT histogram
# ============================================================
p_trans <- ggplot(pit_df, aes(x = x_trans)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 20,
    fill = "grey85",
    color = "grey20",
    linewidth = 0.3
  ) +
  stat_function(
    fun = dnorm,
    color = "black",
    linewidth = 0.6
  ) +
  facet_wrap(~model, ncol = 2) +
  scale_x_continuous(
    limits = c(-3.5, 3.5),
    breaks = seq(-3, 3, by = 1),
    expand = c(0.01, 0.01)
  ) +
  labs(
    title = "Transformed PIT Histograms",
    subtitle = "If PIT is uniform, the transformed values should approximately follow a standard normal distribution",
    x = "Transformed PIT  (qnorm(PIT))",
    y = "Density"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "grey30"),
    strip.background = element_rect(fill = "grey95", color = "grey70"),
    strip.text = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.spacing = unit(0.9, "lines"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  )

p_trans

ggsave(
  file.path(out_dir, "transformed_pit_histogram.png"),
  plot   = p_trans,
  width  = 14,
  height = ceiling(length(model_names) / 2) * 3.2,
  dpi    = 300
)
cat("✓ Saved: transformed_pit_histogram.png\n")




# ============================================================
# 3. Coverage Calibration Plot（单图，全局对比）
#    原名 "Reliability Diagram" → 改名 "Coverage Calibration Plot"
#
#    MCE 嵌入图例标签（"DRF  [MCE=0.021]"），
#    图例按 MCE 从小到大排序，兼顾全局对比与个体校准误差读取。
# ============================================================
rel_df <- bind_rows(lapply(names(all_qgrid), function(m) {
  qmat <- as.matrix(all_qgrid[[m]])
  emp_cov <- sapply(seq_along(alpha_levels), function(j) {
    mean(y_te <= qmat[, j], na.rm = TRUE)
  })
  data.frame(model = m, nominal = alpha_levels, empirical = emp_cov)
}))

# --- 计算 MCE ---
rel_df <- rel_df %>%
  mutate(model = factor(model, levels = model_names))

mce_df <- rel_df %>%
  group_by(model) %>%
  summarise(
    MCE = mean(abs(empirical - nominal), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(MCE) %>%                                        # 按 MCE 升序
  mutate(
    legend_label = sprintf("%-14s [MCE=%.3f]", as.character(model), MCE)
  )

# 把 legend_label 合并回 rel_df，并以 MCE 升序作为图例顺序
rel_df <- rel_df %>%
  left_join(mce_df, by = "model") %>%
  mutate(
    model_leg = factor(legend_label, levels = mce_df$legend_label)
  )

# MODEL_COLOR 需要映射到新的 legend_label key
leg_colors <- setNames(MODEL_COLOR[as.character(mce_df$model)], mce_df$legend_label)

p_cov_cal <- ggplot(rel_df, aes(x = nominal, y = empirical)) +
  
  # (i) Perfect calibration diagonal
  geom_abline(
    slope = 1, intercept = 0,
    linetype = "dashed", color = "grey40", linewidth = 0.6
  ) +
  
  # (iii) Model lines — 图例标签含 MCE
  geom_line(aes(color = model_leg), linewidth = 0.85) +
  geom_point(aes(color = model_leg), size = 1.8) +
  
  scale_color_manual(
    values = leg_colors,
    name   = "Model  [MCE ↑ = worse]"         # 图例标题提示排序含义
  ) +
  
  scale_x_continuous(
    breaks = seq(0.1, 0.9, by = 0.1),
    labels = scales::percent_format(accuracy = 1),
    expand = c(0.01, 0.01)
  ) +
  scale_y_continuous(
    breaks = seq(0.1, 0.9, by = 0.1),
    labels = scales::percent_format(accuracy = 1),
    expand = c(0.01, 0.01),
    limits = c(0, 1)
  ) +
  
  labs(
    title    = "Coverage Calibration Plot",
    subtitle = paste0(
      "Dashed diagonal = perfect calibration  |  ",
      "MCE = Mean Calibration Error (averaged over all quantile levels)  |  ",
      "Legend ordered best \u2192 worst"
    ),
    x = "Nominal quantile level",
    y = "Empirical coverage"
  ) +
  
  theme_bw(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", size = 13),
    plot.subtitle   = element_text(size = 8.5, color = "grey35"),
    legend.position = "right",
    legend.title    = element_text(size = 9, face = "bold"),
    legend.text     = element_text(size = 8.5, family = "mono"),  # 等宽字体对齐
    legend.key.height = unit(1.1, "lines"),
    panel.grid.minor  = element_blank(),
    panel.grid.major  = element_line(color = "#EEEEEE"),
    axis.title        = element_text(size = 10)
  )

print(p_cov_cal)

ggsave(
  file.path(out_dir, "coverage_calibration_plot.png"),
  plot   = p_cov_cal,
  width  = 11,
  height = 7,
  dpi    = 300
)
cat("✓ Saved: coverage_calibration_plot.png\n")

# ---- Conditional calibration by PI width quintile ----
K <- 10  # 改成5组，对应 HRF 论文的 Q1-Q5

cal_df <- bind_rows(lapply(names(all_q), function(m) {
  eval_df %>%
    transmute(model = m, y,
              q05 = all_q[[m]]$q05,
              q95 = all_q[[m]]$q95)
})) %>%
  mutate(
    width   = q95 - q05,                          # ← 改：用PI宽度替代预测中位数
    covered = (y >= q05) & (y <= q95)
  ) %>%
  group_by(model) %>%
  mutate(bin = ntile(width, K)) %>%               # ← 改：按宽度分五分位
  group_by(model, bin) %>%
  summarise(
    x        = mean(width, na.rm = TRUE),          # ← 改：x轴是该bin的平均宽度
    coverage = mean(covered, na.rm = TRUE),
    n_bin    = n(),
    .groups  = "drop"
  ) %>%
  mutate(
    se    = sqrt(coverage * (1 - coverage) / n_bin),
    lower = pmax(0, coverage - 1.96 * se),
    upper = pmin(1, coverage + 1.96 * se),
    model = factor(model, levels = model_names),
    bin_label = factor(paste0("Q", bin),           # ← 新增：给x轴打Q1-Q5标签
                       levels = paste0("Q", 1:K))
  )

p_cal <- ggplot(cal_df, aes(x = bin_label, y = coverage, group = model)) +
  geom_hline(yintercept = 0.90, linetype = "dashed", color = "red", alpha = 0.6) +
  geom_ribbon(aes(ymin = lower, ymax = upper, group = model),
              fill = "steelblue", alpha = 0.15) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  geom_point(color = "steelblue", size = 1.5, alpha = 0.8) +
  facet_wrap(~model, ncol = 2) +
  scale_y_continuous(labels = percent, limits = c(0.55, 1.0)) +
  labs(
    title    = "Conditional Calibration of 90% Prediction Intervals by Uncertainty Quintile",
    subtitle = "Q1 = narrowest intervals (low uncertainty)  |  Q5 = widest intervals (high uncertainty)",
    x        = "Uncertainty Quintile (PI Width)",   # ← 改
    y        = "Empirical Coverage (%)"
  ) +
  theme_bw(base_size = 11) +
  theme(
    strip.background = element_rect(fill = "#f0f0f0"),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold"),
    plot.subtitle    = element_text(size = 9, color = "grey40")
  )

print(p_cal)
ggsave(file.path(out_dir, "conditional_calibration_uncertainty_quintile.png"),
       plot = p_cal, width = 12, height = 18, dpi = 300)
cat("✓ Saved: conditional_calibration_uncertainty_quintile.png\n")
# ---- 配套表格：各模型在每个Decile的覆盖率 ----
cal_table <- cal_df %>%
  mutate(
    ci_str    = sprintf("%.1f%% [%.1f%%, %.1f%%]",
                        coverage * 100,
                        lower    * 100,
                        upper    * 100)
  ) %>%
  select(model, bin_label, ci_str) %>%
  pivot_wider(names_from = bin_label, values_from = ci_str)

# 打印到console
cat("\n=== Conditional Calibration by Uncertainty Decile ===\n")
cat("Format: Coverage% [95% CI lower%, upper%]\n\n")
print(cal_table, n = Inf, width = Inf)

# 保存为CSV
write.csv(cal_table,
          file.path(out_dir, "conditional_calibration_decile_table.csv"),
          row.names = FALSE)
cat("✓ Saved: conditional_calibration_decile_table.csv\n")







# ============================================================
# SENSITIVITY ANALYSIS: Robustness to Introduced Missingness
# Metric changed to CRPS (req. 1 & 2)
# ============================================================
cat("\n========================================\n")
cat("SENSITIVITY ANALYSIS: Robustness to Missing Data (CRPS-based)\n")
cat("========================================\n\n")

num_cols_te <- names(Xte_tree)[sapply(Xte_tree, is.numeric)]
num_cols_te <- grep("_miss$", num_cols_te, value = TRUE, invert = TRUE)
cat(sprintf("Numeric variables for missingness injection: %d\n", length(num_cols_te)))

train_medians <- sapply(num_cols_te, function(v) {
  if (v %in% names(Xtr_tree)) median(Xtr_tree[[v]], na.rm = TRUE) else NA_real_
})

miss_rates <- c(0.10, 0.20, 0.30)
n_seeds    <- 3
sens_seeds <- c(42L, 123L, 999L)

inject_missing <- function(Xte_orig, miss_rate, cols, seed) {
  set.seed(seed)
  Xte_new <- Xte_orig
  n_rows   <- nrow(Xte_new)
  for (v in cols) {
    if (!v %in% names(Xte_new)) next
    n_mask   <- round(miss_rate * n_rows)
    if (n_mask == 0) next
    idx_mask <- sample(n_rows, n_mask, replace = FALSE)
    Xte_new[[v]][idx_mask] <- NA_real_
  }
  Xte_new
}

impute_with_train_median <- function(Xte_df, medians) {
  Xte_imp <- Xte_df
  for (v in names(medians)) {
    if (!v %in% names(Xte_imp)) next
    if (!is.numeric(Xte_imp[[v]])) next
    na_idx <- is.na(Xte_imp[[v]])
    if (any(na_idx) && !is.na(medians[v]))
      Xte_imp[[v]][na_idx] <- medians[v]
  }
  Xte_imp
}

prepare_nn_features <- function(Xte_imp_all) {
  mm <- model.matrix(~ . - 1, data = Xte_imp_all)
  for (cn in setdiff(common_nn, colnames(mm))) {
    mm <- cbind(mm, matrix(0, nrow(mm), 1, dimnames = list(NULL, cn)))
  }
  mm <- mm[, common_nn, drop = FALSE]
  mm[is.na(mm)] <- 0
  sc <- scale(mm, center = x_mean, scale = x_sd)
  sc[sc >  10] <-  10
  sc[sc < -10] <- -10
  sc
}

# CRPS helper for sensitivity loop (19 quantiles, per-obs averaged)
crps19 <- function(pm) {
  pm <- pmax(t(apply(pm, 1, sort)), 1)
  crps_from_quantiles(y_te, pm, q_grid)
}

# ---- Baseline (0% missing) from already-computed results ----
baseline_crps <- sapply(names(all_qgrid), function(m) {
  crps_score_full(all_qgrid[[m]], y_te)
})

sens_results <- list()
sens_results[["0.00"]] <- data.frame(
  model      = names(baseline_crps),
  miss_rate  = 0.00,
  crps_mean  = as.numeric(baseline_crps),
  crps_sd    = 0.0
)

# ---- Loop over miss rates ----
for (mr in miss_rates) {
  cat(sprintf("\n--- Miss rate = %.0f%% ---\n", mr * 100))
  
  crps_by_seed <- lapply(sens_seeds, function(sd) {
    Xte_tree_m <- inject_missing(Xte_tree, mr, num_cols_te, sd)
    
    num_cols_all <- intersect(num_cols_te, names(Xte_all))
    Xte_all_m    <- inject_missing(Xte_all, mr, num_cols_all, sd)
    Xte_all_imp  <- impute_with_train_median(Xte_all_m, train_medians)
    
    Xte_nn_sc <- prepare_nn_features(Xte_all_imp)
    
    # DRF: native missing handling
    drf_pred_m  <- predict(drf_fit, Xte_tree_m)
    drf_q_m     <- wquant_matrix(as.numeric(drf_pred_m$y), drf_pred_m$weights, q_grid)
    crps_drf    <- crps19(drf_q_m)
    
    # QRF: imputed
    qrf_q_m  <- as.matrix(predict(qrf_fit, Xte_all_imp, what = q_grid))
    crps_qrf <- crps19(qrf_q_m)
    
    # Ranger: imputed
    rf_q_m   <- as.matrix(predict(rf_q_fit, Xte_all_imp,
                                  type = "quantiles", quantiles = q_grid)$predictions)
    crps_rf  <- crps19(rf_q_m)
    
    # XGBoost: imputed + one-hot, predict from each quantile model
    Xte_mm_m <- model.matrix(~ . - 1, data = Xte_all_imp)
    for (cn in setdiff(common_cols, colnames(Xte_mm_m)))
      Xte_mm_m <- cbind(Xte_mm_m, matrix(0, nrow(Xte_mm_m), 1, dimnames=list(NULL,cn)))
    Xte_mm_m <- Xte_mm_m[, common_cols, drop = FALSE]; Xte_mm_m[is.na(Xte_mm_m)] <- 0
    dtest_m  <- xgb.DMatrix(Xte_mm_m)
    xgb_qgrid_m <- matrix(NA_real_, nrow(Xte_mm_m), length(q_grid))
    for (ki in seq_along(q_grid)) {
      xgb_qgrid_m[, ki] <- predict(xgb_quantile_models[[ki]], dtest_m)
    }
    xgb_qgrid_m <- pmax(t(apply(xgb_qgrid_m, 1, sort)), min(y_tr))
    crps_xgb    <- crps19(xgb_qgrid_m)
    
    # ENS: Gamma, imputed + NN features (parameter-space averaging)
    ens_params_m <- lapply(ens_members, function(net) get_gamma_params(net, Xte_nn_sc))
    ens_shape_m  <- Reduce(`+`, lapply(ens_params_m, `[[`, "shape")) / M_ens
    ens_rate_m   <- Reduce(`+`, lapply(ens_params_m, `[[`, "rate"))  / M_ens
    ens_qgrid_m  <- pmax(gamma_qgrid_fn(ens_shape_m, ens_rate_m, q_grid), 1)
    crps_ens     <- crps19(ens_qgrid_m)
    
    # MCD: Gamma, imputed + NN features
    mcd_q_m     <- mcd_gamma_quantiles(mc_dropout_net, Xte_nn_sc, M_pass = 50)
    crps_mcd    <- crps19(mcd_q_m)
    
    # DDNN: Gamma, imputed + NN features
    ddnn_params_m <- lapply(ddnn_runs, function(r) get_gamma_params(r$net, Xte_nn_sc))
    ddnn_shape_m  <- Reduce(`+`, lapply(ddnn_params_m, `[[`, "shape")) / N_runs_ddnn
    ddnn_rate_m   <- Reduce(`+`, lapply(ddnn_params_m, `[[`, "rate"))  / N_runs_ddnn
    ddnn_q_m      <- pmax(gamma_qgrid_fn(ddnn_shape_m, ddnn_rate_m, q_grid), 1)
    crps_ddnn     <- crps19(ddnn_q_m)
    
    # Ensemble DRF
    te_qpreds_m <- lapply(drf_base_full, function(fit) {
      pr <- predict(fit, Xte_tree_m)
      wquant_matrix(as.numeric(pr$y), pr$weights, q_grid)
    })
    edrf_mat_m <- matrix(NA_real_, nrow(Xte_tree_m), length(q_grid))
    for (qi in seq_along(q_grid)) {
      X_comb  <- do.call(cbind, lapply(te_qpreds_m, function(qm) qm[, qi]))
      df_te_c <- as.data.frame(X_comb)
      colnames(df_te_c) <- paste0("bl", seq_len(K_drf))
      if (!is.null(qr_combiners[[qi]])) {
        edrf_mat_m[, qi] <- pmax(predict(qr_combiners[[qi]], newdata = df_te_c), 1)
      } else {
        edrf_mat_m[, qi] <- pmax(rowMeans(X_comb), 1)
      }
    }
    for (i in seq_len(nrow(edrf_mat_m))) edrf_mat_m[i, ] <- sort(edrf_mat_m[i, ])
    crps_edrf <- crps19(edrf_mat_m)
    
    # BNN: Gamma, imputed + NN features
    bnn_q_m   <- bnn_gamma_quantiles(bnn_net, Xte_nn_sc, M_pass = 50)
    crps_bnn  <- crps19(bnn_q_m)
    
    # Hybrid NN+DRF
    nn_feat_extractor$eval()
    X_te_t_m <- torch_tensor(Xte_nn_sc, dtype = torch_float(), device = device)
    with_no_grad({ neural_te_m <- as.matrix(nn_feat_extractor(X_te_t_m)) })
    colnames(neural_te_m) <- paste0("nn_feat_", seq_len(ncol(neural_te_m)))
    hyb_pred_m    <- predict(hybrid_drf, newdata = as.data.frame(neural_te_m))
    hyb_q_m       <- wquant_matrix(as.numeric(hyb_pred_m$y), hyb_pred_m$weights, q_grid)
    hyb_q_m       <- pmax(hyb_q_m, min(y_tr))
    crps_hyb      <- crps19(hyb_q_m)
    
    c(DRF          = crps_drf,
      QRF          = crps_qrf,
      RF           = crps_rf,
      XGB          = crps_xgb,
      ENS_Gamma    = crps_ens,
      MCD_Gamma    = crps_mcd,
      DDNN_Gamma   = crps_ddnn,
      Ensemble_DRF = crps_edrf,
      BNN_Gamma    = crps_bnn,
      Hybrid_NN_DRF = crps_hyb)
  })
  
  crps_mat <- do.call(rbind, crps_by_seed)
  cat(sprintf("  CRPS means:\n"))
  print(round(colMeans(crps_mat), 4))
  
  sens_results[[sprintf("%.2f", mr)]] <- data.frame(
    model     = colnames(crps_mat),
    miss_rate = mr,
    crps_mean = colMeans(crps_mat),
    crps_sd   = apply(crps_mat, 2, sd)
  )
}

# ---- Combine into long data frame ----
sens_df <- bind_rows(sens_results)
rownames(sens_df) <- NULL

cat("\n=== Sensitivity Analysis — CRPS by Miss Rate ===\n")
tmp_sens <- tidyr::pivot_wider(sens_df[,c("model","miss_rate","crps_mean")],
                               names_from = miss_rate, values_from = crps_mean)
num_cols_s <- sapply(tmp_sens, is.numeric)
tmp_sens[, num_cols_s] <- round(tmp_sens[, num_cols_s], 4)
print(tmp_sens)

# ---- CRPS degradation relative to baseline ----
baseline_df_s <- sens_df %>% filter(miss_rate == 0) %>%
  select(model, crps_baseline = crps_mean)
sens_df <- sens_df %>%
  left_join(baseline_df_s, by = "model") %>%
  mutate(crps_delta      = crps_mean - crps_baseline,
         crps_pct_change = 100 * crps_delta / crps_baseline)

cat("\n=== CRPS % change from baseline (lower slope = more robust) ===\n")
tmp_pct <- tidyr::pivot_wider(
  sens_df %>% filter(miss_rate > 0) %>% select(model, miss_rate, crps_pct_change),
  names_from = miss_rate, values_from = crps_pct_change)
num_cols_p <- sapply(tmp_pct, is.numeric)
tmp_pct[, num_cols_p] <- round(tmp_pct[, num_cols_p], 2)
print(tmp_pct)

# ---- Sensitivity plot: CRPS vs miss rate ----
drf_family <- c("DRF", "Ensemble_DRF", "Hybrid_NN_DRF")

sens_plot_df <- sens_df %>%
  mutate(
    model      = factor(model, levels = model_names),
    is_drf_fam = model %in% drf_family,
    line_size  = ifelse(model == "DRF", 1.5, 0.8),
    alpha_val  = ifelse(is_drf_fam, 1.0, 0.6)
  )

p_sens <- ggplot(sens_plot_df,
                 aes(x = miss_rate * 100, y = crps_mean,
                     colour = model, group = model,
                     linewidth = I(line_size), alpha = I(alpha_val))) +
  geom_ribbon(aes(ymin = crps_mean - crps_sd, ymax = crps_mean + crps_sd,
                  fill = model), alpha = 0.08, colour = NA) +
  geom_line() +
  geom_point(size = 2.2) +
  scale_colour_manual(values = MODEL_COLOR) +
  scale_fill_manual(values   = MODEL_COLOR) +
  scale_x_continuous(breaks = c(0, 10, 20, 30),
                     labels = c("0%\n(baseline)", "10%", "20%", "30%")) +
  labs(
    title    = "Sensitivity Analysis: CRPS Degradation under Introduced Missingness",
    subtitle = paste0(
      "Missingness injected uniformly at random across all numeric variables\n",
      "DRF: native missing-value handling  |  All other models: median imputation\n",
      "Shaded band = ±1 SD across 3 random seeds  |  Flatter slope = more robust"),
    x      = "Additional missingness injected into test set (% of values per numeric variable)",
    y      = "CRPS (lower = better)",
    colour = "Model", fill = "Model"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(size = 9, colour = "grey40"),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

print(p_sens)
ggsave(file.path(out_dir, "sensitivity_missing_crps.png"),
       plot = p_sens, width = 12, height = 7, dpi = 300)
cat("✓ Saved: sensitivity_missing_crps.png\n")

# ---- CRPS % change plot ----
p_sens_delta <- ggplot(
  sens_df %>% filter(miss_rate > 0) %>%
    mutate(model = factor(model, levels = model_names)),
  aes(x = miss_rate * 100, y = crps_pct_change, colour = model, group = model)
) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_line(aes(linewidth = I(ifelse(model == "DRF", 1.5, 0.8)))) +
  geom_point(size = 2.2) +
  scale_colour_manual(values = MODEL_COLOR) +
  scale_x_continuous(breaks = c(10, 20, 30), labels = c("10%","20%","30%")) +
  labs(
    title    = "CRPS Degradation (% change from baseline) under Introduced Missingness",
    subtitle = "Dashed line = no change from baseline  |  Lower / flatter = more robust",
    x        = "Additional missingness injected (%)",
    y        = "CRPS % change from baseline",
    colour   = "Model"
  ) +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank())

print(p_sens_delta)
ggsave(file.path(out_dir, "sensitivity_missing_crps_pct.png"),
       plot = p_sens_delta, width = 12, height = 7, dpi = 300)
cat("✓ Saved: sensitivity_missing_crps_pct.png\n")

# Save sensitivity table
write.csv(sens_df, file.path(out_dir, "sensitivity_missing_results.csv"), row.names = FALSE)
cat("✓ Saved: sensitivity_missing_results.csv\n\n")

# ============================================================
# SAVE OUTPUTS
# ============================================================
write.csv(summary_all,        file.path(out_dir, "summary_all_models.csv"),       row.names=FALSE)
write.csv(out_vaso_crps,      file.path(out_dir, "subgroup_vaso_crps.csv"),        row.names=FALSE)
write.csv(out_vent_crps,      file.path(out_dir, "subgroup_vent_crps.csv"),        row.names=FALSE)
write.csv(out_los_crps,       file.path(out_dir, "subgroup_los_crps.csv"),         row.names=FALSE)
if (!is.null(out_vasopressin_crps))
  write.csv(out_vasopressin_crps, file.path(out_dir, "subgroup_vasopressin_crps.csv"), row.names=FALSE)
if (!is.null(out_sepsis_crps))
  write.csv(out_sepsis_crps,      file.path(out_dir, "subgroup_sepsis_crps.csv"),      row.names=FALSE)
if (!is.null(out_stroke_crps))
  write.csv(out_stroke_crps,      file.path(out_dir, "subgroup_stroke_crps.csv"),      row.names=FALSE)
if (!is.null(out_unit_crps))
  write.csv(out_unit_crps,        file.path(out_dir, "subgroup_unit_crps.csv"),        row.names=FALSE)
for (nm in names(out_miss_var_list)) {
  write.csv(out_miss_var_list[[nm]],
            file.path(out_dir, sprintf("subgroup_miss_%s_crps.csv", nm)),
            row.names = FALSE)
}

saveRDS(list(
  summary          = summary_all,
  crps             = crps_vec,
  best_hyperparams = best_hyperparameters,
  # model objects
  drf_fit          = drf_fit,
  qrf_fit          = qrf_fit,
  rf_q_fit         = rf_q_fit,
  xgb_quantile_models = xgb_quantile_models,
  ens_members      = ens_members,
  mc_dropout_net   = mc_dropout_net,
  ddnn_runs        = ddnn_runs,
  edrf_members     = drf_base_full,
  bnn_net          = bnn_net,
  hybrid_drf       = hybrid_drf,
  nn_feat_extractor = nn_feat_extractor
), file.path(out_dir, "all_model_results.rds"))

cat("\n✓ All results saved to", out_dir, "\n")
cat("\n========================================\n")
cat("✓ ✓ ✓  ALL 10 MODELS COMPLETE  ✓ ✓ ✓\n")
cat("========================================\n\n")


# ============================================================
# PART: Package version report (req. 5)
# ============================================================
cat("\n========================================\n")
cat("PACKAGE VERSION REPORT\n")
cat("========================================\n\n")

# Collect all packages used in this script
pkg_list <- c(
  # Data wrangling / EDA
  "dplyr", "readr", "lubridate", "tidyr",
  # Visualisation
  "ggplot2", "patchwork", "scales", "ggridges",
  # ML / modelling
  "caret",
  "drf",
  "quantregForest",
  "ranger",
  "xgboost",
  "torch",
  "scoringutils",
  # DL utilities
  "coro"
)

# R itself
cat(sprintf("R version:  %s\n\n", R.version.string))

# Per-package version
pkg_versions <- lapply(pkg_list, function(pkg) {
  tryCatch({
    ver <- as.character(packageVersion(pkg))
    data.frame(Package = pkg, Version = ver, stringsAsFactors = FALSE)
  }, error = function(e) {
    data.frame(Package = pkg, Version = "NOT INSTALLED", stringsAsFactors = FALSE)
  })
})

pkg_df <- do.call(rbind, pkg_versions)
rownames(pkg_df) <- NULL
print(pkg_df, row.names = FALSE)

cat("\n")
# Also print as sessionInfo() for full reproducibility record
cat("--- Full sessionInfo() ---\n")
print(sessionInfo())