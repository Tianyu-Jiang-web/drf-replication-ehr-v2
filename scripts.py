"""
ICU Length-of-Stay Probabilistic Prediction
Python translation of scripts.R

Models: DRF (Distributional Random Forest — real drf Python package),
        QRF, RF (quantile-forest),
        XGBoost + Conformal,
        Deep Ensemble (QRNN),
        MC Dropout (QRNN),
        Hybrid NN-features + DRF

GPU acceleration:
  - PyTorch neural networks (ENS, MCD, Hybrid) use CUDA/MPS automatically if available
  - XGBoost uses GPU if available (tree_method='hist', device='cuda')
  - Tree models (DRF, QRF, RF) are CPU-only (no GPU support)

Dependencies (install with pip):
  pip install pandas numpy scikit-learn xgboost quantile-forest torch matplotlib seaborn scipy tqdm
  pip install -e "git+https://github.com/lorismichel/drf#egg=pkg&subdirectory=python-package"
  # OR (from test PyPI):
  # pip install -i https://test.pypi.org/simple/ drf==0.1
"""

# %%
# ============================================================
# 0. Imports
# ============================================================
import warnings
warnings.filterwarnings("ignore")

import numpy as np
import pandas as pd
from scipy import interpolate, stats
from scipy.stats import ks_2samp
from sklearn.model_selection import KFold
from sklearn.preprocessing import StandardScaler, LabelEncoder
import xgboost as xgb
import torch
import torch.nn as nn
from torch.utils.data import DataLoader, TensorDataset
import matplotlib
matplotlib.use("Agg")   # non-interactive backend for saving figures
import matplotlib.pyplot as plt
import matplotlib.ticker as mticker
import seaborn as sns
from itertools import product
import os, copy, math
from tqdm import tqdm

# Real DRF Python package (Distributional Random Forest)
# Install via GitHub:
#   pip install -e "git+https://github.com/lorismichel/drf#egg=pkg&subdirectory=python-package"
# OR via test PyPI:
#   pip install -i https://test.pypi.org/simple/ drf==0.1
import sys
print(sys.executable)
# %%
from drf import drf as DRF
# %%
# monkey patch for rpy2 compatibility
if not hasattr(pd.DataFrame, "iteritems"):
    pd.DataFrame.iteritems = pd.DataFrame.items
# QuantileForest (QRF + Ranger-style quantile RF)
# pip install quantile-forest
from quantile_forest import RandomForestQuantileRegressor

SEED = 2026
np.random.seed(SEED)
torch.manual_seed(SEED)

# ============================================================
# 1. Device setup
# ============================================================
device = torch.device(
    "cuda" if torch.cuda.is_available()
    else "mps" if torch.backends.mps.is_available()
    else "cpu"
)
print(f"\nUsing device: {device}")

# ============================================================
# 2. Data loading & preprocessing
# ============================================================
import os
print(os.getcwd())
print("\n=== Loading and preprocessing data ===")
# %%
df = pd.read_csv("data/FINAL_master_dataset_9Feb2026(in).csv")
df = df[df["icu_los_days"].notna()].copy()

# --- Race mapping ---
race_mapping = {

    # White
    "WHITE": "White",
    "WHITE - OTHER EUROPEAN": "White",
    "WHITE - EASTERN EUROPEAN": "White",
    "WHITE - BRAZILIAN": "White",
    "WHITE - RUSSIAN": "White",
    "PORTUGUESE": "White",
    "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER": "White",

    # Black
    "BLACK/AFRICAN AMERICAN": "Black",
    "BLACK/CARIBBEAN ISLAND": "Black",
    "BLACK/AFRICAN": "Black",
    "BLACK/CAPE VERDEAN": "Black",

    # Asian
    "ASIAN": "Asian",
    "ASIAN - SOUTH EAST ASIAN": "Asian",
    "ASIAN - CHINESE": "Asian",
    "ASIAN - ASIAN INDIAN": "Asian",
    "ASIAN - KOREAN": "Asian",

    # Hispanic
    "HISPANIC OR LATINO": "Hispanic/Latino",
    "HISPANIC/LATINO - CUBAN": "Hispanic/Latino",
    "HISPANIC/LATINO - DOMINICAN": "Hispanic/Latino",
    "HISPANIC/LATINO - PUERTO RICAN": "Hispanic/Latino",
    "HISPANIC/LATINO - SALVADORAN": "Hispanic/Latino",
    "SOUTH AMERICAN": "Hispanic/Latino",

    # Other
    "OTHER": "Other",
    "MULTIPLE RACE/ETHNICITY": "Other",
    "AMERICAN INDIAN/ALASKA NATIVE": "Other",

    # Unknown
    "UNKNOWN": "Unknown",
    "PATIENT DECLINED TO ANSWER": "Unknown",
    "UNABLE TO OBTAIN": "Unknown",
}

RACE_LEVELS = ["White", "Black", "Asian", "Hispanic/Latino", "Other", "Unknown"]

df["race"] = df["race"].astype(str).str.strip()
df["race_clean"] = df["race"].map(race_mapping).fillna("Other")
df["race_clean"] = pd.Categorical(df["race_clean"], categories=RACE_LEVELS)

df["y"] = df["icu_los_days"]
# %%
# --- Drop columns ---
drop_cols = [
    "stay_id", "race",
    "admittime", "dischtime", "icu_intime_first", "icu_outtime_first", "intime",
    "primary_icd_long_title", "primary_icd_code",
    "readmit_30d", "readmit_90d", "readmit_180d",
    "icu_los_days",
]
df2 = df.drop(columns=[c for c in drop_cols if c in df.columns]).copy()

# --- Strip _final suffix ---
df2.columns = [c.removesuffix("_final") for c in df2.columns]

# --- Categorical variables ---
cat_cols = [c for c in ["race_clean", "marital_status", "insurance", "gender", "first_careunit"]
            if c in df2.columns]
for c in cat_cols:
    df2[c] = df2[c].astype(str).str.strip().astype("category")

# --- Numeric columns ---
num_cols = [c for c in df2.select_dtypes(include=[np.number]).columns if c != "y"]

# --- Missingness indicators ---
for v in num_cols:
    df2[f"{v}_miss"] = df2[v].isna().astype(int)

# --- Admission datetime for temporal split ---
df_time = df[["subject_id", "hadm_id", "admittime"]].copy()
df_time["admit_dt"] = pd.to_datetime(df_time["admittime"], format="%d/%m/%Y %H:%M", dayfirst=True, errors="coerce")
df_time = df_time[["subject_id", "hadm_id", "admit_dt"]]

df2 = df2.merge(df_time, on=["subject_id", "hadm_id"], how="left")

# --- Drop ID columns and their miss indicators ---
id_drop = ["subject_id", "hadm_id", "hadm_id_first",
           "subject_id_miss", "hadm_id_first_miss", "hadm_id_miss"]
df2 = df2.drop(columns=[c for c in id_drop if c in df2.columns])

# --- Median imputation copy (for QRF / RF / XGB / ENS / MCD) ---
df_imp = df2.copy()
for v in num_cols:
    if v in df_imp.columns:
        med = df_imp[v].median()
        df_imp[v] = df_imp[v].fillna(med)

# For categoricals: fill NA with "Unknown"
for v in cat_cols:
    if v in df_imp.columns:
        df_imp[v] = df_imp[v].cat.add_categories("Unknown") if "Unknown" not in df_imp[v].cat.categories else df_imp[v]
        df_imp[v] = df_imp[v].fillna("Unknown")

# ============================================================
# 3. Train / Test temporal split (80/20 by admission time)
# ============================================================
cutoff = df2["admit_dt"].quantile(0.8)
idx_train = df2.index[df2["admit_dt"] <= cutoff].tolist()
idx_test  = df2.index[df2["admit_dt"]  > cutoff].tolist()

df2_model = df2.drop(columns=["admit_dt"])
df_imp2   = df_imp.drop(columns=["admit_dt"], errors="ignore")

y = df2_model["y"].values
X_tree_df = df2_model.drop(columns=["y"])
X_all_df  = df_imp2.drop(columns=["y"])

y_tr = y[idx_train]
y_te = y[idx_test]

Xtr_tree_df = X_tree_df.loc[idx_train].reset_index(drop=True)
Xte_tree_df = X_tree_df.loc[idx_test].reset_index(drop=True)

Xtr_all_df = X_all_df.loc[idx_train].reset_index(drop=True)
Xte_all_df = X_all_df.loc[idx_test].reset_index(drop=True)

# ============================================================
# 4. Clean tree features: drop all-zero _miss + constant cols
# ============================================================
miss_cols = [c for c in Xtr_tree_df.columns if c.endswith("_miss")]
miss_all_zero = [c for c in miss_cols if Xtr_tree_df[c].sum() == 0]
const_cols = [c for c in Xtr_tree_df.columns if Xtr_tree_df[c].nunique(dropna=True) <= 1]
drop_cols_tree = list(set(miss_all_zero) | set(const_cols))

Xtr_tree_df = Xtr_tree_df.drop(columns=drop_cols_tree)
Xte_tree_df = Xte_tree_df.drop(columns=[c for c in drop_cols_tree if c in Xte_tree_df.columns])

print(f"Tree feature cleanup:")
print(f"  Removed useless *_miss (all-zero in train): {len(miss_all_zero)}")
print(f"  Removed constant columns (in train): {len(const_cols)}")
print(f"  Xtr_tree cols: {Xtr_tree_df.shape[1]} | Xte_tree cols: {Xte_tree_df.shape[1]}")

# ============================================================
# 5. Prepare numeric matrices (one-hot encoding for tree models)
# ============================================================

def encode_df(df_train, df_test):
    """One-hot encode categoricals, align columns."""
    df_train_enc = pd.get_dummies(df_train, dummy_na=False)
    df_test_enc  = pd.get_dummies(df_test,  dummy_na=False)
    # Align: test gets any missing cols as 0
    all_cols = sorted(set(df_train_enc.columns) | set(df_test_enc.columns))
    df_train_enc = df_train_enc.reindex(columns=all_cols, fill_value=0)
    df_test_enc  = df_test_enc.reindex(columns=all_cols, fill_value=0)
    return df_train_enc.astype(float), df_test_enc.astype(float)

Xtr_tree_enc, Xte_tree_enc = encode_df(Xtr_tree_df, Xte_tree_df)
Xtr_all_enc,  Xte_all_enc  = encode_df(Xtr_all_df,  Xte_all_df)

Xtr_tree_mat = Xtr_tree_enc.values
Xte_tree_mat = Xte_tree_enc.values
Xtr_all_mat  = Xtr_all_enc.values
Xte_all_mat  = Xte_all_enc.values

# ============================================================
# 6. Utility functions
# ============================================================

Q_LEVELS = np.array([0.05, 0.5, 0.95])
Q_GRID   = np.arange(0.05, 1.00, 0.05)     # 19 quantiles

def wquant(y_vals, weights, probs):
    """Weighted quantile function."""
    y_vals = np.asarray(y_vals, dtype=float)
    weights = np.asarray(weights, dtype=float)
    order = np.argsort(y_vals)
    y_sorted = y_vals[order]
    w_sorted = weights[order]
    cw = np.cumsum(w_sorted) / w_sorted.sum()
    return np.array([y_sorted[np.searchsorted(cw, p)] for p in probs])


def wis_score(y_obs, pred_mat, q_levels):
    """
    Weighted Interval Score (WIS).
    pred_mat: (n, K) quantile predictions; q_levels: (K,) quantile levels.
    Returns vector of per-observation WIS values.
    """
    y_obs = np.asarray(y_obs)
    pred_mat = np.asarray(pred_mat)
    q_levels = np.asarray(q_levels)
    n, K = pred_mat.shape
    scores = np.zeros(n)
    for k, tau in enumerate(q_levels):
        q = pred_mat[:, k]
        err = y_obs - q
        scores += np.where(err >= 0, tau * err, (tau - 1) * err)
    return scores / K  # average over quantile levels (matches scoringutils behaviour)


def mean_wis(y_obs, pred_mat, q_levels):
    return np.nanmean(wis_score(y_obs, pred_mat, q_levels))


def eval_interval(q_df, y):
    """Coverage and interval width for 90% PI."""
    covered = (y >= q_df["q05"].values) & (y <= q_df["q95"].values)
    return {"coverage": np.nanmean(covered), "width": np.nanmean(q_df["q95"].values - q_df["q05"].values)}


def pinball_loss(y_obs, q_pred, tau):
    """Scalar pinball loss for one quantile level."""
    err = y_obs - q_pred
    return np.where(err >= 0, tau * err, (tau - 1) * err).mean()


def crps_from_quantiles(y, qmat, taus):
    """CRPS approximated via trapezoidal integration of pinball losses."""
    taus = np.asarray(taus)
    qmat = np.asarray(qmat)
    order = np.argsort(taus)
    taus = taus[order]
    qmat = qmat[:, order]

    n_tau = len(taus)
    w = np.zeros(n_tau)
    w[0] = (taus[1] - taus[0]) / 2
    w[-1] = (taus[-1] - taus[-2]) / 2
    for j in range(1, n_tau - 1):
        w[j] = (taus[j + 1] - taus[j - 1]) / 2

    pb = 0.0
    for j in range(n_tau):
        err = y - qmat[:, j]
        pb += w[j] * np.where(err >= 0, taus[j] * err, (taus[j] - 1) * err)
    return np.nanmean(2 * pb)

# %%
def approx_density_from_quantiles(y, qmat, taus):
    """
    Approximate PDF at each observation using finite differences on the CDF
    (linearly interpolated from quantile function).
    """
    taus = np.asarray(taus)
    qmat = np.asarray(qmat)
    n = len(y)
    densities = np.zeros(n)
    for i in range(n):
        q_i = qmat[i]
        y_i = y[i]
        if y_i <= q_i[0]:
            dq = q_i[1] - q_i[0]
            dt = taus[1] - taus[0]
            densities[i] = dt / dq if dq > 1e-10 else 1e-6
        elif y_i >= q_i[-1]:
            dq = q_i[-1] - q_i[-2]
            dt = taus[-1] - taus[-2]
            densities[i] = dt / dq if dq > 1e-10 else 1e-6
        else:
            j = np.searchsorted(q_i, y_i) - 1
            j = min(j, len(q_i) - 2)
            dq = q_i[j + 1] - q_i[j]
            dt = taus[j + 1] - taus[j]
            densities[i] = dt / dq if dq > 1e-10 else 1e-6
    return np.maximum(densities, 1e-10)

# %%
def sample_from_quantiles(qmat, taus, n_samples, rng=None):
    """Inverse CDF sampling from quantile matrix."""
    if rng is None:
        rng = np.random.default_rng(SEED)
    qmat = np.asarray(qmat)
    n_test = qmat.shape[0]
    samples = np.zeros((n_test, n_samples))
    for i in range(n_test):
        u = rng.uniform(0, 1, n_samples)
        f = interpolate.interp1d(taus, qmat[i], kind="linear", bounds_error=False,
                                 fill_value=(qmat[i, 0], qmat[i, -1]))
        samples[i] = f(u)
    return samples

# %%
def kl_divergence_kde(x_true, x_pred, n_grid=200):
    """KL divergence D_KL(P_true || P_pred) via KDE."""
    x_min = min(x_true.min(), x_pred.min())
    x_max = max(x_true.max(), x_pred.max())
    grid = np.linspace(x_min, x_max, n_grid)
    eps = 1e-10
    kde_true = stats.gaussian_kde(x_true)(grid)
    kde_pred = stats.gaussian_kde(x_pred)(grid)
    p = np.maximum(kde_true, eps); p /= p.sum()
    q = np.maximum(kde_pred, eps); q /= q.sum()
    return np.sum(p * np.log(p / q))

# %%
# ============================================================
# 7. 3-fold CV split (for HP tuning, consistent with R code)
# ============================================================
kf = KFold(n_splits=3, shuffle=True, random_state=SEED)
cv_folds = list(kf.split(Xtr_tree_mat))   # list of (train_idx, val_idx)
val_idx_hp = cv_folds[0][1]               # use fold 0 val set for HP search
train_idx_hp = cv_folds[0][0]
# %%
# ============================================================
# 8. MODEL 1 – DRF (Distributional Random Forest)
#    Uses the official drf Python package (lorismichel/drf),
#    which is a direct port of the R drf package.
#    API: drf(min_node_size, num_trees, splitting_rule)
#         .fit(X, Y)  →  .predict(newdata)  →  .weights
# ============================================================
print("\n=== Tuning DRF (Distributional Random Forest) ===")

# DRF expects pandas DataFrames; Y must be a DataFrame (multivariate supported)
Xtr_tree_pd = pd.DataFrame(Xtr_tree_mat, columns=Xtr_tree_enc.columns)
Xte_tree_pd = pd.DataFrame(Xte_tree_mat, columns=Xte_tree_enc.columns)
Ytr_pd = pd.DataFrame({"y": y_tr})

drf_param_grid = list(product([500, 1000], [20, 50, 100]))   # (num_trees, min_node_size)

drf_cv_scores = []
for num_trees, min_node_size in drf_param_grid:
    print(f"  trees={num_trees}, min_node_size={min_node_size} ...", end="", flush=True)

    DRF_cv = DRF(
        min_node_size=min_node_size,
        num_trees=num_trees,
        splitting_rule="FourierMMD",   # same default as R package
    )
    DRF_cv.fit(
        Xtr_tree_pd.iloc[train_idx_hp].reset_index(drop=True),
        pd.DataFrame({"y": y_tr[train_idx_hp]})
    )

    out_cv = DRF_cv.predict(newdata=Xtr_tree_pd.iloc[val_idx_hp].reset_index(drop=True))
    W_cv   = out_cv.weights                    # (n_val, n_train)
    y_train_cv = y_tr[train_idx_hp]

    n_val = len(val_idx_hp)
    preds = np.zeros((n_val, 3))
    for i in range(n_val):
        preds[i] = wquant(y_train_cv, W_cv[i], Q_LEVELS)

    wis = mean_wis(y_tr[val_idx_hp], preds, Q_LEVELS)
    drf_cv_scores.append(wis)
    print(f" WIS={wis:.4f}")
# %%
best_drf_idx = int(np.argmin(drf_cv_scores))
best_drf_trees, best_drf_node = drf_param_grid[best_drf_idx]
print(f"✓ Best DRF: num_trees={best_drf_trees}, min_node_size={best_drf_node} "
      f"(WIS={drf_cv_scores[best_drf_idx]:.4f})")

# Fit final DRF on full training set
drf_fit = DRF(
    min_node_size=best_drf_node,
    num_trees=best_drf_trees,
    splitting_rule="FourierMMD",
)
drf_fit.fit(Xtr_tree_pd, Ytr_pd)

# Predict on test set → extract weights → weighted quantiles
print("Computing DRF predictions ...")
drf_pred = drf_fit.predict(newdata=Xte_tree_pd)
W        = drf_pred.weights           # (n_te, n_tr) – dense numpy array
y_train  = y_tr                       # aligned with W columns

n_te = len(y_te)
drf_q_mat     = np.zeros((n_te, 3))
drf_qgrid_mat = np.zeros((n_te, len(Q_GRID)))

for i in tqdm(range(n_te), ncols=60):
    drf_q_mat[i]     = wquant(y_train, W[i], Q_LEVELS)
    drf_qgrid_mat[i] = wquant(y_train, W[i], Q_GRID)

drf_q     = pd.DataFrame(drf_q_mat,     columns=["q05", "q50", "q95"])
drf_qgrid = pd.DataFrame(drf_qgrid_mat, columns=[f"q{int(round(100*t)):02d}" for t in Q_GRID])
# %%
# ============================================================
# 9. MODEL 2 – QRF (Quantile Random Forest via quantile-forest)
# ============================================================
print("\n=== Tuning QRF ===")

qrf_param_grid = list(product([500, 1000], [20, 50, 100]))  # (n_estimators, min_samples_leaf)
qrf_cv_scores = []
for n_trees, min_leaf in qrf_param_grid:
    print(f"  ntree={n_trees}, nodesize={min_leaf} ...", end="", flush=True)
    qrf = RandomForestQuantileRegressor(
        n_estimators=n_trees, min_samples_leaf=min_leaf,
        max_features="sqrt", random_state=SEED, n_jobs=-1
    )
    qrf.fit(Xtr_all_mat[train_idx_hp], y_tr[train_idx_hp])
    preds = qrf.predict(Xtr_all_mat[val_idx_hp], quantiles=Q_LEVELS.tolist())
    wis = mean_wis(y_tr[val_idx_hp], preds, Q_LEVELS)
    qrf_cv_scores.append(wis)
    print(f" WIS={wis:.4f}")

best_qrf_idx = int(np.argmin(qrf_cv_scores))
best_qrf_trees, best_qrf_leaf = qrf_param_grid[best_qrf_idx]
print(f"✓ Best QRF: ntree={best_qrf_trees}, nodesize={best_qrf_leaf} (WIS={qrf_cv_scores[best_qrf_idx]:.4f})")

qrf_fit = RandomForestQuantileRegressor(
    n_estimators=best_qrf_trees, min_samples_leaf=best_qrf_leaf,
    max_features="sqrt", random_state=SEED, n_jobs=-1
)
qrf_fit.fit(Xtr_all_mat, y_tr)

qrf_q_mat      = qrf_fit.predict(Xte_all_mat, quantiles=Q_LEVELS.tolist())
qrf_qgrid_mat  = qrf_fit.predict(Xte_all_mat, quantiles=Q_GRID.tolist())

qrf_q     = pd.DataFrame(qrf_q_mat,     columns=["q05", "q50", "q95"])
qrf_qgrid = pd.DataFrame(qrf_qgrid_mat, columns=[f"q{int(round(100*t)):02d}" for t in Q_GRID])
# %%
# ============================================================
# 10. MODEL 3 – RF Quantile (Ranger → same library)
# ============================================================
print("\n=== Tuning Quantile RF (Ranger-equivalent) ===")

ranger_param_grid = list(product([500, 1000], [10, 20, 50]))
ranger_cv_scores = []
for n_trees, min_leaf in ranger_param_grid:
    print(f"  trees={n_trees}, min_leaf={min_leaf} ...", end="", flush=True)
    rf_q = RandomForestQuantileRegressor(
        n_estimators=n_trees, min_samples_leaf=min_leaf,
        max_features="sqrt", random_state=SEED, n_jobs=-1
    )
    rf_q.fit(Xtr_all_mat[train_idx_hp], y_tr[train_idx_hp])
    preds = rf_q.predict(Xtr_all_mat[val_idx_hp], quantiles=Q_LEVELS.tolist())
    wis = mean_wis(y_tr[val_idx_hp], preds, Q_LEVELS)
    ranger_cv_scores.append(wis)
    print(f" WIS={wis:.4f}")

best_ranger_idx = int(np.argmin(ranger_cv_scores))
best_ranger_trees, best_ranger_leaf = ranger_param_grid[best_ranger_idx]
print(f"✓ Best RF: n_trees={best_ranger_trees}, min_leaf={best_ranger_leaf} (WIS={ranger_cv_scores[best_ranger_idx]:.4f})")

rf_q_fit = RandomForestQuantileRegressor(
    n_estimators=best_ranger_trees, min_samples_leaf=best_ranger_leaf,
    max_features="sqrt", random_state=SEED, n_jobs=-1
)
rf_q_fit.fit(Xtr_all_mat, y_tr)

rf_q_mat      = rf_q_fit.predict(Xte_all_mat, quantiles=Q_LEVELS.tolist())
rf_qgrid_mat  = rf_q_fit.predict(Xte_all_mat, quantiles=Q_GRID.tolist())

rf_q     = pd.DataFrame(rf_q_mat,     columns=["q05", "q50", "q95"])
rf_qgrid = pd.DataFrame(rf_qgrid_mat, columns=[f"q{int(round(100*t)):02d}" for t in Q_GRID])
# %%
# ============================================================
# 11. MODEL 4 – XGBoost + Conformal Prediction
# ============================================================
print("\n=== Tuning XGBoost + Conformal ===")

alpha = 0.10

xgb_param_grid = list(product([4, 6, 8], [0.03, 0.05, 0.1], [200, 400]))
xgb_cv_scores = []

# Use GPU for XGBoost if available
tree_method = "hist"
xgb_device  = "cuda" if torch.cuda.is_available() else "cpu"

for max_depth, eta, nrounds in xgb_param_grid:
    print(f"  depth={max_depth}, eta={eta:.3f}, nrounds={nrounds} ...", end="", flush=True)

    # Inner conformal split
    rng_xgb = np.random.default_rng(SEED)
    cal_size = int(0.2 * len(train_idx_hp))
    cal_local = rng_xgb.choice(train_idx_hp, size=cal_size, replace=False)
    train2    = np.setdiff1d(train_idx_hp, cal_local)

    dtrain_cv = xgb.DMatrix(Xtr_all_mat[train2],    label=y_tr[train2])
    dcal_cv   = xgb.DMatrix(Xtr_all_mat[cal_local], label=y_tr[cal_local])
    dval_cv   = xgb.DMatrix(Xtr_all_mat[val_idx_hp])

    params_xgb = {
        "objective": "reg:squarederror",
        "max_depth": max_depth, "eta": eta,
        "subsample": 0.8, "colsample_bytree": 0.8,
        "tree_method": tree_method, "device": xgb_device,
        "verbosity": 0,
    }
    bst = xgb.train(params_xgb, dtrain_cv, num_boost_round=nrounds, verbose_eval=False)

    pred_cal = bst.predict(dcal_cv)
    resid = np.abs(y_tr[cal_local] - pred_cal)
    q_hat = np.quantile(resid, 1 - alpha)

    pred_val = bst.predict(dval_cv)
    lower_b  = y_tr[train2].min()
    preds = np.column_stack([
        np.maximum(pred_val - q_hat, lower_b),
        pred_val,
        pred_val + q_hat
    ])
    wis = mean_wis(y_tr[val_idx_hp], preds, Q_LEVELS)
    xgb_cv_scores.append(wis)
    print(f" WIS={wis:.4f}")

best_xgb_idx = int(np.argmin(xgb_cv_scores))
best_xgb_depth, best_xgb_eta, best_xgb_rounds = xgb_param_grid[best_xgb_idx]
print(f"✓ Best XGB: depth={best_xgb_depth}, eta={best_xgb_eta}, nrounds={best_xgb_rounds} (WIS={xgb_cv_scores[best_xgb_idx]:.4f})")

# Conformal calibration split on full train
rng_xgb = np.random.default_rng(SEED)
n_tr = len(y_tr)
cal_idx  = rng_xgb.choice(n_tr, size=int(0.2 * n_tr), replace=False)
train_idx_final = np.setdiff1d(np.arange(n_tr), cal_idx)

y_cal = y_tr[cal_idx]

dtrain = xgb.DMatrix(Xtr_all_mat[train_idx_final], label=y_tr[train_idx_final])
dcal   = xgb.DMatrix(Xtr_all_mat[cal_idx],         label=y_cal)
dtest  = xgb.DMatrix(Xte_all_mat)

params_xgb_final = {
    "objective": "reg:squarederror",
    "max_depth": best_xgb_depth, "eta": best_xgb_eta,
    "subsample": 0.8, "colsample_bytree": 0.8,
    "tree_method": tree_method, "device": xgb_device,
    "verbosity": 0,
}
xgb_fit = xgb.train(params_xgb_final, dtrain, num_boost_round=best_xgb_rounds, verbose_eval=False)

pred_cal_final = xgb_fit.predict(dcal)
resid_final    = np.abs(y_cal - pred_cal_final)
q_hat_final    = np.quantile(resid_final, 1 - alpha)

pred_test      = xgb_fit.predict(dtest)
lower_bound_xgb = y_tr.min()

xgb_q = pd.DataFrame({
    "q05": np.maximum(pred_test - q_hat_final, lower_bound_xgb),
    "q50": pred_test,
    "q95": pred_test + q_hat_final,
})

# Dense quantile grid for XGB (linear symmetric extrapolation)
scale_tau2 = (Q_GRID - 0.5) / 0.45
xgb_qgrid_mat = np.outer(pred_test, np.ones(len(Q_GRID))) + np.outer(np.ones(n_te), scale_tau2) * q_hat_final
xgb_qgrid = pd.DataFrame(xgb_qgrid_mat, columns=[f"q{int(round(100*t)):02d}" for t in Q_GRID])
# %%
# ============================================================
# 12. MODEL 5 – Deep Ensemble (QRNN)  [GPU-accelerated]
# ============================================================
print("\n=== Deep Ensemble (QRNN) ===")

# --- Standardise for NN ---
x_mean_ens = Xtr_all_mat.mean(axis=0)
x_std_ens  = Xtr_all_mat.std(axis=0)
x_std_ens[x_std_ens == 0] = 1.0

Xtr_ens_sc = (Xtr_all_mat - x_mean_ens) / x_std_ens
Xte_ens_sc = (Xte_all_mat - x_mean_ens) / x_std_ens

y_mean_ens = y_tr.mean()
y_std_ens  = y_tr.std()
y_tr_sc    = (y_tr - y_mean_ens) / y_std_ens

n_input     = Xtr_ens_sc.shape[1]
n_quantiles = len(Q_GRID)

tau_t = torch.tensor(Q_GRID, dtype=torch.float32, device=device).unsqueeze(0)  # (1, K)


def pinball_loss_torch(pred, y_true, tau):
    """pred: (B, K), y_true: (B, 1), tau: (1, K)"""
    err = y_true - pred
    loss = torch.where(err >= 0, tau * err, (tau - 1) * err)
    return loss.mean()


def make_qrnn(n_in, n_out, h1, h2, dropout_rate=0.0):
    if dropout_rate > 0:
        return nn.Sequential(
            nn.Linear(n_in, h1), nn.ReLU(), nn.Dropout(dropout_rate),
            nn.Linear(h1, h2),   nn.ReLU(), nn.Dropout(dropout_rate),
            nn.Linear(h2, n_out)
        )
    return nn.Sequential(
        nn.Linear(n_in, h1), nn.ReLU(),
        nn.Linear(h1, h2),   nn.ReLU(),
        nn.Linear(h2, n_out)
    )


def train_qrnn(X_sc, y_sc, h1, h2, lr, epochs=60, batch_size=512, dropout_rate=0.0, seed_offset=0):
    torch.manual_seed(SEED + seed_offset)
    net = make_qrnn(n_input, n_quantiles, h1, h2, dropout_rate).to(device)
    optimizer = torch.optim.Adam(net.parameters(), lr=lr)
    Xt = torch.tensor(X_sc, dtype=torch.float32)
    yt = torch.tensor(y_sc.reshape(-1, 1), dtype=torch.float32)
    ds = TensorDataset(Xt, yt)
    dl = DataLoader(ds, batch_size=batch_size, shuffle=True)
    net.train()
    for _ in range(epochs):
        for Xb, yb in dl:
            Xb, yb = Xb.to(device), yb.to(device)
            optimizer.zero_grad()
            pred = net(Xb)
            loss = pinball_loss_torch(pred, yb, tau_t)
            loss.backward()
            optimizer.step()
    net.eval()
    return net


def predict_quantiles(net, X_sc, training=False):
    """Returns (n, K) de-standardised quantile predictions."""
    net.train(training)
    with torch.no_grad():
        Xt = torch.tensor(X_sc, dtype=torch.float32, device=device)
        out_sc = net(Xt).cpu().numpy()
    q_all = out_sc * y_std_ens + y_mean_ens
    q_all = np.sort(q_all, axis=1)   # enforce monotonicity
    return q_all


# --- HP tuning ---
ens_param_grid = list(product([64, 128], [32, 64], [0.0005, 0.001]))

Xtr_tune = Xtr_ens_sc[train_idx_hp]
ytr_tune = y_tr_sc[train_idx_hp]
Xval_sc  = Xtr_ens_sc[val_idx_hp]
yval     = y_tr[val_idx_hp]

ens_cv_scores = []
for h1, h2, lr in ens_param_grid:
    print(f"  h1={h1} h2={h2} lr={lr:.4g} ...", end="", flush=True)
    net = train_qrnn(Xtr_tune, ytr_tune, h1, h2, lr, epochs=60, batch_size=512)
    q_all = predict_quantiles(net, Xval_sc)
    q05_i = np.argmin(np.abs(Q_GRID - 0.05))
    q50_i = np.argmin(np.abs(Q_GRID - 0.50))
    q95_i = np.argmin(np.abs(Q_GRID - 0.95))
    preds = q_all[:, [q05_i, q50_i, q95_i]]
    wis = mean_wis(yval, preds, Q_LEVELS)
    ens_cv_scores.append(wis)
    print(f" WIS={wis:.4f}")

best_ens_idx = int(np.argmin(ens_cv_scores))
best_ens_h1, best_ens_h2, best_ens_lr = ens_param_grid[best_ens_idx]
print(f"✓ Best ENS: h1={best_ens_h1}, h2={best_ens_h2}, lr={best_ens_lr} (WIS={ens_cv_scores[best_ens_idx]:.4f})")

# --- Train ensemble ---
M_ens = 5
ensemble_nets = []
for m in range(M_ens):
    print(f"  Training ensemble member {m+1}/{M_ens} ...")
    net = train_qrnn(Xtr_ens_sc, y_tr_sc, best_ens_h1, best_ens_h2, best_ens_lr,
                     epochs=150, batch_size=512, seed_offset=m)
    ensemble_nets.append(net)

# --- Ensemble prediction ---
qpreds_list = [predict_quantiles(net, Xte_ens_sc) for net in ensemble_nets]
ens_q_all = np.mean(qpreds_list, axis=0)   # (n_te, K)
ens_q_all = np.sort(ens_q_all, axis=1)
lower_bound_ens = y_tr.min()
ens_q_all = np.maximum(ens_q_all, lower_bound_ens)

q05_i = np.argmin(np.abs(Q_GRID - 0.05))
q50_i = np.argmin(np.abs(Q_GRID - 0.50))
q95_i = np.argmin(np.abs(Q_GRID - 0.95))

ens_q = pd.DataFrame({
    "q05": ens_q_all[:, q05_i],
    "q50": ens_q_all[:, q50_i],
    "q95": ens_q_all[:, q95_i],
})
ens_qgrid = pd.DataFrame(ens_q_all, columns=[f"q{int(round(100*t)):02d}" for t in Q_GRID])
print("Deep Ensemble training complete.\n")
# %%
# ============================================================
# 13. MODEL 6 – MC Dropout (QRNN)  [GPU-accelerated]
# ============================================================
print("=== MC Dropout (QRNN) ===")

mcd_param_grid = list(product([0.05, 0.1, 0.2], [0.0005, 0.001]))
mcd_cv_scores = []

lower_bound_mc = y_tr.min()

for dropout_rate, lr in mcd_param_grid:
    print(f"  dropout={dropout_rate:.2f}, lr={lr:.4g} ...", end="", flush=True)
    net = train_qrnn(Xtr_tune, ytr_tune, 128, 64, lr, epochs=60, batch_size=512,
                     dropout_rate=dropout_rate)
    # MC inference: average over 25 stochastic forward passes
    preds_acc = np.zeros((len(val_idx_hp), n_quantiles))
    M_pass = 25
    for _ in range(M_pass):
        preds_acc += predict_quantiles(net, Xval_sc, training=True)
    preds_acc /= M_pass
    preds_acc = np.sort(preds_acc, axis=1)
    preds_acc = np.maximum(preds_acc, lower_bound_mc)
    preds3 = preds_acc[:, [q05_i, q50_i, q95_i]]
    wis = mean_wis(yval, preds3, Q_LEVELS)
    mcd_cv_scores.append(wis)
    print(f" WIS={wis:.4f}")

best_mcd_idx = int(np.argmin(mcd_cv_scores))
best_mcd_dropout, best_mcd_lr = mcd_param_grid[best_mcd_idx]
print(f"✓ Best MCD: dropout={best_mcd_dropout}, lr={best_mcd_lr} (WIS={mcd_cv_scores[best_mcd_idx]:.4f})")

# Final MC Dropout model
print("=== Training FINAL MC Dropout ===")
mc_dropout_net = train_qrnn(Xtr_ens_sc, y_tr_sc, 128, 64, best_mcd_lr,
                             epochs=150, batch_size=512,
                             dropout_rate=best_mcd_dropout)

M_dropout = 100
mcd_q_acc = np.zeros((n_te, n_quantiles))
for _ in range(M_dropout):
    mcd_q_acc += predict_quantiles(mc_dropout_net, Xte_ens_sc, training=True)
mcd_q_all = mcd_q_acc / M_dropout
mcd_q_all = np.sort(mcd_q_all, axis=1)
mcd_q_all = np.maximum(mcd_q_all, lower_bound_mc)

mcd_q = pd.DataFrame({
    "q05": mcd_q_all[:, q05_i],
    "q50": mcd_q_all[:, q50_i],
    "q95": mcd_q_all[:, q95_i],
})
mcd_qgrid = pd.DataFrame(mcd_q_all, columns=[f"q{int(round(100*t)):02d}" for t in Q_GRID])
print("MC Dropout training complete.\n")
# %%
# ============================================================
# 14. Evaluation – Coverage, Width, WIS, CRPS, NLL
# ============================================================
print("\n=== Evaluation ===")

res_drf = eval_interval(drf_q, y_te)
res_qrf = eval_interval(qrf_q, y_te)
res_rf  = eval_interval(rf_q,  y_te)
res_xgb = eval_interval(xgb_q, y_te)
res_ens = eval_interval(ens_q, y_te)
res_mcd = eval_interval(mcd_q, y_te)

eval_table = pd.DataFrame({
    "Model":    ["DRF", "QRF", "RF", "XGB", "ENS", "MCD"],
    "Coverage": [res_drf["coverage"], res_qrf["coverage"], res_rf["coverage"],
                 res_xgb["coverage"], res_ens["coverage"], res_mcd["coverage"]],
    "Width":    [res_drf["width"],    res_qrf["width"],    res_rf["width"],
                 res_xgb["width"],    res_ens["width"],    res_mcd["width"]],
})

def wis_score_df(q_df, y):
    preds = q_df[["q05","q50","q95"]].values
    return mean_wis(y, preds, Q_LEVELS)

wis_comparison = {
    "DRF": wis_score_df(drf_q, y_te), "QRF": wis_score_df(qrf_q, y_te),
    "RF":  wis_score_df(rf_q,  y_te), "XGB": wis_score_df(xgb_q, y_te),
    "ENS": wis_score_df(ens_q, y_te), "MCD": wis_score_df(mcd_q, y_te),
}

crps_comparison = {
    "DRF": crps_from_quantiles(y_te, drf_qgrid.values, Q_GRID),
    "QRF": crps_from_quantiles(y_te, qrf_qgrid.values, Q_GRID),
    "RF":  crps_from_quantiles(y_te, rf_qgrid.values,  Q_GRID),
    "XGB": crps_from_quantiles(y_te, xgb_qgrid.values, Q_GRID),
    "ENS": crps_from_quantiles(y_te, ens_qgrid.values, Q_GRID),
    "MCD": crps_from_quantiles(y_te, mcd_qgrid.values, Q_GRID),
}

nll_comparison = {}
for name, qgrid in [("DRF", drf_qgrid), ("QRF", qrf_qgrid), ("RF", rf_qgrid),
                     ("ENS", ens_qgrid), ("MCD", mcd_qgrid)]:
    dens = approx_density_from_quantiles(y_te, qgrid.values, Q_GRID)
    nll_comparison[name] = -np.mean(np.log(dens))

print("\nCoverage & Width:")
print(eval_table.to_string(index=False))

print("\nWIS (lower is better):")
for k, v in wis_comparison.items():
    print(f"  {k}: {v:.4f}")

print("\nCRPS (lower is better):")
for k, v in crps_comparison.items():
    print(f"  {k}: {v:.4f}")

print("\nNLL (lower is better):")
for k, v in nll_comparison.items():
    print(f"  {k}: {v:.4f}")
# %%
# ============================================================
# 15. Build eval_df for subgroup analysis
# ============================================================
eval_df = pd.DataFrame({
    "y": y_te,
    "drf_q05": drf_q["q05"].values, "drf_q50": drf_q["q50"].values, "drf_q95": drf_q["q95"].values,
    "qrf_q05": qrf_q["q05"].values, "qrf_q50": qrf_q["q50"].values, "qrf_q95": qrf_q["q95"].values,
    "rf_q05":  rf_q["q05"].values,  "rf_q50":  rf_q["q50"].values,  "rf_q95":  rf_q["q95"].values,
    "xgb_q05": xgb_q["q05"].values, "xgb_q50": xgb_q["q50"].values, "xgb_q95": xgb_q["q95"].values,
    "ens_q05": ens_q["q05"].values, "ens_q50": ens_q["q50"].values, "ens_q95": ens_q["q95"].values,
    "mcd_q05": mcd_q["q05"].values, "mcd_q50": mcd_q["q50"].values, "mcd_q95": mcd_q["q95"].values,
}).reset_index(drop=True)

# LOS group
eval_df["los_group"] = pd.cut(eval_df["y"], bins=[-np.inf, 3, 7, np.inf],
                               labels=["Short (≤3d)", "Medium (3–7d)", "Long (>7d)"])

# Missingness group
miss_mat = Xte_tree_df.isnull().values
eval_df["miss_rate"] = miss_mat.mean(axis=1)
terciles = np.quantile(eval_df["miss_rate"], [0, 1/3, 2/3, 1])
eval_df["miss_group"] = pd.cut(eval_df["miss_rate"], bins=terciles,
                                labels=["Low missing", "Mid missing", "High missing"],
                                include_lowest=True)

# Ventilation and vasopressor subgroups
if "vent_any" in Xte_tree_df.columns:
    eval_df["vent_group"] = Xte_tree_df["vent_any"].map({0: "No ventilation", 1: "Ventilated"}).values
if "vasopressors" in Xte_tree_df.columns:
    eval_df["vaso_group"] = Xte_tree_df["vasopressors"].map({0: "No vasopressors", 1: "Vasopressors"}).values
# %%
# ============================================================
# 16. Subgroup WIS analysis
# ============================================================
models = ["DRF", "QRF", "RF", "XGB", "ENS", "MCD"]
model_cols = {
    "DRF": ("drf_q05", "drf_q50", "drf_q95"),
    "QRF": ("qrf_q05", "qrf_q50", "qrf_q95"),
    "RF":  ("rf_q05",  "rf_q50",  "rf_q95"),
    "XGB": ("xgb_q05", "xgb_q50", "xgb_q95"),
    "ENS": ("ens_q05", "ens_q50", "ens_q95"),
    "MCD": ("mcd_q05", "mcd_q50", "mcd_q95"),
}

def wis_one(sub, model):
    q05, q50, q95 = model_cols[model]
    preds = sub[[q05, q50, q95]].values
    return mean_wis(sub["y"].values, preds, Q_LEVELS)

def compute_subgroup_wis(eval_df, groupby_col):
    rows = []
    for grp, sub in eval_df.groupby(groupby_col, observed=True):
        for m in models:
            rows.append({"group": grp, "model": m, "WIS": wis_one(sub, m)})
    return pd.DataFrame(rows)

miss_wis  = compute_subgroup_wis(eval_df, "miss_group")
print("\n=== WIS by missingness subgroup ===")
print(miss_wis.pivot(index="group", columns="model", values="WIS").round(4))

if "vent_group" in eval_df.columns:
    vent_wis = compute_subgroup_wis(eval_df, "vent_group")
    print("\n=== WIS by ventilation subgroup ===")
    print(vent_wis.pivot(index="group", columns="model", values="WIS").round(4))

if "vaso_group" in eval_df.columns:
    vaso_wis = compute_subgroup_wis(eval_df, "vaso_group")
    print("\n=== WIS by vasopressor subgroup ===")
    print(vaso_wis.pivot(index="group", columns="model", values="WIS").round(4))
# %%
# ============================================================
# 17. High-missingness uncertainty analysis
# ============================================================
eval_hm = eval_df[eval_df["miss_group"] == "High missing"].copy()

for m, (q05c, q50c, q95c) in model_cols.items():
    w = eval_hm[q95c] - eval_hm[q05c]
    median_w = w.median()
    eval_hm[f"{m.lower()}_uncert"] = np.where(w >= median_w, "High uncertainty", "Low uncertainty")

uncert_wis_rows = []
for m in models:
    uc_col = f"{m.lower()}_uncert"
    q05c, q50c, q95c = model_cols[m]
    for uc in ["Low uncertainty", "High uncertainty"]:
        sub = eval_hm[eval_hm[uc_col] == uc]
        if len(sub) > 0:
            preds = sub[[q05c, q50c, q95c]].values
            w_val = mean_wis(sub["y"].values, preds, Q_LEVELS)
        else:
            w_val = np.nan
        uncert_wis_rows.append({"model": m, "uncert": uc, "WIS": w_val})

out_uncert_wis = pd.DataFrame(uncert_wis_rows)
print("\n=== WIS under high missingness by uncertainty stratum ===")
print(out_uncert_wis.pivot(index="uncert", columns="model", values="WIS").round(4))
# %%
# ============================================================
# 18. Conditional calibration
# ============================================================
K_bins = 10

cal_rows = []
for m in models:
    q05c, q50c, q95c = model_cols[m]
    tmp = eval_df[["y", q05c, q50c, q95c]].copy()
    tmp.columns = ["y", "q05", "q50", "q95"]
    tmp["model"] = m
    tmp["covered"] = (tmp["y"] >= tmp["q05"]) & (tmp["y"] <= tmp["q95"])
    tmp["bin"] = pd.qcut(tmp["q50"], q=K_bins, labels=False, duplicates="drop")
    grp = tmp.groupby("bin").agg(
        x=("q50", "mean"), coverage=("covered", "mean"), n_bin=("covered", "count")
    ).reset_index()
    grp["model"] = m
    cal_rows.append(grp)

cal_df_clean = pd.concat(cal_rows, ignore_index=True)
cal_df_clean["se"] = np.sqrt(cal_df_clean["coverage"] * (1 - cal_df_clean["coverage"]) / cal_df_clean["n_bin"])
cal_df_clean["lower"] = (cal_df_clean["coverage"] - 1.96 * cal_df_clean["se"]).clip(0, 1)
cal_df_clean["upper"] = (cal_df_clean["coverage"] + 1.96 * cal_df_clean["se"]).clip(0, 1)
# %%
# ============================================================
# 19. Distribution comparison plots
# ============================================================
print("\n=== Generating distribution comparison plots ===")

from pathlib import Path

# --- output dir (portable & writable) ---
OUT_DIR = Path.cwd() / "outputs"
OUT_DIR.mkdir(parents=True, exist_ok=True)

print("Saving to:", OUT_DIR.resolve())
rng_plot = np.random.default_rng(SEED)
n_samples_per_patient = 100

drf_samples = sample_from_quantiles(drf_qgrid.values, Q_GRID, n_samples_per_patient, rng=rng_plot)
qrf_samples = sample_from_quantiles(qrf_qgrid.values, Q_GRID, n_samples_per_patient, rng=rng_plot)
rf_samples  = sample_from_quantiles(rf_qgrid.values,  Q_GRID, n_samples_per_patient, rng=rng_plot)
ens_samples = sample_from_quantiles(ens_qgrid.values, Q_GRID, n_samples_per_patient, rng=rng_plot)
mcd_samples = sample_from_quantiles(mcd_qgrid.values, Q_GRID, n_samples_per_patient, rng=rng_plot)

cut_99 = np.quantile(y_te, 0.99) * 1.5

df_dist = pd.DataFrame({
    "LOS": np.concatenate([
        y_te,
        drf_samples.flatten(), qrf_samples.flatten(), rf_samples.flatten(),
        ens_samples.flatten(), mcd_samples.flatten(),
    ]),
    "Model": (
        ["Actual"] * len(y_te) +
        ["DRF"]  * drf_samples.size +
        ["QRF"]  * qrf_samples.size +
        ["RF"]   * rf_samples.size  +
        ["ENS"]  * ens_samples.size +
        ["MCD"]  * mcd_samples.size
    )
})
df_dist = df_dist[(df_dist["LOS"] >= 0) & (df_dist["LOS"] <= cut_99)].copy()

MODEL_ORDER = ["Actual", "DRF", "QRF", "RF", "ENS", "MCD"]
df_dist["Model"] = pd.Categorical(df_dist["Model"], categories=MODEL_ORDER)

COLORS_FILL  = {"DRF": "#F4A3A3", "QRF": "#A8C5E5", "RF": "#B7E3B0", "ENS": "#984EA3", "MCD": "#FF7F00"}
COLORS_LINE  = {"DRF": "#D65C5C", "QRF": "#5A9BD4", "RF": "#66BB66", "ENS": "#7A3E82", "MCD": "#CC6600"}

df_actual  = df_dist[df_dist["Model"] == "Actual"]
df_models  = df_dist[df_dist["Model"] != "Actual"]

fig, axes = plt.subplots(2, 3, figsize=(14, 8), sharex=True)
axes_flat = axes.flatten()
x_lim = (0, np.quantile(y_te, 0.95) * 1.3)

for ax, m in zip(axes_flat, ["DRF", "QRF", "RF", "ENS", "MCD"]):
    sns.kdeplot(df_actual["LOS"], ax=ax, color="gray", fill=True, alpha=0.3, linewidth=0.5)
    sub = df_models[df_models["Model"] == m]
    sns.kdeplot(sub["LOS"], ax=ax, color=COLORS_LINE[m], fill=True, facecolor=COLORS_FILL[m], alpha=0.5, linewidth=0.9)
    ax.set_title(m, fontweight="bold")
    ax.set_xlim(x_lim)
    ax.set_xlabel("Length of Stay (Days)")
    ax.set_ylabel("Density")
    ax.grid(axis="y", alpha=0.3)
    ax.grid(axis="x", visible=False)

axes_flat[-1].set_visible(False)
fig.suptitle("Comparison of Predictive Distributions against Actual LOS\n"
             "(Gray = actual distribution in all panels)", fontsize=14, fontweight="bold")
plt.tight_layout()
plt.savefig(OUT_DIR / "academic_dist_comparison.png", dpi=600, bbox_inches="tight")
plt.close()
print("✓ Plot saved: academic_dist_comparison.png")

# --- KL divergence & KS test ---
kl_results = {
    "DRF": kl_divergence_kde(y_te, drf_samples.flatten()),
    "QRF": kl_divergence_kde(y_te, qrf_samples.flatten()),
    "RF":  kl_divergence_kde(y_te, rf_samples.flatten()),
    "ENS": kl_divergence_kde(y_te, ens_samples.flatten()),
    "MCD": kl_divergence_kde(y_te, mcd_samples.flatten()),
}

ks_results = {
    "DRF": ks_2samp(y_te, drf_samples.flatten()).statistic,
    "QRF": ks_2samp(y_te, qrf_samples.flatten()).statistic,
    "RF":  ks_2samp(y_te, rf_samples.flatten()).statistic,
    "ENS": ks_2samp(y_te, ens_samples.flatten()).statistic,
    "MCD": ks_2samp(y_te, mcd_samples.flatten()).statistic,
}

print("\nKL Divergence (lower is better):", {k: round(v, 4) for k, v in kl_results.items()})
print("KS Distance (lower is better):",   {k: round(v, 4) for k, v in ks_results.items()})
# %%
# ============================================================
# 20. Conditional calibration plot
# ============================================================
fig, axes = plt.subplots(3, 2, figsize=(12, 12), sharey=True)
axes_flat = axes.flatten()
for ax, m in zip(axes_flat, models):
    sub = cal_df_clean[cal_df_clean["model"] == m]
    ax.axhline(0.90, color="red", linestyle="--", alpha=0.6)
    ax.fill_between(sub["x"], sub["lower"], sub["upper"], alpha=0.15, color="steelblue")
    ax.plot(sub["x"], sub["coverage"], color="steelblue", lw=0.8)
    ax.scatter(sub["x"], sub["coverage"], color="steelblue", s=15, alpha=0.8)
    ax.set_title(m, fontweight="bold")
    ax.set_ylim(0.6, 1.0)
    ax.yaxis.set_major_formatter(mticker.PercentFormatter(xmax=1, decimals=0))
    ax.set_xlabel("Predicted Median LOS (Days)")
    ax.set_ylabel("Empirical Coverage")
fig.suptitle("Conditional Calibration of 90% Prediction Intervals", fontweight="bold")
plt.tight_layout()
plt.savefig(OUT_DIR / "academic_dist_comparison.png", dpi=600, bbox_inches="tight")
plt.close()
print("✓ Plot saved: conditional_calibration.png")
# %%
# ============================================================
# 21. Coverage vs Width plot
# ============================================================
cover_rows = []
for m in models:
    q05c, _, q95c = model_cols[m]
    tmp = eval_df[["y", q05c, q95c]].copy()
    tmp.columns = ["y", "q05", "q95"]
    tmp["w"]   = tmp["q95"] - tmp["q05"]
    tmp["cov"] = ((tmp["y"] >= tmp["q05"]) & (tmp["y"] <= tmp["q95"])).astype(int)
    tmp["model"] = m
    cover_rows.append(tmp[["y","w","cov","model"]])

cover_long = pd.concat(cover_rows)
cover_long = cover_long[(cover_long["w"].notna()) & (cover_long["w"] >= 0)]

n_bins = 10
cal_w_rows = []
for m, grp in cover_long.groupby("model"):
    grp = grp.copy()
    grp["w_bin"] = pd.qcut(grp["w"], q=n_bins, labels=False, duplicates="drop")
    bgrp = grp.groupby("w_bin").agg(n_bin=("cov","count"), x=("w","median"), coverage=("cov","mean")).reset_index()
    bgrp["model"] = m
    bgrp["se"] = np.sqrt(bgrp["coverage"] * (1 - bgrp["coverage"]) / bgrp["n_bin"])
    bgrp["lower"] = (bgrp["coverage"] - 1.96 * bgrp["se"]).clip(0, 1)
    bgrp["upper"] = (bgrp["coverage"] + 1.96 * bgrp["se"]).clip(0, 1)
    cal_w_rows.append(bgrp)

cal_w_df = pd.concat(cal_w_rows)

fig, axes = plt.subplots(3, 2, figsize=(12, 12))
axes_flat = axes.flatten()
for ax, m in zip(axes_flat, models):
    sub = cal_w_df[cal_w_df["model"] == m]
    ax.axhline(0.90, color="red", linestyle="--", alpha=0.7, lw=0.8)
    ax.fill_between(sub["x"], sub["lower"], sub["upper"], alpha=0.15)
    ax.plot(sub["x"], sub["coverage"], lw=0.9)
    ax.scatter(sub["x"], sub["coverage"], s=18, alpha=0.85)
    ax.set_title(m, fontweight="bold")
    ax.set_ylim(0, 1)
    ax.yaxis.set_major_formatter(mticker.PercentFormatter(xmax=1, decimals=0))
    ax.set_xlabel("Predicted interval width (bin median)")
    ax.set_ylabel("Empirical coverage within bin")
fig.suptitle("Coverage vs Predicted Interval Width (90% PI)", fontweight="bold")
plt.tight_layout()
plt.savefig(OUT_DIR / "academic_dist_comparison.png", dpi=600, bbox_inches="tight")
plt.close()
print("✓ Plot saved: coverage_vs_width.png")
# %%
# ============================================================
# 22. HYBRID MODEL – Neural Features + DRF (Quantile RF)
# ============================================================
print("\n\n========================================")
print("TRAINING HYBRID MODEL: Neural Features + RF-DRF")
print("========================================\n")

# Stage 1: Train feature extractor network
print("Stage 1: Training neural feature extractor ...")
torch.manual_seed(SEED)

nn_feature_extractor = nn.Sequential(
    nn.Linear(n_input, 128), nn.ReLU(),
    nn.Linear(128, 64),      nn.ReLU()
).to(device)

nn_output_head = nn.Linear(64, n_quantiles).to(device)

optimizer_hybrid = torch.optim.Adam(
    list(nn_feature_extractor.parameters()) + list(nn_output_head.parameters()), lr=0.001
)

Xtr_t = torch.tensor(Xtr_ens_sc, dtype=torch.float32)
Xte_t = torch.tensor(Xte_ens_sc, dtype=torch.float32)
ytr_t = torch.tensor(y_tr_sc.reshape(-1, 1), dtype=torch.float32)

ds_hyb = TensorDataset(Xtr_t, ytr_t)
dl_hyb = DataLoader(ds_hyb, batch_size=512, shuffle=True)

nn_feature_extractor.train()
nn_output_head.train()
for epoch in range(1, 151):
    for Xb, yb in dl_hyb:
        Xb, yb = Xb.to(device), yb.to(device)
        optimizer_hybrid.zero_grad()
        feats = nn_feature_extractor(Xb)
        qpreds = nn_output_head(feats)
        loss = pinball_loss_torch(qpreds, yb, tau_t)
        loss.backward()
        optimizer_hybrid.step()
    if epoch % 50 == 0:
        print(f"    Epoch {epoch}, Loss: {loss.item():.4f}")

print("  Neural network training complete.\n")

# Stage 2: Extract features
print("Stage 2: Extracting 64-dim neural features ...")
nn_feature_extractor.eval()
with torch.no_grad():
    neural_features_tr = nn_feature_extractor(Xtr_t.to(device)).cpu().numpy()
    neural_features_te = nn_feature_extractor(Xte_t.to(device)).cpu().numpy()

print(f"  Train: {neural_features_tr.shape[0]} samples x {neural_features_tr.shape[1]} features")
print(f"  Test: {neural_features_te.shape[0]} samples x {neural_features_te.shape[1]} features")

feat_cols = [f"nn_feat_{i+1}" for i in range(neural_features_tr.shape[1])]
df_neural_tr = pd.DataFrame(neural_features_tr, columns=feat_cols)
df_neural_te = pd.DataFrame(neural_features_te, columns=feat_cols)

# Stage 3: Fit DRF on neural features (using real drf package)
print("Stage 3: Training DRF on neural features (FourierMMD splitting) ...")
df_neural_tr_pd = pd.DataFrame(neural_features_tr, columns=feat_cols)
df_neural_te_pd = pd.DataFrame(neural_features_te, columns=feat_cols)

hybrid_drf = drf(
    min_node_size=50,
    num_trees=1000,
    splitting_rule="FourierMMD",
)
hybrid_drf.fit(df_neural_tr_pd, pd.DataFrame({"y": y_tr}))
print("  DRF on neural features complete.\n")

# Stage 4: Predict quantiles via DRF weights
print("Stage 4: Generating hybrid predictions ...")
hybrid_pred  = hybrid_drf.predict(newdata=df_neural_te_pd)
W_h          = hybrid_pred.weights      # (n_te, n_tr)
y_train_h    = y_tr                     # aligned with W_h columns

lower_bound_hyb = y_tr.min()
hybrid_q_mat     = np.zeros((n_te, 3))
hybrid_qgrid_mat = np.zeros((n_te, len(Q_GRID)))

for i in tqdm(range(n_te), ncols=60, desc="Hybrid predict"):
    w_i = np.asarray(W_h[i], dtype=float)
    if w_i.sum() <= 0:
        hybrid_q_mat[i]     = [lower_bound_hyb] * 3
        hybrid_qgrid_mat[i] = [lower_bound_hyb] * len(Q_GRID)
        continue
    hybrid_q_mat[i]     = wquant(y_train_h, w_i, Q_LEVELS)
    hybrid_qgrid_mat[i] = wquant(y_train_h, w_i, Q_GRID)

hybrid_q_mat     = np.maximum(hybrid_q_mat,     lower_bound_hyb)
hybrid_qgrid_mat = np.maximum(hybrid_qgrid_mat, lower_bound_hyb)

hybrid_q     = pd.DataFrame(hybrid_q_mat,     columns=["q05", "q50", "q95"])
hybrid_qgrid = pd.DataFrame(hybrid_qgrid_mat, columns=[f"q{int(round(100*t)):02d}" for t in Q_GRID])

# ============================================================
# 23. Hybrid evaluation
# ============================================================
print("\n========================================")
print("EVALUATION: Hybrid vs Pure Models")
print("========================================\n")

res_hybrid = eval_interval(hybrid_q, y_te)
wis_hybrid = wis_score_df(hybrid_q, y_te)
crps_hybrid = crps_from_quantiles(y_te, hybrid_qgrid.values, Q_GRID)
hybrid_dens = approx_density_from_quantiles(y_te, hybrid_qgrid.values, Q_GRID)
nll_hybrid  = -np.mean(np.log(hybrid_dens))

wis_comparison["HYBRID"]  = wis_hybrid
crps_comparison["HYBRID"] = crps_hybrid
nll_comparison["HYBRID"]  = nll_hybrid

summary_all = pd.DataFrame({
    "Model":    list(wis_comparison.keys()),
    "Coverage": [res_drf["coverage"], res_qrf["coverage"], res_rf["coverage"],
                 res_xgb["coverage"], res_ens["coverage"], res_mcd["coverage"],
                 res_hybrid["coverage"]],
    "Width":    [res_drf["width"],    res_qrf["width"],    res_rf["width"],
                 res_xgb["width"],    res_ens["width"],    res_mcd["width"],
                 res_hybrid["width"]],
    "WIS":      list(wis_comparison.values()),
    "CRPS":     list(crps_comparison.values()),
    "NLL":      [nll_comparison.get(m, np.nan) for m in wis_comparison.keys()],
})
summary_all["NLL"][summary_all["Model"] == "XGB"] = np.nan
summary_all["WIS_Rank"] = summary_all["WIS"].rank()

print("Summary Table (All Models):")
print(summary_all.round(4).to_string(index=False))

print("\nModel Rankings (by WIS):")
print(summary_all[["Model","WIS","WIS_Rank"]].sort_values("WIS_Rank").to_string(index=False))

# Hypothesis test
pure_nn     = ["ENS", "MCD"]
pure_forest = ["DRF", "QRF", "RF"]
best_nn_wis     = min(wis_comparison[m] for m in pure_nn)
best_forest_wis = min(wis_comparison[m] for m in pure_forest)
hybrid_wis_val  = wis_comparison["HYBRID"]

best_nn_name     = min(pure_nn,     key=lambda m: wis_comparison[m])
best_forest_name = min(pure_forest, key=lambda m: wis_comparison[m])

print(f"\nBest Pure NN (WIS): {best_nn_wis:.4f} ({best_nn_name})")
print(f"Best Pure Forest (WIS): {best_forest_wis:.4f} ({best_forest_name})")
print(f"Hybrid NN+RF-DRF (WIS): {hybrid_wis_val:.4f}")

beats_nn     = hybrid_wis_val < best_nn_wis
beats_forest = hybrid_wis_val < best_forest_wis

if beats_nn and beats_forest:
    print("\n✓ ✓ ✓ SUCCESS! Hybrid beats BOTH pure NN and pure forest! ✓ ✓ ✓")
    print(f"  Improvement over best NN: {100*(best_nn_wis - hybrid_wis_val)/best_nn_wis:.2f}%")
    print(f"  Improvement over best forest: {100*(best_forest_wis - hybrid_wis_val)/best_forest_wis:.2f}%")
elif beats_nn:
    print("\n⚠ Partial Success: Hybrid beats pure NN, but not pure forest")
elif beats_forest:
    print("\n⚠ Partial Success: Hybrid beats pure forest, but not pure NN")
else:
    print("\n✗ Hybrid does not beat either pure approach")
# %%
# ============================================================
# 24. Hybrid distribution comparison plot
# ============================================================
hybrid_samples = sample_from_quantiles(hybrid_qgrid.values, Q_GRID, n_samples_per_patient, rng=rng_plot)

df_dist_ext = pd.concat([
    df_dist,
    pd.DataFrame({"LOS": hybrid_samples.flatten(), "Model": "Hybrid_NN_DRF"})
], ignore_index=True)
df_dist_ext["Model"] = pd.Categorical(
    df_dist_ext["Model"],
    categories=["Actual", "DRF", "QRF", "RF", "ENS", "MCD", "Hybrid_NN_DRF"]
)

colors_hybrid = {
    "Actual": "black", "DRF": "#e41a1c", "QRF": "#984ea3",
    "RF": "#ff7f00", "ENS": "#377eb8", "MCD": "#4daf4a", "Hybrid_NN_DRF": "#a65628"
}

fig, ax = plt.subplots(figsize=(12, 6.5))
for m in ["Actual", "DRF", "QRF", "RF", "ENS", "MCD", "Hybrid_NN_DRF"]:
    sub = df_dist_ext[df_dist_ext["Model"] == m]
    if len(sub) == 0:
        continue
    ls = "solid" if m in ["Actual", "DRF", "Hybrid_NN_DRF"] else "dashed"
    sns.kdeplot(sub["LOS"], ax=ax, label=m, color=colors_hybrid[m], linestyle=ls, linewidth=1.2)
ax.set_xlim(0, np.quantile(y_te, 0.95) * 1.2)
ax.set_xlabel("Length of Stay (Days)")
ax.set_ylabel("Density")
ax.set_title("Hybrid Model: Neural Features + DRF\n"
             "Combining neural representation learning with distributional random forest", fontweight="bold")
ax.legend()
plt.tight_layout()
plt.savefig(OUT_DIR / "academic_dist_comparison.png", dpi=600, bbox_inches="tight")
plt.close()
print("\n✓ Plot saved: hybrid_distribution_comparison.png")
# %%
# ============================================================
# 25. Variable importance (DRF native — splitting frequency)
# ============================================================
print("\n=== Neural Feature Importance (DRF variable importance) ===")

# drf.info() prints variable importance based on splitting frequency
# We capture it via the internal attribute if available
try:
    hybrid_drf.info()   # prints importance table to stdout
except Exception:
    pass

# Also extract as a Series for programmatic use
try:
    var_imp = hybrid_drf.variable_importance()   # returns dict or array depending on version
    if isinstance(var_imp, dict):
        feat_imp = pd.Series(var_imp).sort_values(ascending=False)
    else:
        feat_imp = pd.Series(var_imp, index=feat_cols).sort_values(ascending=False)
    print("\nTop 15 most important neural features:")
    print(feat_imp.head(15).round(4))
except Exception as e:
    print(f"  (Variable importance not available via API: {e})")

# ============================================================
# 26. Save results
# ============================================================
print("\n=== Saving Results ===")
summary_all.to_csv("/mnt/user-data/outputs/summary_all_models.csv", index=False)
eval_df.to_csv("/mnt/user-data/outputs/eval_df.csv", index=False)
print("✓ Summary saved to: /mnt/user-data/outputs/summary_all_models.csv")
print("✓ Eval df saved to: /mnt/user-data/outputs/eval_df.csv")

print("\n\n========================================")
print("✓ ✓ ✓ HYBRID MODEL EVALUATION COMPLETE ✓ ✓ ✓")
print("========================================\n")


