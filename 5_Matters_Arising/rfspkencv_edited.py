"""
Script Name:        rfspkencv_edited.py
Description:        Calculates top-5 feature imporantance lists using random forest, Spearman's correlation, and Kendall's tau

Original author:    Yoshiyasu Takefuji (https://github.com/y-takefuji/nature)
Date accessed:      2025-05-16

Modified by:        Lindsay Hracs
Date modified:      2025-05-16
Python version:     3.13.2
Conda environment:  nature_ma_environment.yml

Dependencies:
    - pandas
    - numpy
    - scikit-learn
    - SciPy
"""

import pandas as pd
import numpy as np
from sklearn.ensemble import RandomForestClassifier
from sklearn.preprocessing import LabelEncoder
from sklearn.model_selection import cross_val_score
from scipy.stats import spearmanr, kendalltau

# 1) LOAD & CLEAN
data = pd.read_csv('imputed_data.csv') # LH: LINE EDITED TO CHANGE DATASET
if 'status' in data.columns:
   data = data[data['status'] != 'unlabelled'] # LH: LINE EDITED TO REMOVE ALL UNLABELLED DATA 
data = data[data['stage'].notna()].reset_index(drop=True) # LH: COMMENT - THERE SHOULD BE NO ROWS WHERE STAGE IS BLANK AFTER FITERING OUT UNLABELLED DATA

# LH: ADDED LINE 34 TO DROP ANY FEATURES THAT WERE NOT USED IN ORIGINAL HRACS, WINDSOR, ET AL. 2025 STUDY 
data.drop(columns=['rowname', 'year_data', 'country', 'name', 'CD_inc', 'UC_inc', 'CD_change_inc', 'UC_change_inc', 'CD_prev', 'UC_prev', 'CD_change_prev', 'UC_change_prev', 'status', 'decade'], inplace=True)

# 2) SPLIT OFF X & y
X = data.drop('stage', axis=1).copy()
y_raw = data['stage'].copy()

# 3) ENCODE CATEGORICAL FEATURES IN X
for col in X.select_dtypes(include=['object']).columns:
    X[col] = LabelEncoder().fit_transform(X[col].astype(str))

# 4) ENCODE TARGET y
y = LabelEncoder().fit_transform(y_raw)

# 5) RF IMPORTANCES ON FULL DATA → top5 & top10
rf_full = RandomForestClassifier(n_estimators=100, random_state=42)
rf_full.fit(X, y)
rf_imp_full = pd.Series(rf_full.feature_importances_, index=X.columns)\
                   .sort_values(ascending=False)
top5_rf_full    = rf_imp_full.index[:5].tolist()
top10_rf_full   = rf_imp_full.index[:10].tolist()

print("RF – top 5 features on FULL data:")
print(top5_rf_full)

# 6) RF IMPORTANCES ON REDUCED (TOP-10) DATA → top5
rf_reduced = RandomForestClassifier(n_estimators=100, random_state=42)
rf_reduced.fit(X[top10_rf_full], y)
rf_imp_reduced = pd.Series(rf_reduced.feature_importances_, index=top10_rf_full)\
                      .sort_values(ascending=False)
top5_rf_reduced = rf_imp_reduced.index[:5].tolist()

print("\nRF – top 5 features on REDUCED (top-10) data:")
print(top5_rf_reduced)

# 7) SPEARMAN ON FULL DATA → top5 & top10
spearman_full = {}
for col in X.columns:
    corr, _ = spearmanr(X[col], y)
    spearman_full[col] = abs(corr)
    
spearman_full = pd.Series(spearman_full).sort_values(ascending=False)
top5_sp_full   = spearman_full.index[:5].tolist()
top10_sp_full  = spearman_full.index[:10].tolist()

print("\nSpearman – top 5 features on FULL data:")
print(top5_sp_full)

# 8) SPEARMAN ON REDUCED (TOP-10) DATA → top5
# Use the top 10 features from Spearman on full data to create a reduced dataset
# Then recalculate Spearman on this reduced set to get top 5
spearman_reduced = {}
for col in top10_sp_full:
    corr, _ = spearmanr(X[col], y)
    spearman_reduced[col] = abs(corr)

spearman_reduced = pd.Series(spearman_reduced).sort_values(ascending=False)
top5_sp_reduced = spearman_reduced.index[:5].tolist()

print("\nSpearman – top 5 features on REDUCED (top-10) data:")
print(top5_sp_reduced)

# 9) KENDALL ON FULL DATA → top5 & top10
kendall_full = {}
for col in X.columns:
    corr, _ = kendalltau(X[col], y)
    kendall_full[col] = abs(corr)
    
kendall_full = pd.Series(kendall_full).sort_values(ascending=False)
top5_kd_full = kendall_full.index[:5].tolist()
top10_kd_full = kendall_full.index[:10].tolist()

print("\nKendall – top 5 features on FULL data:")
print(top5_kd_full)

# 10) KENDALL ON REDUCED (TOP-10) DATA → top5
# Use the top 10 features from Kendall on full data to create a reduced dataset
# Then recalculate Kendall on this reduced set to get top 5
kendall_reduced = {}
for col in top10_kd_full:
    corr, _ = kendalltau(X[col], y)
    kendall_reduced[col] = abs(corr)

kendall_reduced = pd.Series(kendall_reduced).sort_values(ascending=False)
top5_kd_reduced = kendall_reduced.index[:5].tolist()

print("\nKendall – top 5 features on REDUCED (top-10) data:")
print(top5_kd_reduced)

# 11) SHOW THE IMPORTANCES / CORRELATIONS FOR EACH 5-FEATURE SET
print("\n--- Feature importances / correlation values for each 5-feature subset ---")

print("\nRF_full_top5 importances:")
print(rf_imp_full[top5_rf_full])

print("\nRF_reduced_top5 importances:")
print(rf_imp_reduced[top5_rf_reduced])

print("\nSP_full_top5 |Spearman| values:")
print(spearman_full[top5_sp_full])

print("\nSP_reduced_top5 |Spearman| values:")
print(spearman_reduced[top5_sp_reduced])

print("\nKD_full_top5 |Kendall| values:")
print(kendall_full[top5_kd_full])

print("\nKD_reduced_top5 |Kendall| values:")
print(kendall_reduced[top5_kd_reduced])

# 12) 5-FOLD CROSS-VALIDATION ON EACH 5-FEATURE SUBSET
cv = 5
rf = RandomForestClassifier(n_estimators=100, random_state=42)

cases = [
    ("RF_full_top5",    top5_rf_full),
    ("RF_reduced_top5", top5_rf_reduced),
    ("SP_full_top5",    top5_sp_full),
    ("SP_reduced_top5", top5_sp_reduced),
    ("KD_full_top5",    top5_kd_full),
    ("KD_reduced_top5", top5_kd_reduced)
]

print("\n=== 5-Fold CV Accuracy (mean ± std) on each 5-feature subset ===")
for name, feats in cases:
    scores = cross_val_score(rf, X[feats], y, cv=cv, scoring='accuracy')
    print(f"{name:17s}: {scores.mean():.4f} ± {scores.std():.4f}")

# LH: ADDED LINES 162–183 TO CREATE OUTPUT CSV FOR EASIER COPY AND PASTE OF RESULTS
# LH: Convert top-5 series into long format with source and rank
def make_long_df(series, source_name):
    df = series.reset_index()
    df.columns = ['Feature', 'Importance value']
    df['Source'] = source_name
    return df[['Source', 'Feature', 'Importance value']]

# LH: Generate long-format dfs for each source
df_list = [
    make_long_df(rf_imp_full[top5_rf_full], 'rf_full'),
    make_long_df(rf_imp_reduced[top5_rf_reduced], 'rf_reduced'),
    make_long_df(spearman_full[top5_sp_full], 'SP_full'),
    make_long_df(spearman_reduced[top5_sp_reduced], 'SP_reduced'),
    make_long_df(kendall_full[top5_kd_full], 'KD_full'),
    make_long_df(kendall_reduced[top5_kd_reduced], 'KD_reduced'),
]

# LH: Concatenate into a single df
combined_df = pd.concat(df_list, ignore_index=True)

# LH: Save output to CSV
combined_df.to_csv('rf_feature_importances.csv', index=False)
