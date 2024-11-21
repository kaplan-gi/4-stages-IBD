# Analysis Code

This folder contains the scripts used in the analysis described in our paper *The Global Evolution of Inflammatory Bowel Disease across Four Epidemiologic Stages* (submitted - 2024/01/08). Synthetic example data, supplied for reproducibility, can be found in the 1_Data folder of this repository. More complete datasets will be available upon publication.

Pre-print DOI: https://doi.org/10.21203/rs.3.rs-3846147/v1

## File Descriptions

#### Systematic Review
AAPCs.R | Code for calculating study-level average annual percent change in incidence 

weighted_means.R | Code for calculating population weighted mean incidence and prevalence at grouped by different variables (ie. study_id or country, and year or decade)

#### Epidemiologic Stage Classification
indicators.R | Code for assessing difference in societal indicator measure between epidemiologic stages, Kruskal-Wallis non-parametric test followed by post hoc Dunn's pairwise comparisons

knn_classifier.R | 

random_forest.Rmd | Code for random forest classifier algorithm used to classify region-year groups into three epidemiologic stages based on incidence and/or prevalence of Crohn's disease and/or ulcerative colitis and change in rates over time. Includes imputation methods used to fill in missing data. Relies on the base_labelled_data.csv file in the 1_Data folder of this repository

uc_cd_ratio.R | Code used to calculate UC:CD ratio at the country-year level and model difference in UC:CD ratio between epidemiologic stages

#### Forecasting Stage 4 Prevalence
Stage3_4_IBD_prevalence.nb | Wolfram Mathematica code used to forecast IBD prevalence in three example stage 3 regions - Canada, Denmark, Scotland (Lothian) with PDE models

