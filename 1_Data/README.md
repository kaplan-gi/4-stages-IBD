# Data

This folder contains a selection of datasets used for analysis and visualization of the four epidemiologic stages of inflammatory bowel disease as described in our paper *The Global Evolution of Inflammatory Bowel Disease across Four Epidemiologic Stages* (submitted - 2024/01/08). Synthetic example data has been supplied for reproducibility, more complete datasets will be available upon publication.

Pre-print DOI: https://doi.org/10.21203/rs.3.rs-3846147/v1

## File Descriptions

base_labelled_data.csv | labelled subset of features used to train classifier in the random_forest.R script.

GIVES21_example_data.csv | 10% random sample of our systematic review data which can be used to run the AAPCs.R and weighted_mean.R scripts. Contains historic population-based Crohn's disease and Ulcerative colitis incidence and prevalence rates published at anytime anywhere in the world.

indicators_example_data.csv | 10% sample of countries in a dataset containing five country-level temporal social indicator measures. Use to run the indicators.R and indicators_plot.Rmd scripts which show trends in social indicators across the first three epidemiologic stages.

synthetic_example_data_PDE_input.csv | fake data corresponding to the Stage3_4_IBD_prevalence.nb file. 

GIVES21_example_data_dictionary.xlsx | detailed descriptions for each of the columns in the GIVES21_example_data.csv file