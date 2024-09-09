# Visualization Code

This folder contains the scripts used to generate the figures in our paper *The Global Evolution of Inflammatory Bowel Disease across Four Epidemiologic Stages* (submitted - 2024/01/08). Synthetic example data, supplied for reproducibility, can be found in the 1_Data/data_for_figure_generation folder of this repository. More complete datasets will be available upon publication.

Pre-print DOI: https://doi.org/10.21203/rs.3.rs-3846147/v1

## File Descriptions

heatmaps_medians.R | Produces gradient heatmaps for country-decade-level aggregate incidence and prevalence of Crohn's disease and ulcerative colitis, plots are built with the ggplot2 package. See the 1_Data

indicators_plot.Rmd | Produces box plots for five societal indicator measures stratified by three epidemiologic stages, plots are built with the plotly package

scatterplots_regional.Rmd | Produces interactive scatterplots with LOESS trend lines for individual incidence and prevalence of Crohn's disease and ulcerative colitis, plots are built with the plotly package and are available for download from the 4_Plots/4.1_Regional_Scatterplots folder in this repository

stages_plots.Rmd | Produces boxplot of incidence and prevalence of Crohn's disease and ulcerative colitis stratified by three epidemiologic stages, plots are built with the plotly package

uc_cd_ratio_plot.R | Produces a boxplot of UC:CD incidence ratio stratified by three epidemiologic stages, plots are built with the plotly package. Relies on rates aggregated at the country-year level via the weighted_means.R script in the 2_Analysis/2.1_Systematic_Review folder

violin_plot.Rmd | Produces a violin density plot of all Crohn's disease and ulcerative colitis incidence rates in the dataset, plots are built with the plotly package. Uses the GIVES21_exmple_data.csv dataset