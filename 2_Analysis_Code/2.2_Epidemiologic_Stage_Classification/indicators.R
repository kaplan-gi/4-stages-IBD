# title: social indicator by epidemiologic stage statistics 
# author: Julia Gorospe
# script date: 2024-09-05
# R version 4.3.2 (2023-10-31)
# description: this script assesses the trends in five country-level societal indicator measures across three epidmeiologic stages defined by IBD incidence and prevalence rates


# The five indicators that will be assessed are: <br>
#   
#   * <b>ahdi:</b> Augmented Human Development Index from Escosura 2021 (https://doi.org/10.1111/ehr.13064) - calculation is geometric mean of normalized life expectancy at birth (health), mean years of schooling (education), GDP per capita (well-being), and Varieties of Democracy's Liberal democracy index (freedom). Scale of 0–1. Available in 5 year increments; interpolated using na.approx from zoo (inner-linear).
# 
# * <b>obesity:</b> Prevalence of obesity among adults defined as the percentage of the population with a body mass index (BMI) or 30kg/m2 or higher. Data is age-standardized. Extracted from WHO database.
# 
# * <b>percent_urban:</b> Percentage of population at mid-year residing in urban areas (as defined by the region). Predicted values to 2050 are also available in the original dataset. Extracted from UN database.
# 
# * <b>UHC_index:</b> Universal Health Coverage Service Coverage Index - measure of essential health services at the country level. Includes many indicators covering 4 categories of care: reproductive, newborn, maternal and child health, infectious diseases, noncommunicable diseases, and service capacity and access. See figure 1 in methods paper for complete list of features (http://dx.doi.org/10.1016/S2214-109X(17)30472-2).
# 
# * <b>western_diet_index:</b> A rough indication of how similar the average diet in the country is to that of a standard western diet high in processed foods, animal product and added sugar. Conceptually similar to the Western Diet Similarity Index described in Azzam 2021 (http://dx.doi.org/10.1017/S136898002000350X). Calculated by dividing the sum of calories/capita/day available from meat, animal oils/fats, milk, eggs, plant oils/fats and sugar by the total calories/capita/day available. Food categories are based on the 12 categories in Azzam 2021, however available calories were not adjusted for food waste due to lack of temporal data for global food waste rates. Raw data on calories by food category were extracted from the Food Balance Sheets maintained by the Food and Agriculture Organization (FAO).
# <br><br>


# Clear environment
rm(list = ls(all = TRUE))

# Set working directory
setwd("~/Desktop")      #CHANGE ME!

# Install and load packages
pacman::p_load(
  rio,                 # v0.5.16 import/exporting files
  tidyverse,           # v1.3.0
  zoo,
  plotly,
  htmlwidgets,
  dunn.test
)

# Load data
meta_raw <- import("https://raw.githubusercontent.com/kaplan-gi/4-stages-IBD/main/indicators_example_data.csv")
# for demo provided on GitHub please use the indicators_example_data.csv file which contains a 10% sample of regions in the dataset


# Data preparation
# Interpolate variables with missing values
meta <- meta_raw %>% 
  group_by(country) %>% 
  arrange(country, year) %>% 
  mutate(ahdi_int = na.approx(ahdi, na.rm = FALSE),
         UHC_index_int = na.approx(UHC_index, na.rm = FALSE))

# List indicators to assess
indicators <- c("ahdi_int", "percent_urban", "western_diet_index", "obesity", "UHC_index_int")

# Reshape data
box_data <- meta[, c("year", indicators, "predicted_stage")] %>% 
  gather(-year, -predicted_stage, key = "var", value = "value", na.rm = TRUE) %>% 
  group_by(var, predicted_stage) %>% 
  mutate(label = sprintf("%.2f", median(value)))




# Test for differences in indcators between stages
# Create separate dataframes for each variable
indicator_list <- split(box_data, box_data$var)

set.seed(5678)

# Kruskal-Wallis (non-parametric) test for rank sum difference between >=3 groups
for(i in  1:length(indicator_list)){
  print(unique(indicator_list[[i]]$var))
  print(kruskal.test(value ~ predicted_stage, data = indicator_list[[i]]))
}

# Post hoc Dunn's test for pairwise comparisons
for(i in 1:length(indicator_list)){
  print(unique(indicator_list[[i]]$var))
  print(dunn.test(indicator_list[[i]]$value, indicator_list[[i]]$predicted_stage, kw = TRUE, method = "bonferroni"))
}

