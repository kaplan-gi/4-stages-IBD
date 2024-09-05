# title: UC:CD ratio by epidemiologic stage statistics 
# author: Julia Gorospe
# script date: 2024-09-05
# R version 4.3.2 (2023-10-31)
# description: this script assesses the trends in UC:CD ratio across three epidmeiologic stages defined by IBD incidence and prevalence rates

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
  emmeans
)

# Load data
means_raw <- import("wm_country-year.csv") # generated using the weighted_means script with aggregation at the country and year_data level
stages_raw <- import("decade_random_forest_results.csv") # results from the random_forest classifier script aggregated at the country and decade level

# Data manipulation
# Collapse by country and year to calculate UC_CD ratio across rows
ratio_cy <- means_raw %>% 
  mutate(UC_CD_ratio = ifelse(rate_mean.CDinc >0, rate_mean.UCinc/rate_mean.CDinc, NA),
         decade = (year_data) %/% 10) %>% 
  group_by(decade) %>% 
  mutate(decade = paste(range(year_data), collapse="-")) %>% 
  mutate(decade = case_when(decade == "1925-1925" ~ "1920-1929",
                            decade == "1931-1939" ~ "1930-1939",
                            decade == "2020-2020" ~ "2020-2029",
                            decade == "2030-2030" ~ "2030-2039",
                            TRUE ~ as.character(decade))) %>% 
  dplyr::select(country, year_data, rate_mean.CDinc, rate_mean.UCinc, UC_CD_ratio) %>% 
  base::merge(., stages_raw[,c("country", "decade","predicted_stage")], by = c("country", "decade")) %>% 
  mutate(predicted_stage = case_when(predicted_stage == "stage1" ~ "Stage 1",
                                     predicted_stage == "stage2" ~ "Stage 2",
                                     predicted_stage == "stage3" ~ "Stage 3",
                                     TRUE ~ predicted_stage))

ratio_inc_cy <- filter(ratio_cy, !is.na(UC_CD_ratio))


# Visualization
# calculate median for label
labels <- ratio_inc_ny %>% 
  group_by(predicted_stage) %>% 
  summarise(label = sprintf("%.2f", median(UC_CD_ratio)))

# generate boxplot
plot <- ratio_inc_ny %>% 
  mutate(predicted_stage = case_when(predicted_stage == "stage1" ~ "Stage 1",
                                     predicted_stage == "stage2" ~ "Stage 2",
                                     predicted_stage == "stage3" ~ "Stage 3",
                                     TRUE ~ predicted_stage)) %>% 
  plot_ly(y = ~UC_CD_ratio, color = ~predicted_stage, colors = c("#7570B3", "#D95F02", "#1B9E77"),
          type= "box",
          width = 800, height = 700,
          marker = list(size = 5, opacity = 0.6)) %>% 
  layout(legend = list(title = list(text = "Epidemiologic Stage"),
                       y = 0.5),
         xaxis = list(title = "Epidemiologic Stage",
                      showticklabels = FALSE),
         yaxis = list(title = "UC:CD Ratio",
                      type = "log",
                      range = c(-1,2.1),
                      tickformat = ".1r",
                      hoverformat = '.2f'),
         annotations = list(
           list(
             x = 0,
             y = log(as.numeric(labels[labels$predicted_stage == "Stage 1", ]$label))-log(1.65),
             text = labels[labels$predicted_stage == "Stage 1", ]$label,
             xref = "x", yref = "y",
             xanchor = 'center',
             showarrow = FALSE
           ),
           list(
             x = 1,
             y = log(as.numeric(labels[labels$predicted_stage == "Stage 2", ]$label))-log(1.32),
             text = labels[labels$predicted_stage == "Stage 2", ]$label,
             xref = "x", yref = "y",
             xanchor = 'center',
             showarrow = FALSE
           ),
           list(
             x = 2,
             y = log(as.numeric(labels[labels$predicted_stage == "Stage 3", ]$label))-log(1.16),
             text = labels[labels$predicted_stage == "Stage 3", ]$label,
             xref = "x", yref = "y",
             xanchor = 'center',
             showarrow = FALSE
           )
         ),
         margin = c(1, 1, 0, 0)) %>% 
  config(
    toImageButtonOptions = list(
      format = "png",
      filename = "ratio_boxes",
      width = 750, height = 750, scale = 5) #use this to generate .png from the tool bar
  )
plot

