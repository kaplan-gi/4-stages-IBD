By: The Kaplan Global Epidemiology Lab<br>
Contact:

# The 4 Stages of IBD

This repository contains the scripts for analysis and visualization of the four epidemiologic stages of Inflammatory Bowel Disease as described in our paper *The Global Evolution of Inflammatory Bowel Disease across Four Epidemiologic Stages* (draft form - 2023/11/17).

Citation:<br>
DOI:

## Overview

Observation of patterns in the incidence and prevalence of Inflammatory Bowel Disease across time and geography led to the proposal of four epidemiologic stages. The aim of this project is to quantify the epidemiologic characteristics of each stage in order to classify regions based on their position along an epidemiologic trajectory. Understanding the progression across stages allows us to better prepare healthcare systems for the future global burden of inflammatory bowel disease.

The data for this project were compiled from a systematic review of four academic databases: MEDLINE, Embase, PubMed, and Web of Science. Collaborators from around the world reviewed the data collected for their respective regions and recommended any additional data sources. This is the most comprehensive review of IBD epidemiology literature to date.

## Contents

- Data: A subset of our data has been supplied for reproducibility, a more complete dataset is available from [kaplan-gi.shinyapps.io/GIVES21/](https://kaplan-gi.shinyapps.io/GIVES21/)
- Analysis: All scripts used for the three segments of this project are included. 
- Visualization: Scripts used to generate the figures are provided.
- Plots: Interactive or large plots that could not be included in either the body of the paper or the supplementary information file can be viewed here.

## Analyses

##### Systematic Review

Weighted mean incidence and prevalence as well as Annual Average Percent Change measures were calculated to summarize the dataset and describe the epidemiology at either a study-level or a region-level. A classic meta-analysis grouped by epidemiologic stage was performed to determine pooled values within the scope of the literature.

##### Epidemiologic Stage Classification

An iterative machine learning methedology was used to classify regions by epidemiologic stage based on the absolute incidence and prevalence in a given year as well as the change in incidence and prevalence year-to-year.

##### Forecasting Prevalence

A Partial Differential Equation was developed to model the transition from an observable stage 3 to a theoretical stage 4, allowing for prediciton of prevalence equilibrium.



