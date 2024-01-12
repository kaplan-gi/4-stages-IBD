By: The Kaplan Global Epidemiology Lab<br>
Contact: kaplan [dot] lab [at] ucalgary [dot] ca

# The 4 Stages of IBD

This repository contains the scripts for analysis and visualization of the four epidemiologic stages of inflammatory bowel disease as described in our paper *The Global Evolution of Inflammatory Bowel Disease across Four Epidemiologic Stages* (submitted - 2024/01/08).

Citation:<br>
DOI:

## Overview

Observation of patterns in the incidence and prevalence of inflammatory bowel disease (IBD) across time and geography led to the proposal of four epidemiologic stages. The aim of this project is to quantify the epidemiologic characteristics of each stage in order to classify regions based on their position along an epidemiologic trajectory. Understanding the progression across stages allows us to better prepare healthcare systems for the future global burden of IBD.

The data for this project were compiled from a systematic review of four academic databases: MEDLINE, Embase, PubMed, and Web of Science. Collaborators from around the world reviewed the data collected for their respective regions and recommended any additional data sources. This is the most comprehensive review of IBD epidemiology literature to date.

## Contents

- Data: Synthetic example data has been supplied for reproducibility, more complete datasets will be available upon publication.
- Analysis: All scripts used for the three segments of this project are included. 
- Visualization: Scripts used to generate the figures are provided.
- Plots: Interactive or large plots that could not be included in either the body of the paper or the supplementary information file can be viewed here.

## Analyses

##### Systematic Review

Weighted mean incidence and prevalence as well as Annual Average Percent Change (AAPC) measures were calculated to summarize the dataset and describe the epidemiology at either a study-level or a region-level. A classic meta-analysis grouped by epidemiologic stage was performed to determine pooled rates across the available studies.<br>
    ***Data:*** *GIVES21_example_data.csv, a 10% random sample of our systematic review data can be used to run the AAPCs.R, meta-analysis.R, and weighted_means.R scripts.*

##### Epidemiologic Stage Classification

An iterative machine learning methedology was used to classify regions by epidemiologic stage based on the absolute incidence and prevalence in a given year as well as the change in incidence and prevalence year-to-year.

##### Forecasting Stage 4 Prevalence

A Partial Differential Equation (PDE) was developed to model the transition from an observable stage 3 to a theoretical stage 4, allowing for prediction of prevalence equilibrium in three model regions. The model relies on an observed incidence and prevalence point as well as time-dependent age-stuctured population values.<br>
    ***Data:*** *synthetic_example_data_PDE_input.csv, an example of data with similar structure to the data used in our stage 4 model corresponding to the Stage3_4_IBD_prevalence.nb file.*<br>
    ***Note:*** *The analysis for this section was performed with Wolfram Mathimatica and is contained in a .nb file which can be viewed with the free Wolfram Player application available from [www.wolfram.com](https://www.wolfram.com/player/).*



