# title: "kNN with Monte Carlo sampling"
# author: "Lindsay Hracs"
# date: "2024-01-12"
# output: console, CSV
# R version: 4.2.2 (2022-10-31)
# description: This script contains code for a kNN classifier that is used in classifying regions as epidemiologic stage 1, 2, or 3 to be used in interative data labelling. The kNN model classifies each disease and data type subset (i.e., CD inc, CD prev, UC inc, UC prev) separately.


########## 1. SCRIPT SETUP ##########

# clear environment

rm(list = ls(all = TRUE))

# ensure pacman is installed

if (!require("pacman")) install.packages("pacman")

# install and load packages

pacman::p_load(
      rio,                 
      openxlsx,
      tidyverse,           
      ggpubr,
      plotly,
      htmlwidgets,
      manipulateWidget,
      htmltools,
      RColorBrewer,
      stringr,
      class) # for knn function       


########## 2. LOAD DATA ##########

# full dataset

raw <- import("~/your/source/file_path/goes/here/your_full_dataset.xlsx", guess_max = 3000) # full dataset

# manual labelled data to be fine-tuned using knn

labelled <- read.csv("~/your/source/file_path/goes/here/your_base_labelled_data.csv") 


########## 3. DATA PREP ##########

# filter dataset

data <- raw %>%
    filter(age_cat == "All",
           year_data <= 2019 & year_data >= 1930) %>%
    mutate(country = str_replace_all(country, c("England" = "United Kingdom", "Scotland" = "United Kingdom", "Wales" = "United Kingdom", "Northern Ireland" = "United Kingdom"))) %>%
    group_by(country, name, data_type, year_data) %>%
    mutate(CD_collapsed = mean(CD_rate, na.rm = TRUE),
           UC_collapsed = mean(UC_IBDu_rate, na.rm = TRUE)) %>%
    mutate(IBD_rate = (CD_collapsed + UC_collapsed)) %>%
    distinct(country, name, data_type, year_data, .keep_all = TRUE) %>%
    select(country, name, epi_stage2020, year_data, decade, data_type, # removed population_summed and metadata because no NAs allowed in models
           CD = CD_collapsed, UC = UC_collapsed, IBD = IBD_rate) %>%
    ungroup() %>%
    mutate_all(~ifelse(is.nan(.), NA, .)) %>%
    mutate(period = case_when(between(year_data, 1930, 1959) ~ "1930-1959",
                              between(year_data, 1960, 1989) ~ "1960-1989",
                              year_data >= 1990 ~ "1990-2019",
                              TRUE ~ "NA"),
           indust = "indust",
           stage = "raw_data",
           index = row_number())

# format labelled dataset

labelled <- labelled %>%
     rename("stage" = "labelled_stage") 

cols <- colnames(data)

labelled <- labelled %>%
    select(one_of(cols)) %>%
    mutate(status = "labelled")

labelled_countries <- unique(labelled$country)

# create unlabelled dataset

extract <- c()

for (row in 1:nrow(data)) {
    if (!data$country[[row]] %in% labelled_countries){
        print(data$country[[row]])
        combined <- c(data[[row,1]], data[[row,2]], data[[row,3]], data[[row,4]], data[[row,5]], data[[row,6]], data[[row,7]], data[[row,8]], data[[row,9]], data[[row,10]], data[[row,11]], data[[row,12]], data[[row,13]])
        extract <- rbind(extract, combined)
    } else {
        print("Already here!")
    }
}    

unlabelled <- data.frame(extract)

# format unlabelled dataset

row.names(unlabelled) <- seq_along(1:nrow(unlabelled))
colnames(unlabelled) <- c("country","name", "epi_stage2020", "year_data","decade", "data_type", "CD", "UC", "IBD", "period", "indust", "stage", "index")
unlabelled <- unlabelled %>%
    mutate(status = "unlabelled")

# check training/test split
# should be approx. 80/20 training/test split

nrow(data)
nrow(labelled)
nrow(unlabelled)
nrow(labelled) / nrow(data)
nrow(unlabelled) / nrow(data) # 80/20 training/test split

# combine labelled and unlabelled data

full <- rbind(labelled, unlabelled)

# subset full dataset by disease

full$CD <- as.numeric(full$CD)
full$UC <- as.numeric(full$UC)
full$IBD <- as.numeric(full$IBD)

# create inc subsets

inc <- full %>%
    filter(data_type == "Incidence") %>%
    group_by(country, name, data_type) %>% # no year in group
    arrange(year_data, .by_group = TRUE) %>%
    mutate(count = seq_len(n())) %>% # create variable with sequence per group
    mutate(CD_diff = ifelse(count > 1, abs(CD - lag(CD)), NA),
           UC_diff = ifelse(count > 1, abs(UC - lag(UC)), NA),
           IBD_diff = ifelse(count > 1, abs(IBD - lag(IBD)), NA)) %>%# 0 seems to perform better than NA
    ungroup()

# create prev subset

prev <- full %>%
    filter(data_type == "Prevalence") %>%
    group_by(country, name, data_type) %>% # no year in group
    arrange(year_data, .by_group = TRUE) %>%
    mutate(count = seq_len(n())) %>% # create variable with sequence per group
    mutate(CD_diff = ifelse(count > 1, abs(CD - lag(CD)), NA),
           UC_diff = ifelse(count > 1, abs(UC - lag(UC)), NA),
           IBD_diff = ifelse(count > 1, abs(IBD - lag(IBD)), NA)) %>% # 0 seems to perform better than
    ungroup()


##### 4. kNN CLASSIFIER #####

# data norming using min-max
# sensitivity analysis showed normed data does not significantly affect results
# normed data not used in classification but included here for documentation purposes

# min/max normalize function

normalize <- function(x) {
     return((x - min(na.omit(x))) / (max(na.omit(x)) - min(na.omit(x))))
}

# min/max calculations for inc

inc_norm <- inc %>%
    mutate(UC_norm = ifelse(!is.na(UC), normalize(UC), NA)) %>%
    mutate(CD_norm = ifelse(!is.na(CD), normalize(CD), NA)) %>%
    mutate(IBD_norm = ifelse(!is.na(IBD), normalize(IBD), NA)) %>%
    mutate_all(~ifelse(is.nan(.), NA, .))

# min/max calculations for prev

prev_norm <- prev %>%
    mutate(UC_norm = ifelse(!is.na(UC), normalize(UC), NA)) %>%
    mutate(CD_norm = ifelse(!is.na(CD), normalize(CD), NA)) %>%
    mutate(IBD_norm = ifelse(!is.na(IBD), normalize(IBD), NA)) %>%
    mutate_all(~ifelse(is.nan(.), NA, .))

# combine normed inc and prev

normed <- rbind(inc_norm, prev_norm)

# split off class labels
# NOTE: you are classifying all years/decades/periods

# create CD inc subset for kNN input
CD_inc_knn <- inc_norm %>%
    filter(!is.na(CD), 
           !is.na(CD_diff),
           status != "unlabelled") %>% # filter out unlabelled data 
    select(-IBD, -UC, -IBD_norm, -UC_norm, -IBD_diff, -UC_diff)

# create CD prev subset for kNN input
CD_prev_knn <- prev_norm %>%
    filter(!is.na(CD), 
           !is.na(CD_diff),
           status != "unlabelled") %>% # filter out unlabelled data
    select(-IBD, -UC, -IBD_norm, -UC_norm, -IBD_diff, -UC_diff)

# create UC inc subset for kNN input
UC_inc_knn <- inc_norm %>%
    filter(!is.na(UC), 
           !is.na(UC_diff),
           status != "unlabelled") %>% # filter out unlabelled data
    select(-IBD, -CD, -IBD_norm, -CD_norm, -IBD_diff, -CD_diff)

# create UC prev subset for kNN input
UC_prev_knn <- prev_norm %>%
    filter(!is.na(UC), 
           !is.na(UC_diff),
           status != "unlabelled") %>% # filter out unlabelled data
    select(-IBD, -CD, -IBD_norm, -CD_norm, -IBD_diff, -CD_diff)


# function to run kNN classifier with Monte Carlo sampling
# data_subset = {CD_inc_knn, CD_prev_knn, UC_inc_knn, UC_prev_knn}
# disease = {CD, UC}
# disease_diff = {CD_diff, UC_diff}
# file_path = "file/output/location/your_file.csv"

knn_mc <- function(data_subset, disease, disease_diff, file_path) {

    labels <- {data_subset} %>%
        select(stage)

    context <- {data_subset} %>%
        select(-stage)

    n1 = round(nrow({data_subset})*0.75) # training set sample size
    n2 = round(nrow({data_subset}) - round(nrow({data_subset})*0.75)) # testing set sample size
    n = nrow({data_subset}); # the total sample size

    set.seed(8675309)

    # initialize the ERROR values for all models in all $B=1000$ loops
    L = 1000 # number of loops
    ERROR = NULL # final ERROR values
    results = NULL # place for results to go

    for (l in 1:L){
    # randomly select n1 observations as a new training subset in each loop

        flag <- sort(sample(1:n, n1))
        train <- {data_subset}[flag,]
        test <- {data_subset}[-flag,]

        labelstrainset <- labels[flag, ]
        labelstestset <- labels[-flag, ]

        contexttrainset <- context[flag, ] # may need to wrap in as.factor for non-dataframe
        contexttestset <- context[-flag, ]

        k_mc <- 40 # round(sqrt(nrow(traintempset))) # start with k = square root of number of training examples in data

        pred <- knn(
            train = data.frame(train[, disease], train[, disease_diff]),
            test = data.frame(test[, disease], test[, disease]),
            cl = labelstrainset$stage, # need to specify column even though singleton
            k = k_mc
        )
        ERROR <- mean(pred != labelstestset$stage) # opposite of accuracy

        # determine number of errors per sample and output results
        results <- rbind(results, data.frame(contexttestset[, 1:length(contexttestset)], "stage_class" = labelstestset$stage, "stage_predict" = pred, "misclass" = ifelse(pred == labelstestset$stage, 0, 1)))

    }

    ERROR

    write.csv(results, {file_path}, row.names = FALSE)

}

# call knn_mc function with data subset and file path
# comment/uncomment the following line to run function and write the csv
# knn_mc(CD_inc_knn, "CD", "CD_diff", "~/your/output/file_path/goes/here/CD_inc_knn_results_date.csv") # leave line commented out when finished with script


########## 5. EXPLORE kNN OUTPUT ##########

# load in kNN results generated above

CD_inc_knn_results <- read.csv("~/your/source/file_path/goes/here/CD_inc_knn_results_date.csv")
CD_prev_knn_results <- read.csv("~/your/source/file_path/goes/here/CD_prev_knn_results_date.csv")
UC_inc_knn_results <- read.csv("~/your/source/file_path/goes/here/UC_inc_knn_results_date.csv")
UC_prev_knn_results <- read.csv("~/your/source/file_path/goes/here/UC_prev_knn_results_date.csv")

# change out the results file in the below code to view and generate misclassification results
# determine number of unique groups

groups <- CD_inc_knn_results %>% 
    group_by(country, name, year_data) %>%
    group_keys()

# calculate output values for sample analysis

samples <- CD_inc_knn_results %>%
    aggregate(misclass~country+name+year_data, sum)

# calculate accuracy by sample

L = 1000 # number of Monte Carlo sampling interations
samples <- samples %>%
    mutate(accuracy = 100*(1 - (misclass / L)))

# subset all country/name/year_data groupings with less than 85% accuracy

less <- subset(samples, accuracy < 85.0)

# count number of stage predictions per country per decade

counts <- CD_inc_knn_results %>%
    group_by(country, name, decade) %>%
    count(stage_predict) %>%
    mutate(max_year = as.numeric(strsplit(decade, "-")[[1]][[2]]))

# join sample analysis df with original df

joined <- merge(less, distinct(CD_inc_knn_results), by = c("country", "name", "year_data"))
misclass <- subset(joined_CD, misclass.y == 1)
# write.csv(misclass, "~/your/output/file_path/goes/here/inc_misclass_CD.csv", row.names = FALSE) # leave line commented out when finished with script