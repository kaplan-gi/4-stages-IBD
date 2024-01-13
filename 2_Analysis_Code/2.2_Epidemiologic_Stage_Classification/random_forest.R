# title: "Random Forest Classifier"
# author: "Lindsay Hracs"
# date: "2024-01-24"
# output: console, CSV
# R version: 4.2.2 (2022-10-31)
# description: This script contains code for a random forest classifier that is used to classify countries at a particular time period as IBD epidemiologic Stage 1, Stage 2, or Stage 3; code is for extension to novel data points


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
      dplyr,
      htmlwidgets,
      manipulateWidget,
      htmltools,
      RColorBrewer,
      stringr,
      class, # for knn function 
      randomForest,
      mice, 
      caret)           


########## 2. LOAD DATA ##########

# full dataset

raw <- import("~/your/source/file_path/goes/here/your_full_dataset.xlsx", guess_max = 3000)

# labelled data output from kNN supported iterative data labelling

labelled <- read.csv("~/your/source/file_path/goes/here/your_base_labelled_data.csv") # this base labelled data separates out regions in UK (i.e., England, Northern Ireland, Scotland, and Wales) and Catalonia


########## 3. DATA PREP ##########

# filter dataset

data <- raw %>%
    filter(age_cat == "All",
           year_data <= 2019 & year_data >= 1930) %>% # whole decades only
    group_by(country, name, data_type, year_data) %>%
    mutate(CD_collapsed = mean(CD_rate, na.rm = TRUE),
           UC_collapsed = mean(UC_IBDu_rate, na.rm = TRUE)) %>%
    mutate(IBD_rate = (CD_collapsed + UC_collapsed)) %>%
    distinct(country, name, data_type, year_data, .keep_all = TRUE) %>%
    dplyr::select(country, name, epi_stage2020, year_data, decade, data_type, CD = CD_collapsed, 
                  UC = UC_collapsed, IBD = IBD_rate, study_id) %>%
    ungroup() %>%
    mutate_all(~ifelse(is.nan(.), NA, .)) %>%
    select(country, name, year_data, decade, data_type, CD, UC)

# format labelled dataset

labelled <- labelled %>%
    rename("stage" = "labelled_stage") %>% 
    mutate(status = "labelled") %>%
    select(country, name, year_data, decade, data_type, CD, UC, stage, status)

# join labelled data with complete dataset

joined <- left_join(data, labelled, by = c("country", "name", "data_type", "year_data"))

# format joined dataframe

joined <- joined %>%
    rename(c("decade" = "decade.x", "CD" = "CD.x", "UC" = "UC.x", "stage_lab" = "stage", "status_lab" = "status")) %>%
    mutate(stage = ifelse(is.na(stage_lab), "unclassified", stage_lab),
           status = ifelse(is.na(status_lab), "unlabelled", status_lab)) %>%
    select(-decade.y, -CD.y, -UC.y, -stage_lab, -status_lab)

# create incidence subset with year-to-year absolute difference calculations

inc <- joined %>% 
    filter(data_type == "Incidence") %>%
    group_by(country, name, data_type) %>% # no year in group 
    arrange(year_data, .by_group = TRUE) %>%
    mutate(count = seq_len(n())) %>% # create variable with sequence per group for diff calc
    mutate(CD_diff = ifelse(count > 1, abs(CD - lag(CD)), NA),
           UC_diff = ifelse(count > 1, abs(UC - lag(UC)), NA)) %>%
    ungroup()

# create prevalence subset with year-to-year absolute difference calculations

prev <- joined %>% 
    filter(data_type == "Prevalence") %>%
    group_by(country, name, data_type) %>% # no year in group
    arrange(year_data, .by_group = TRUE) %>%
    mutate(count = seq_len(n())) %>% # create variable with sequence per group for diff calc
    mutate(CD_diff = ifelse(count > 1, abs(CD - lag(CD)), NA),
           UC_diff = ifelse(count > 1, abs(UC - lag(UC)), NA)) %>%
    ungroup()

# format incidence data frame

inc <- inc %>%
    rename("CD_inc" = "CD", "UC_inc" = "UC", "CD_diff_inc" = "CD_diff", "UC_diff_inc" = "UC_diff", "stage_inc" = "stage", "status_inc" = "status") %>%
    select(-data_type, -count)

# format incidence data frame

prev <- prev %>%
    rename("CD_prev" = "CD", "UC_prev" = "UC", "CD_diff_prev" = "CD_diff", "UC_diff_prev" = "UC_diff", "stage_prev" = "stage", "status_prev" = "status") %>%
    select(-data_type, -count)


# create wide data set because classification is on the basis of inc and prev of CD and UC
# incidence and prevalence need to be on the same row

wide <- merge(inc, prev, by = c("country", "name", "year_data"), all = TRUE)

# create a single stage column and single decade column

combined <- wide %>%
    mutate(stage = coalesce(stage_inc, stage_prev),
           status = coalesce(status_inc, status_prev),
           decade = coalesce(decade.x, decade.y),
           CD_inc = replace(CD_inc, CD_inc == 0, 0.00000000000000000001),
           UC_inc = replace(UC_inc, UC_inc == 0, 0.00000000000000000001),
           CD_prev = replace(CD_prev, CD_prev == 0, 0.00000000000000000001),
           UC_prev = replace(UC_prev, UC_prev == 0, 0.00000000000000000001),
           CD_diff_inc = replace(CD_diff_inc, CD_diff_inc == 0, 0.00000000000000000001),
           UC_diff_inc = replace(UC_diff_inc, UC_diff_inc == 0, 0.00000000000000000001),
           CD_diff_prev = replace(CD_diff_prev, CD_diff_prev == 0, 0.00000000000000000001),
           UC_diff_prev = replace(UC_diff_prev, UC_diff_prev == 0, 0.00000000000000000001),
           CD_inc_impute = "",
           UC_inc_impute = "", 
           CD_prev_impute = "",
           UC_prev_impute = "",
           CD_diff_inc_impute = "",
           UC_diff_inc_impute = "", 
           CD_diff_prev_impute = "",
           UC_diff_prev_impute = "") %>%
    select(-stage_inc, -stage_prev, -decade.x, -decade.y, -status_inc, -status_prev)

# split dataset into country-decade subsets for data imputation

data_sp <- split(combined, interaction(combined$country, combined$decade)) 

# create empty data frames for all disease x data type subsets for imputations

CDinc1 <- data.frame()
CDinc2 <- data.frame()
CDinc3 <- data.frame()
UCinc1 <- data.frame()
UCinc2 <- data.frame()
UCinc3 <- data.frame()
CDprev1 <- data.frame()
CDprev2 <- data.frame()
CDprev3 <- data.frame()
UCprev1 <- data.frame()
UCprev2 <- data.frame()
UCprev3 <- data.frame()
CDdiff_inc1 <- data.frame()
CDdiff_inc2 <- data.frame()
CDdiff_inc3 <- data.frame()
UCdiff_inc1 <- data.frame()
UCdiff_inc2 <- data.frame()
UCdiff_inc3 <- data.frame()
CDdiff_prev1 <- data.frame()
CDdiff_prev2 <- data.frame()
CDdiff_prev3 <- data.frame()
UCdiff_prev1 <- data.frame()
UCdiff_prev2 <- data.frame()
UCdiff_prev3 <- data.frame()

# impute missing data so that there are no NA values for any disease x data type subset
# incidence, prevalence, and absolute differences are imputed
# for loop to iterate over each country-decade split to impute missing values
# output of for loop is complete data frame
# if possible, predictive mean matching (pmm) is used
# --> if pmm assumptions not met, mean imputation is used
# -----> if no data available for mean calculation, zero imputation is used

for (i in 1:length(data_sp)) {
    
    if (nrow(data_sp[[i]]) > 0) {
        
        # CD inc impute
        
        # zero imput if all available data are NA
        if (length(which(is.na(data_sp[[i]]$CD_inc))) == nrow(data_sp[[i]])){
            
            data_sp[[i]]$CD_inc_impute <- ifelse(is.na(data_sp[[i]]$CD_inc), 0, data_sp[[i]]$CD_inc)
            imputed <- data_sp[[i]]
            CDinc1 <- rbind(CDinc1, imputed)
            
        } else {
            
            # try to impute with predicitve mean matching
            try_impute <- try(complete(mice(data_sp[[i]], method = "pmm", allow.na = TRUE))$CD_inc)
            
            # if pmm returns error, use mean imputation
            if ("try-error" %in% class(try_impute)){
                print("TRY-ERROR")
                data_sp[[i]]$CD_inc_impute <- ifelse(is.na(data_sp[[i]]$CD_inc), mean(data_sp[[i]]$CD_inc, na.rm = TRUE), data_sp[[i]]$CD_inc)
                imputed <- data_sp[[i]]
                CDinc2<- rbind(CDinc2, imputed)
            
            # if pmm does not return error, use pmm imputation     
            } else {

                print("NO TRY-ERROR")
                data_sp[[i]]$CD_inc_impute <- complete(mice(data_sp[[i]], method = "pmm", allow.na = TRUE))$CD_inc
                imputed <- data_sp[[i]]
                CDinc3 <- rbind(CDinc3, imputed)
            }
        }
        
        CDincimpute <- rbind(CDinc1, CDinc2, CDinc3)
        
        # UC inc impute
        
        if (length(which(is.na(data_sp[[i]]$UC_inc))) == nrow(data_sp[[i]])){
            
            data_sp[[i]]$UC_inc_impute <- ifelse(is.na(data_sp[[i]]$UC_inc), 0, data_sp[[i]]$UC_inc)
            imputed <- data_sp[[i]]
            UCinc1 <- rbind(UCinc1, imputed)
            
        } else {
            
            try_impute <- try(complete(mice(data_sp[[i]], method = "pmm", allow.na = TRUE))$UC_inc)

            if ("try-error" %in% class(try_impute)){
                print("TRY-ERROR")
                data_sp[[i]]$UC_inc_impute <- ifelse(is.na(data_sp[[i]]$UC_inc), mean(data_sp[[i]]$UC_inc, na.rm = TRUE), data_sp[[i]]$UC_inc)
                imputed <- data_sp[[i]]
                UCinc2<- rbind(UCinc2, imputed)
                 
            } else {

                print("NO TRY-ERROR")
                data_sp[[i]]$UC_inc_impute <- complete(mice(data_sp[[i]], method = "pmm", allow.na = TRUE))$UC_inc
                imputed <- data_sp[[i]]
                UCinc3 <- rbind(UCinc3, imputed)
            }
        }
        
        UCincimpute <- rbind(UCinc1, UCinc2, UCinc3)
        
        # CD prev impute
        
        if (length(which(is.na(data_sp[[i]]$CD_prev))) == nrow(data_sp[[i]])){
            
            data_sp[[i]]$CD_prev_impute <- ifelse(is.na(data_sp[[i]]$CD_prev), 0, data_sp[[i]]$CD_prev)
            imputed <- data_sp[[i]]
            CDprev1 <- rbind(CDprev1, imputed)
            
        } else {
            
            try_impute <- try(complete(mice(data_sp[[i]], method = "pmm", allow.na = TRUE))$CD_prev)

            if ("try-error" %in% class(try_impute)){
                print("TRY-ERROR")
                data_sp[[i]]$CD_prev_impute <- ifelse(is.na(data_sp[[i]]$CD_prev), mean(data_sp[[i]]$CD_prev, na.rm = TRUE), data_sp[[i]]$CD_prev)
                imputed <- data_sp[[i]]
                CDprev2<- rbind(CDprev2, imputed)
                 
            } else {

                print("NO TRY-ERROR")
                data_sp[[i]]$CD_prev_impute <- complete(mice(data_sp[[i]], method = "pmm", allow.na = TRUE))$CD_prev
                imputed <- data_sp[[i]]
                CDprev3 <- rbind(CDprev3, imputed)
            }
        }
        
        CDprevimpute <- rbind(CDprev1, CDprev2, CDprev3)
        
        # UC prev impute
        
        if (length(which(is.na(data_sp[[i]]$UC_prev))) == nrow(data_sp[[i]])){
            
            data_sp[[i]]$UC_prev_impute <- ifelse(is.na(data_sp[[i]]$UC_prev), 0, data_sp[[i]]$UC_prev)
            imputed <- data_sp[[i]]
            UCprev1 <- rbind(UCprev1, imputed)
            
        } else {
            
            try_impute <- try(complete(mice(data_sp[[i]], method = "pmm", allow.na = TRUE))$UC_prev)

            if ("try-error" %in% class(try_impute)){
                print("TRY-ERROR")
                data_sp[[i]]$UC_prev_impute <- ifelse(is.na(data_sp[[i]]$UC_prev), mean(data_sp[[i]]$UC_prev, na.rm = TRUE), data_sp[[i]]$UC_prev)
                imputed <- data_sp[[i]]
                UCprev2<- rbind(UCprev2, imputed)
                 
            } else {

                print("NO TRY-ERROR")
                data_sp[[i]]$UC_prev_impute <- complete(mice(data_sp[[i]], method = "pmm", allow.na = TRUE))$UC_prev
                imputed <- data_sp[[i]]
                UCprev3 <- rbind(UCprev3, imputed)
            }
        }
        
        UCprevimpute <- rbind(UCprev1, UCprev2, UCprev3)
        
        # CD inc diff impute
        
        if (length(which(is.na(data_sp[[i]]$CD_diff_inc))) == nrow(data_sp[[i]])){
            
            data_sp[[i]]$CD_diff_inc_impute <- ifelse(is.na(data_sp[[i]]$CD_diff_inc), 0, data_sp[[i]]$CD_diff_inc)
            imputed <- data_sp[[i]]
            CDdiff_inc1 <- rbind(CDdiff_inc1, imputed)
            
        } else {
            
            try_impute <- try(complete(mice(data_sp[[i]], method = "pmm", allow.na = TRUE))$CD_diff_inc)

            if ("try-error" %in% class(try_impute)){
                print("TRY-ERROR")
                data_sp[[i]]$CD_diff_inc_impute <- ifelse(is.na(data_sp[[i]]$CD_diff_inc), mean(data_sp[[i]]$CD_diff_inc, na.rm = TRUE), data_sp[[i]]$CD_diff_inc)
                imputed <- data_sp[[i]]
                CDinc_diff2<- rbind(CDdiff_inc2, imputed)
                 
            } else {

                print("NO TRY-ERROR")
                data_sp[[i]]$CD_diff_inc_impute <- complete(mice(data_sp[[i]], method = "pmm", allow.na = TRUE))$CD_diff_inc
                imputed <- data_sp[[i]]
                CDdiff_inc3 <- rbind(CDdiff_inc3, imputed)
            }
        }
        
        CDdiff_inc_impute <- rbind(CDdiff_inc1, CDdiff_inc2, CDdiff_inc3)
        
        # UC inc impute
        
        if (length(which(is.na(data_sp[[i]]$UC_diff_inc))) == nrow(data_sp[[i]])){
            
            data_sp[[i]]$UC_diff_inc_impute <- ifelse(is.na(data_sp[[i]]$UC_diff_inc), 0, data_sp[[i]]$UC_diff_inc)
            imputed <- data_sp[[i]]
            UCdiff_inc1 <- rbind(UCdiff_inc1, imputed)
            
        } else {
            
            try_impute <- try(complete(mice(data_sp[[i]], method = "pmm", allow.na = TRUE))$UC_diff_inc)

            if ("try-error" %in% class(try_impute)){
                print("TRY-ERROR")
                data_sp[[i]]$UC_diff_inc_impute <- ifelse(is.na(data_sp[[i]]$UC_diff_inc), mean(data_sp[[i]]$UC_diff_inc, na.rm = TRUE), data_sp[[i]]$UC_diff_inc)
                imputed <- data_sp[[i]]
                UCdiff_inc2 <- rbind(UCdiff_inc2, imputed)
                 
            } else {

                print("NO TRY-ERROR")
                data_sp[[i]]$UC_diff_inc_impute <- complete(mice(data_sp[[i]], method = "pmm", allow.na = TRUE))$UC_diff_inc
                imputed <- data_sp[[i]]
                UCdiff_inc3 <- rbind(UCdiff_inc3, imputed)
            }
        }
        
        UCdiff_inc_impute <- rbind(UCdiff_inc1, UCdiff_inc2, UCdiff_inc3)
        
        # CD prev impute
        
        if (length(which(is.na(data_sp[[i]]$CD_diff_prev))) == nrow(data_sp[[i]])){

            data_sp[[i]]$CD_diff_prev_impute <- ifelse(is.na(data_sp[[i]]$CD_diff_prev), 0, data_sp[[i]]$CD_diff_prev)
            imputed <- data_sp[[i]]
            CDdiff_prev1 <- rbind(CDdiff_prev1, imputed)
            
        } else {

            try_impute <- try(complete(mice(data_sp[[i]], method = "pmm", allow.na = TRUE))$CD_diff_prev)

            if ("try-error" %in% class(try_impute)){
                print("TRY-ERROR")
                data_sp[[i]]$CD_diff_prev_impute <- ifelse(is.na(data_sp[[i]]$CD_diff_prev), mean(data_sp[[i]]$CD_diff_prev, na.rm = TRUE), data_sp[[i]]$CD_diff_prev)
                imputed <- data_sp[[i]]
                CDdiff_prev2<- rbind(CDdiff_prev2, imputed)
                 
            } else {

                print("NO TRY-ERROR")
                data_sp[[i]]$CD_diff_prev_impute <- complete(mice(data_sp[[i]], method = "pmm", allow.na = TRUE))$CD_diff_prev
                imputed <- data_sp[[i]]
                CDdiff_prev3 <- rbind(CDdiff_prev3, imputed)
            }
        }
        
        CDdiff_prev_impute <- rbind(CDdiff_prev1, CDdiff_prev2, CDdiff_prev3)
        
        # UC prev impute
        
        if (length(which(is.na(data_sp[[i]]$UC_diff_prev))) == nrow(data_sp[[i]])){

            data_sp[[i]]$UC_diff_prev_impute <- ifelse(is.na(data_sp[[i]]$UC_diff_prev), 0, data_sp[[i]]$UC_diff_prev)
            imputed <- data_sp[[i]]
            UCdiff_prev1 <- rbind(UCdiff_prev1, imputed)
            
        } else {

            try_impute <- try(complete(mice(data_sp[[i]], method = "pmm", allow.na = TRUE))$UC_diff_prev)

            if ("try-error" %in% class(try_impute)){
                print("TRY-ERROR")
                data_sp[[i]]$UC_diff_prev_impute <- ifelse(is.na(data_sp[[i]]$UC_diff_prev), mean(data_sp[[i]]$UC_diff_prev, na.rm = TRUE), data_sp[[i]]$UC_diff_prev)
                imputed <- data_sp[[i]]
                UCdiff_prev2<- rbind(UCdiff_prev2, imputed)
                 
            } else {

                print("NO TRY-ERROR")
                data_sp[[i]]$UC_diff_prev_impute <- complete(mice(data_sp[[i]], method = "pmm", allow.na = TRUE))$UC_diff_prev
                imputed <- data_sp[[i]]
                UCdiff_prev3 <- rbind(UCdiff_prev3, imputed)
            }
        }
        
        UCdiff_prev_impute <- rbind(UCdiff_prev1, UCdiff_prev2, UCdiff_prev3)
       
    } else {
    
    }
    
} 

# rename imputed data frame

imputed_full <- UCdiff_prev_impute

# zero-impute any remaining NAs in feature columns

imputed_full <- imputed_full %>%
    mutate_at(c("CD_inc_impute", "UC_inc_impute", "CD_prev_impute", "UC_prev_impute", 
                "CD_diff_inc_impute", "UC_diff_inc_impute", "CD_diff_prev_impute", "UC_diff_prev_impute"), 
              ~replace_na(., 0))

# create training/validation data set

train_validate <- imputed_full %>%
    filter(status == "labelled")

train_validate_labels <- train_validate %>% 
    select(stage)

train_validate_context <- train_validate %>%
    select(-stage)

# create test (i.e., unclassified) data set
# stage classifications to be assigned with model fit using training and validation data

test <- imputed_full %>%
    filter(stage == "unclassified")
test$stage <- as.factor(test$stage)

test_labels <- test %>% 
    select(stage)

test_context <- test %>%
    select(-stage)

# create training and validation sets and train model

set.seed(1234)

sample_index <- sample(nrow(train_validate), round(nrow(train_validate)*.8), replace = FALSE)

train <- train_validate[sample_index, ]
train$stage <- as.factor(train$stage)

validation <- train_validate[-sample_index, ] # all the rows not given a sample index
validation$stage <- as.factor(validation$stage)

train_labels <- train_validate_labels[sample_index, ] # may need to wrap in as.factor for non-dataframe
validation_labels <- train_validate_labels[-sample_index, ]

train_context <- train_validate_context[sample_index, ] # may need to wrap in as.factor for non-dataframe
validation_context <- train_validate_context[-sample_index, ]

# create object rf with random forest model

rf <- randomForest(formula = stage ~ CD_inc_impute + CD_prev_impute + UC_inc_impute + UC_prev_impute + CD_diff_inc_impute + CD_diff_prev_impute + UC_diff_inc_impute + UC_diff_prev_impute, 
                   data = train, 
                   ntree = 1000, 
                   na.action = na.omit, 
                   mtry = 4, 
                   importance = TRUE)
rf

# extract feature importance measures from model

rf_imp <- rf$importance

# importance measures for each stage

rf_imp_stage1 <- rf_imp[,1]
rf_imp_stage2 <- rf_imp[,1]
rf_imp_stage3 <- rf_imp[,1]

# rudimentary visualization of importance measures

dotchart(rf_imp_stage1)

# get classifications for each tree
# NOTE: the following line is imcomplete and needs to be developed
# getTree(rf, k = 1, labelVar = TRUE)

# random forest model stage prediction for validation data

validation_prediction <- predict(rf, validation[,15:22])

validation_rf_results <- confusionMatrix(data = validation_prediction, reference = as.factor(validation$stage))

validation_tabulation <- table(validation_prediction, as.factor(validation$stage))

# random forest model stage prediction for test (i.e., unclassified) data

test_prediction <- data.frame(predict(rf, test[,15:22]))

# create data fram with test data and corresponding stage predictions

test_complete <- data.frame(test, test_prediction)

names(test_complete)[23] <- "predicted_stage" # rename predicted stage column

train_validate <- train_validate %>%
    mutate("predicted_stage" = stage)

# combine training, validation, and test data

output <- rbind(train_validate, test_complete)

# write.csv(output, "~/your/output/file_path/goes/here/annual_random_forest_results_output_date.csv") # leave line commented out when finished with script