#---
# title: "4-stages-IBD Annual Average Percent Change calculations"
# author: "Julia Gorospe"; "Lindsay Hracs"
# date: "2023-08-22"
# R version: 4.2.1 (2022-06-23)
# Platform: x86_64-apple-darwin17.0 (64-bit)
# Running under: macOS Monterey 12.5.1
#---

#--- 1 Script Setup

rm(list = ls(all = TRUE))
setwd("~/Dropbox/Ratio_2021 (2)") # CHANGE ME!
path <- ""                        # CHANGE ME!

# Install and load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  rio,                 # v0.5.16 import/exporting files
  tidyverse,           # v1.3.0
  broom,
  AER,
  MASS)        

# Load data
raw <- rio::import("https://raw.githubusercontent.com/kaplan-gi/4-stages-IBD/main/1_Data/GIVES21_example_data.csv")


#--- 2 Data Prep

# Filtering 
data <- raw %>% 
  filter(age_cat == "All",
         data_type == "Incidence") %>% 
  mutate(author_year = paste(author, year_study),
         id_name = paste0(study_id, ";", name)) %>% 
  group_by(id_name) %>% 
  mutate(period_st = min(period_start),
         period_nd = max(period_end)) %>% 
  mutate(period_length = (as.numeric(period_nd) - as.numeric(period_st)) + 1) %>% 
  ungroup() %>% 
  mutate(period = paste0(as.character(period_st), "-", as.character(period_nd))) %>% 
  mutate(period = ifelse(period_length != 1, period, period_nd))

inc_CD <- data %>% filter(!is.na(CD_rate))
inc_UC <- data %>% filter(!is.na(UC_IBDu_rate))


#--- 3 AAPC calculations
# poisson for equi-dispersed subsets and negative binomial for over-dispersed subsets

aapcs <- function(df, file_path) {
  # create list of study_id-name dataframes
  list <- split({{df}}, f = {{df}}$id_name)
  
  # create an empty output object
  output <- c()
  
  # conditions to calculate AAPC
  for (i in 1:length(list)) {
    
    # exclude studies with less than 3 unique years of data and a period of less than 5 years (QUANTITY)
    if (length(unique(list[[i]]$year_data)) >= 3 & list[[i]]$period_length[1] >= 5) { 
      
      # exclude studies with less than 3 years of data per 5 year span (DENSITY)
      if ((length(unique(list[[i]]$year_data)) / list[[i]]$period_length[1]) >= (3/5)) {
        
        id_name <- list[[i]]$id_name[1]
        author_year <- list[[i]]$author_year[1]
        
        # poisson regression
        mod_pois <- glm(CD_rate ~ year_data, family = poisson(link = "log"), data = list[[i]], na.action = na.exclude)
        mod_pois_coefs <- tidy(mod_pois)
        slope_pois <-  mod_pois_coefs[2,2]
        SE_pois <-  mod_pois_coefs[2,3]
        p_value_pois <- mod_pois_coefs[2,5]
        
        # goodness of fit test
        disp <- dispersiontest(mod_pois, alternative = "greater")
        disp_coefs <- tidy(disp)
        p_value_disp <- disp_coefs[1,3]
        
        if (p_value_disp >= 0.05) {
          overdisp = "no"
        } else if (p_value_disp < 0.05){
          overdisp = "yes"
        }
        
        # negative binomial regression
        mod_nb <- glm.nb(CD_rate ~ year_data, data = list[[i]], na.action = na.exclude, link = log)
        mod_nb_coefs <- tidy(mod_nb)
        slope_nb <-  mod_nb_coefs[2,2] # string needs to be removed before they can be used to calculate AAPCs...
        SE_nb <-  mod_nb_coefs[2,3]
        p_value_nb <- mod_nb_coefs[2,5]
        
        # conditionally determine AAPCs and CIs
        if (p_value_disp >= 0.05) {
          AAPC <- (exp(slope_pois) - 1)*100
          lower_CI <- (exp(slope_pois - (1.96*SE_pois)) - 1)*100
          upper_CI <- (exp(slope_pois + (1.96*SE_pois)) - 1)*100
        } else if (p_value_disp < 0.05){
          AAPC <- (exp(slope_nb) - 1)*100
          lower_CI <- (exp(slope_nb - (1.96*SE_nb)) - 1)*100
          upper_CI <- (exp(slope_nb + (1.96*SE_nb)) - 1)*100
        }
        
        # label AAPC trend
        if (overdisp == "no") {
          if (p_value_pois < 0.05 & AAPC < 0) {
            change = "decrease"
          } else if (p_value_pois < 0.05 & AAPC > 0){
            change = "increase"
          } else {
            change = "stable"
          }
        } else if (overdisp == "yes") {
          if (p_value_nb < 0.05 & AAPC < 0) {
            change = "decrease"
          } else if (p_value_nb < 0.05 & AAPC > 0){
            change = "increase"
          } else {
            change = "stable"
          }
          
        }
        
        # create dataframe with outputs
        combined <- paste0(id_name, ";", author_year, ";",
                           slope_pois, ";", SE_pois, ";", p_value_pois, ";", p_value_disp, ";", overdisp, ";",
                           slope_nb, ";", SE_nb, ";", p_value_nb, ";", AAPC, ";", lower_CI, ";", upper_CI, ";", change)
        output <- c(output, combined)
        AAPC <- data.frame(output)
        AAPC <- separate(AAPC, col = output, 
                         into = c("study_id", "name", "author_year", 
                                  "slope_pois", "SE_pois", "p_value_pois", "p_value_disp", "overdisp", 
                                  "slope_nb", "SE_nb", "p_value_nb", "AAPC", "lower_CI", "upper_CI", "change"), 
                         sep = ";")
        AAPC <- AAPC %>% 
          mutate_at(vars(slope_nb:p_value_nb), ~str_replace(., ".*year_data = ", ""))
        
        write.csv(AAPC, paste0(path, {{file_path}}), row.names = FALSE)
        
      } else {
        
      }
    }
  }
}

aapcs(inc_CD, "CD_aapc.csv")

# CHANGE 2 instances of "CD_rate" to "UC_IBDu_rate"
aapcs(inc_UC, "UC_aapc.csv")
