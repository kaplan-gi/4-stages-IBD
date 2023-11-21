#---
# title: "4-stages-IBD Weighted Means"
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
  tidyverse)           # v1.3.0

# Load data
raw <- rio::import("https://raw.githubusercontent.com/kaplan-gi/4-stages-IBD/main/1_Data/GIVES21_example_data.csv")


#--- 2 Data Prep

# Filtering and back calculations
data <- raw %>% 
  filter(age_cat == "All") %>% 
  mutate(author_year = paste(author, year_study),
         id_name = paste0(study_id, ";", name)) %>% 
  
  # back calculate missing case counts and populations
  mutate(UC_IBDu_patients = case_when(!is.na(UC_IBDu_patients) ~ UC_IBDu_patients,
                                      TRUE ~ (population_summed*UC_IBDu_rate/100000)),
         CD_patients = case_when(!is.na(CD_patients) ~ CD_patients,
                                 TRUE ~ (population_summed*CD_rate/100000))) %>%
  mutate(population_summed = case_when(!is.na(population_summed) ~ population_summed,
                                       TRUE ~ (CD_patients*100000/CD_rate))) %>% 
  mutate(population_summed = case_when(!is.na(population_summed) ~ population_summed,
                                       TRUE ~ (UC_IBDu_patients*100000/UC_IBDu_rate))) %>% 
  
  # create weighted columns
  mutate(CD_rate_wt = CD_rate * population_summed,
         UC_rate_wt = UC_IBDu_rate * population_summed)

# Incidence data
inc_data <- data %>% 
  filter(data_type == "Incidence") %>% 
  group_by(id_name) %>% 
  mutate(period_st = min(period_start),
         period_nd = max(period_end)) %>% 
  mutate(period_length = (as.numeric(period_nd) - as.numeric(period_st)) + 1) %>% 
  ungroup() %>% 
  mutate(period = paste0(as.character(period_st), "-", as.character(period_nd))) %>% 
  mutate(period = ifelse(period_length != 1, period, period_nd))

inc_CD <- inc_data %>% filter(!is.na(CD_rate),
                              !is.na(period_length))
inc_UC <- inc_data %>% filter(!is.na(UC_IBDu_rate),
                              !is.na(period_length))

# Prevalence data
prev_data <- data %>% 
  filter(data_type == "Prevalence") %>% 
  group_by(id_name) %>% 
  mutate(period_st = min(year_data),
         period_nd = max(year_data)) %>% 
  mutate(period_length = (as.numeric(period_nd) - as.numeric(period_st)) + 1) %>% 
  ungroup() %>% 
  mutate(period = paste0(as.character(period_st), "-", as.character(period_nd))) %>% 
  mutate(period = ifelse(period_length != 1, period, period_nd))

prev_CD <- prev_data %>% filter(!is.na(CD_rate))
prev_UC <- prev_data %>% filter(!is.na(UC_IBDu_rate))


#--- 3 Weighted Means

# Weighted means function
weighted_means <- function(df, rate_type, rate_wt, file_path) {
  # mean rate                
  means <- aggregate(data = {{df}}, {{rate_type}}~id_name+author_year, mean)
  names(means)[3] <- "rate_mean"
  # sum of weighted rate (numerator)
  sums <- aggregate(data = {{df}}, {{rate_wt}}~id_name+author_year, sum)
  names(sums)[3] <- "rate_wt_sum"
  
  sums_means <- merge(sums, means, all.y = TRUE)
  
  # sum of weights (denominator)
  pop <- aggregate(data = {{df}}, population_summed~id_name+author_year, sum, na.rm = FALSE, na.action = NULL)
  
  # create df with numerators and denominators
  complete <- merge(sums_means, pop, all.x = TRUE, all.y = FALSE) # only merge pop if there exists a summed rate
  
  wm <- complete %>% 
    rowwise() %>% 
    mutate(weighted_mean = ifelse(is.na(rate_wt_sum) | is.na(population_summed), rate_mean, (rate_wt_sum/population_summed)))
  
  write.csv(wm, {{file_path}}, row.names = FALSE) 
} 


# Incidence weighted means
weighted_means(inc_CD, inc_CD$CD_rate, inc_CD$CD_rate_wt, paste0(path, "CD_wmI_", Sys.Date(), ".csv"))
weighted_means(inc_UC, inc_UC$UC_IBDu_rate, inc_UC$UC_rate_wt, paste0(path, "UC_wmI_", Sys.Date(), ".csv"))

inc_CD_wm <- read.csv(paste0(path, "CD_wmI_2023-11-17.csv"), header = T)    # CHANGE DATE
inc_UC_wm <- read.csv(paste0(path, "UC_wmI_2023-11-17.csv"), header = T)    # CHANGE DATE
inc_wm <- merge(inc_CD_wm, inc_UC_wm, by = "id_name", all.x = TRUE, all.y = TRUE)
colnames(inc_wm) <- gsub("\\.x", ".CD", colnames(inc_wm))
colnames(inc_wm) <- gsub("\\.y", ".UC", colnames(inc_wm))
inc_wm <- inc_wm %>% 
  separate(col = id_name, sep = ";", into = c("study_id", "name")) %>% 
  mutate(author_year = case_when(is.na(author_year.CD) ~ author_year.UC,
                                 is.na(author_year.UC) ~ author_year.CD,
                                 TRUE ~ author_year.UC)) %>% 
  select(study_id, name, author_year, everything(),-author_year.CD, -author_year.UC)


# Prevalence weighted means
weighted_means(prev_CD, prev_CD$CD_rate, prev_CD$CD_rate_wt, paste0(path, "CD_wmP_", Sys.Date(), ".csv"))
weighted_means(prev_UC, prev_UC$UC_IBDu_rate, prev_UC$UC_rate_wt, paste0(path, "UC_wmP_", Sys.Date(), ".csv"))

prev_CD_wm <- read.csv(paste0(path, "CD_wmP_2023-11-17.csv"), header = T)    # CHANGE DATE
prev_UC_wm <- read.csv(paste0(path, "UC_wmP_2023-11-07.csv"), header = T)    # CHANGE DATE
prev_wm <- merge(prev_CD_wm, prev_UC_wm, by = "id_name", all.x = TRUE, all.y = TRUE)
colnames(prev_wm) <- gsub("\\.x", ".CD", colnames(prev_wm))
colnames(prev_wm) <- gsub("\\.y", ".UC", colnames(prev_wm))
prev_wm <- prev_wm %>% 
  separate(col = id_name, sep = ";", into = c("study_id", "name")) %>% 
  mutate(author_year = case_when(is.na(author_year.CD) ~ author_year.UC,
                                 is.na(author_year.UC) ~ author_year.CD,
                                 TRUE ~ author_year.UC)) %>% 
  select(study_id, name, author_year, everything(),-author_year.CD, -author_year.UC)


