#---
# title: "4-stages-IBD Meta-analysis"
# author: "Julia Gorospe"
# date: "2023-09-27"
# R version: 4.2.1 (2022-06-23)
# Platform: x86_64-apple-darwin17.0 (64-bit)
# Running under: macOS Monterey 12.5.1
#---

#--- 1 Script Setup

rm(list = ls(all = TRUE))
#setwd("") # CHANGE ME!

# Install and load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  rio,                 # v0.5.16 import/exporting files
  tidyverse)           # v1.3.0

# Load data
raw <- rio::import("https://raw.githubusercontent.com/kaplan-gi/4-stages-IBD/main/1_Data/GIVES21_example_data.csv")


#--- 2 Data Prep

data <- raw %>% 
  filter(age_cat == "All",
         exclude_overlap_meta == 0) %>% 
  #back-calculate
  mutate(UC_IBDu_patients = case_when(!is.na(UC_IBDu_patients) ~ UC_IBDu_patients,
                                      TRUE ~ (population_summed*UC_IBDu_rate/100000))) %>% 
  #mutate(CD_patients = case_when(!is.na(CD_patients) ~ CD_patients,
  #                               TRUE ~ (population_summed*CD_rate/100000))) %>% #causing an error with example subset??
  mutate(population_summed = case_when(!is.na(population_summed) ~ population_summed,
                                       TRUE ~ (CD_patients*100000/CD_rate))) %>% 
  mutate(population_summed = case_when(!is.na(population_summed) ~ population_summed,
                                       TRUE ~ (UC_IBDu_patients*100000/UC_IBDu_rate))) %>% 
  mutate(population_summed = round(population_summed, digits = 0)) %>% 
  # add most recent indicator
  group_by(study_id, name, data_type) %>% 
  mutate(most_recent = ifelse(year_data == max(year_data), "Most Recent", "")) %>% 
  filter(!is.na(population_summed),
         !is.na(epi_stage))  


# Prepare prevalence data
data_prev <- data %>% 
  filter(data_type == "Prevalence",
         most_recent == "Most Recent") 

# Prevalence subsets
prev_CD1 <- filter(data_prev, epi_stage == 1, !is.na(CD_patients))
prev_CD2 <- filter(data_prev, epi_stage == 2, !is.na(CD_patients))
prev_CD3 <- filter(data_prev, epi_stage == 3, !is.na(CD_patients))

prev_UC1 <- filter(data_prev, epi_stage == 1, !is.na(UC_IBDu_patients))
prev_UC2 <- filter(data_prev, epi_stage == 2, !is.na(UC_IBDu_patients))
prev_UC3 <- filter(data_prev, epi_stage == 3, !is.na(UC_IBDu_patients))


# Prepare incidence data
data_inc <- data %>% 
  filter(data_type == "Incidence") %>% 
  # calculate period
  group_by(study_id, name, epi_stage) %>% 
  mutate(period_st = min(year_data),
         period_nd = max(year_data)) %>%
  mutate(period_length = (as.numeric(period_nd) - as.numeric(period_st)) + 1) %>%
  ungroup() %>%
  mutate(period = paste0(as.character(period_st), "-", as.character(period_nd))) %>%
  mutate(period = ifelse(period_length != 1, period, period_nd)) %>%
  # aggregate
  group_by(study_id, author, year_study, country, name, epi_stage, period, period_st, period_nd) %>%
  summarise(population_summed = sum(population_summed, na.rm = FALSE),
            CD_patients = sum(CD_patients, na.rm = FALSE),
            UC_IBDu_patients = sum(UC_IBDu_patients, na.rm = FALSE)) %>%
  distinct()

# Incidence subsets
inc_CD1 <- filter(data_inc, epi_stage == 1, !is.na(CD_patients))
inc_CD2 <- filter(data_inc, epi_stage == 2, !is.na(CD_patients))
inc_CD3 <- filter(data_inc, epi_stage == 3, !is.na(CD_patients))

inc_UC1 <- filter(data_inc, epi_stage == 1, !is.na(UC_IBDu_patients))
inc_UC2 <- filter(data_inc, epi_stage == 2, !is.na(UC_IBDu_patients))
inc_UC3 <- filter(data_inc, epi_stage == 3, !is.na(UC_IBDu_patients))


#___ Meta-analysis & Forest plots

# Prevalence meta-analysis function
metaprev <- function (df, disease, events, file_name) {
  #run metarate
  meta <- metarate({{events}}, population_summed, data = {{df}}, 
                   studlab = paste(author, year_study), 
                   #byvar = decade, 
                   random = TRUE, fixed = FALSE, 
                   level = 0.95, method = "Inverse",
                   method.tau = "PM", #DL for reproducibility, PM for binary outcomes, SJ for very high heterogeneity
                   #hakn = TRUE, #use to switch to t-distribution rather than normal, controls for uncertainty of heterogeneity
                   irscale = 100000, irunit = "Persons")
  print(summary(meta))
  #print out forest plot
  png(file = {{file_name}}, width = 4000, height = 6500, res = 300)
  forest.meta(meta, sortvar = TE, bysort = TRUE,
              #xlim = c(0,max(meta$data$CD_rate)+(0.1*max(meta$data$CD_rate))),
              smlab = paste({{disease}}, "Prevalence per 100,000"),
              leftcols = c("studlab", "country", "year_data", "event", "time"),
              leftlabs = c("Study", "Region", "Year", "Cases", "Population"),
              rightcols = c("effect", "ci", "w.random"),
              rightlabs = c("Rate", "95% CI", "Weight"),
              just.addcols.left = "left",
              col.diamond = "#D95F02", col.square = "#FECEA9")
  dev.off()
}

# Run prevalence meta-analysis
metaprev(prev_CD1, "CD", prev_CD1$CD_patients, "forestplot_prev_CD_stage1.png")
metaprev(prev_CD2, "CD", prev_CD2$CD_patients, "forestplot_prev_CD_stage2.png")
metaprev(prev_CD3, "CD", prev_CD3$CD_patients, "forestplot_prev_CD_stage3.png")

metaprev(prev_UC1, "UC", prev_UC1$UC_IBDu_patients, "forestplot_prev_UC_stage1.png")
metaprev(prev_UC2, "UC", prev_UC2$UC_IBDu_patients, "forestplot_prev_UC_stage2.png")
metaprev(prev_UC3, "UC", prev_UC3$UC_IBDu_patients, "forestplot_prev_UC_stage3.png")


# Incidence meta-analysis function
metainc <- function (df, disease, events, file_name) {
  #run metarate
  meta <- metarate({{events}}, population_summed, data = {{df}}, 
                   studlab = paste(author, year_study), 
                   #byvar = decade, 
                   random = TRUE, fixed = FALSE, 
                   level = 0.95, method = "Inverse",
                   method.tau = "PM", #DL for reproducibility, PM for binary outcomes, SJ for very high heterogeneity
                   #hakn = TRUE, #use to switch to t-distribution rather than normal, controls for uncertainty of heterogeneity
                   irscale = 100000, irunit = "Persons")
  print(summary(meta))
  #print out forest plot
  png(file = {{file_name}}, width = 4000, height = 13000, res = 300)
  forest.meta(meta, sortvar = TE, bysort = TRUE,
              #xlim = c(0,max(meta$event/meta$time)+(0.1*max(meta$event/meta$time))),
              smlab = paste({{disease}},"Incidence per 100,000"),
              leftcols = c("studlab", "country", "period", "event", "time"),
              leftlabs = c("Study", "Region", "Period", "Cases", "Population"),
              rightcols = c("effect", "ci", "w.random"),
              rightlabs = c("Rate", "95% CI", "Weight"),
              just.addcols.left = "left",
              col.diamond = "#1B9E77", col.square = "#ADF1DD")
  dev.off()
}

# Run incidence meta-analysis
metainc(inc_CD1, "CD", inc_CD1$CD_patients, "forestplot_inc_CD_stage1.png")
metainc(inc_CD2, "CD", inc_CD2$CD_patients, "forestplot_inc_CD_stage2.png")
metainc(inc_CD3, "CD", inc_CD3$CD_patients, "forestplot_inc_CD_stage3.png")


metainc(inc_UC1, "UC", inc_UC1$UC_IBDu_patients, "forestplot_inc_UC_stage1.png")
metainc(inc_UC2, "UC", inc_UC2$UC_IBDu_patients, "forestplot_inc_UC_stage2.png")
metainc(inc_UC3, "UC", inc_UC3$UC_IBDu_patients, "forestplot_inc_UC_stage3.png")


