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


# Test for association between stage and UC:CD ratio
# check for distribution
mean_cy = round(mean(ratio_inc_cy$UC_CD_ratio), digits = 2)
var_cy = round(var(ratio_inc_cy$UC_CD_ratio), digits = 2)

# check for overdispersion
## overdispersed if variance is much larger than a mean for poisson
check_overdispersion(glm(UC_CD_ratio ~ predicted_stage, family = poisson, data = ratio_inc_cy))

# plot distribution
ggplot(ratio_inc_cy, aes(x = UC_CD_ratio)) + geom_histogram(bins = 25) + labs(title = "UC:CD Ratio Distribution\n(country-year_data)", x = "UC:CD Ratio", y = "Count of Observations") +
  annotate("text", x = c(40,40, 40), y = c(300, 275, 225), label = c(paste0("mean = ", mean_cy), paste0("var = ", var_cy), "OVERDISPERSED!")) + theme_bw()


# check model selection by testing model fit
set.seed(5678)
# negative binomial regression model
summary(nb_inc <- glm.nb(UC_CD_ratio ~ predicted_stage, data = ratio_inc_cy))
pchisq(nb_inc$deviance, nb_inc$df.residual)
## residual deviance lower than the degrees of freedom indicates acceptable fit of negative binomial model
  
# print out model results
(nb_results_inc <- round(cbind(`% reduct` = (1-(exp(coef(nb_inc))))*100,
                               RR = exp(coef(nb_inc)), 
                               exp(confint(nb_inc))), digits = 2) %>% 
    cbind(., pval = summary(nb_inc)$coefficients[,"Pr(>|z|)"]))
## a statistically significant reduction in UC:CD ratio in stage 2 and 3 when compared to stage 1 is observed

# em means + pairwise contrasts
emmeans(nb_inc, specs = pairwise ~ predicted_stage, type = "response")
## small p-values for pairwise contrasts indicate significant differences in UC:CD ratio between stages



