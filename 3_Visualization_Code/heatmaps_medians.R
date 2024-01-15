# title: heatmaps for incidence and prevalence data 
# author: Lindsay Hracs
# script date: 2024-01-15
# R version 4.2.2
# description: This script contains code for generating heatmaps for incidence and prevalence data using ggplot2; the central tendency measures used in plotting are median and 25th and 75th percentiles

# clear environment

rm(list = ls(all = TRUE))

# load libraries

library(ggplot2)
library(ggpubr)
library(reshape)
library(dplyr)
library(tidyverse)
library(RColorBrewer)

# load data
# data already filtered for age_cat = "All" & year_data <= 2019
# data contains mins, maxes, percentiles, and medians

inc_CD_raw <- read.csv("~/Dropbox/your/file_path/goes/here/your_CD_inc_dataset.csv", header = TRUE)
prev_CD_raw <- read.csv("~/Dropbox/your/file_path/goes/here/your_CD_prev_dataset.csv", header = TRUE)
inc_UC_raw <- read.csv("~/Dropbox/your/file_path/goes/here/your_UC_inc_dataset.csv", header = TRUE)
prev_UC_raw <- read.csv("~/Dropbox/your/file_path/goes/here/your_UC_prev_dataset.csv", header = TRUE)

# create formatted range for heatmap output

inc_CD_raw$pretty_range <- ifelse(inc_CD_raw$percentile.25. != inc_CD_raw$percentile.75., paste0(trimws(format(round(inc_CD_raw$percentile.25., digits = 2))), "–", trimws(format(round(inc_CD_raw$percentile.75., digits = 2)))), trimws(format(round(inc_CD_raw$percentile.75., digits = 2))))

prev_CD_raw$pretty_range <- ifelse(prev_CD_raw$percentile.25. != prev_CD_raw$percentile.75., paste0(trimws(format(round(prev_CD_raw$percentile.25., digits = 2))), "–", trimws(format(round(prev_CD_raw$percentile.75., digits = 2)))), trimws(format(round(prev_CD_raw$percentile.75., digits = 2))))

inc_UC_raw$pretty_range <- ifelse(inc_UC_raw$percentile.25. != inc_UC_raw$percentile.75., paste0(trimws(format(round(inc_UC_raw$percentile.25., digits = 2))), "–", trimws(format(round(inc_UC_raw$percentile.75., digits = 2)))), trimws(format(round(inc_UC_raw$percentile.75., digits = 2))))

prev_UC_raw$pretty_range <- ifelse(prev_UC_raw$percentile.25. != prev_UC_raw$percentile.75., paste0(trimws(format(round(prev_UC_raw$percentile.25., digits = 2))), "–", trimws(format(round(prev_UC_raw$percentile.75., digits = 2)))), trimws(format(round(prev_UC_raw$percentile.75., digits = 2))))

# determine ordering by most recent maximum incidence value for y-axis order

inc_CD_order <- inc_CD_raw %>%
    group_by(country) %>%
    filter(decade == max(decade)) %>%
    arrange(desc(median))
inc_CD_order$order <- 1:nrow(inc_CD_order)
inc_CD_order <- inc_CD_order[, c("country", "order")]

prev_CD_order <- prev_CD_raw %>%
    group_by(country) %>%
    filter(decade == max(decade)) %>%
    arrange(desc(median))
prev_CD_order$order <- 1:nrow(prev_CD_order)
prev_CD_order<- prev_CD_order[, c("country", "order")]

inc_UC_order <- inc_UC_raw %>%
    group_by(country) %>%
    filter(decade == max(decade)) %>%
    arrange(desc(median)) 
inc_UC_order$order <- 1:nrow(inc_UC_order)
inc_UC_order <- inc_UC_order[, c("country", "order")]

prev_UC_order <- prev_UC_raw %>%
    group_by(country) %>%
    filter(decade == max(decade)) %>%
    arrange(desc(median))
prev_UC_order$order <- 1:nrow(prev_UC_order)
prev_UC_order <- prev_UC_order[, c("country", "order")]

# join order with merged df to reorder for incidence

inc_CD <- inner_join(inc_CD_order, inc_CD_raw, by = "country", multiple = "all")
inc_CD$country <- reorder(inc_CD$country, -inc_CD$order)

prev_CD <- inner_join(prev_CD_order, prev_CD_raw, by = "country", multiple = "all")
prev_CD$country <- reorder(prev_CD$country, -prev_CD$order)

inc_UC <- inner_join(inc_UC_order, inc_UC_raw, by = "country", multiple = "all")
inc_UC$country <- reorder(inc_UC$country, -inc_UC$order)

prev_UC <- inner_join(prev_UC_order, prev_UC_raw, by = "country", multiple = "all")
prev_UC$country <- reorder(prev_UC$country, -prev_UC$order)

# create heatmap for incidence with ggplot2 
# function takes:
#   data subset (data = inc_CD | inc_UC)
#   measure of interest (fill_measure = inc_CD$median | inc_UC$meadian)
#   pretty range (range = inc_CD$pretty_range | inc_UC$pretty_range)
#   plot title 
#   low end colour in gradient (HTML or base R)
#   high end colour in gradient (HTML or base R)

plot_inc <- function(data, fill_measure, range, title, low, high) {
    
    # format x axis; duplicate sec.axis() function only works with continuous scale
    # extract levels of decade variable to use as labels for x axis
    labs <- levels(factor({data}$decade))
    # change decade to numeric for continuous axis 
    facts <- factor({data}$decade)
    fac_num <- as.numeric(facts)
        
    ggplot({data}, aes(x = fac_num, y = country)) + # use numeric factor x axis                    
        geom_tile(aes(fill = {fill_measure}), color = "black") + 
        geom_text(label = {range}, colour = "white", size = 5) + 
        ggtitle({title}) +
        labs(x = "Decade", y = "Region (by most recent median incidence)") +
        scale_fill_gradient(name = "Incidence\nper 100,000", low = {low}, high = {high}) + 
        scale_x_continuous(breaks = 1:length(labs), labels = labs, sec.axis = dup_axis(~., breaks = 1:length(labs), labels = labs), expand = c(0,0.5)) +
        scale_y_discrete(position = "right") +
        theme_bw() + 
        theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "#000000", size = 18),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 12),
              axis.title.x = element_text(face = "bold", color = "#000000", size = 16),
              axis.title.y = element_text(face = "bold", color = "#000000", size = 16),
              axis.text.x = element_text(color = "#000000", size = 12),
              axis.text.y = element_text(color = "#000000", size = 12)) + #angle=45
        theme(legend.position = c(0.1075, 0.125), legend.box.background = element_rect(colour = "black", linewidth = 1.5)) + 
        coord_cartesian(xlim = c(1, length(labs))) + # this focuses the x-axis on the range of interest
                      # clip = 'off') +   # this keeps the labels from disappearing
        theme(plot.margin = margin(0.25, 1, 0.25, 0.25, "cm")) # format margins
    
} 
plot_CD_inc <- plot_inc(inc_CD, inc_CD$median, inc_CD$pretty_range, " ", "#57F8C7", "#001E15") # call function 
plot_UC_inc <- plot_inc(inc_UC, inc_UC$median, inc_UC$pretty_range, " ", "#57F8C7", "#001E15") # call function 

# create heatmap for prevalence with ggplot2 
# function takes:
#   data subset (data = prev_CD | prev_UC)
#   measure of interest (fill_measure = prev_CD$median | prev_UC$meadian)
#   pretty range (range = prev_CD$pretty_range | prev_UC$pretty_range)
#   plot title 
#   low end colour in gradient (HTML or base R)
#   high end colour in gradient (HTML or base R)

plot_prev <- function(data, fill_measure, range, title, low, high) {
    
    # format x axis; duplicate sec.axis() function only works with continuous scale
    # extract levels of decade variable to use as labels for x axis
    labs <- levels(factor({data}$decade))
    # change decade to numeric for continuous axis 
    facts <- factor({data}$decade)
    fac_num <- as.numeric(facts)
    
    ggplot({data}, aes(x = fac_num, y = country)) +                           
        geom_tile(aes(fill = {fill_measure}), color = "black") + 
        geom_text(label = {range}, colour = "white", size = 5) + 
        ggtitle({title}) +
        labs(x = "Decade", y = "Region (by most recent median prevalence)") +
        scale_fill_gradient(name = "Prevalence\nper 100,000", low = {low}, high = {high}) + 
        # following expand argument adjusted to ensure the same size for all tiles;
        # ggplot auto-adjusting size of leftmost and rightmost tiles
        scale_x_continuous(breaks = 1:length(labs), labels = labs, sec.axis = dup_axis(~., breaks = 1:length(labs), labels = labs), expand = c(0,0.5)) +
        scale_y_discrete(position = "right") + 
        theme_bw() + 
        theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "#000000", size = 18),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 12),
              axis.title.x = element_text(face = "bold", color = "#000000", size = 16),
              axis.title.y = element_text(face = "bold", color = "#000000", size = 16),
              axis.text.x = element_text(color = "#000000", size = 12),
              axis.text.y = element_text(color = "#000000", size = 12)) + #angle=45
        theme(legend.position = c(0.1075, 0.125), legend.box.background = element_rect(colour = "black", linewidth = 1.5)) + 
        coord_cartesian(xlim = c(1, length(labs))) + # this focuses the x-axis on the range of interest
                      # clip = 'off') +   # this keeps the labels from disappearing
        theme(plot.margin = margin(0.25, 1, 0.25, 0.25, "cm"))
    
}
plot_CD_prev <- plot_prev(prev_CD, prev_CD$median, prev_CD$pretty_range, " ", "#FC9647", "#200E00") # call function
plot_UC_prev <- plot_prev(prev_UC, prev_UC$median, prev_UC$pretty_range, " ", "#FC9647", "#200E00") # call function 


# combine plots into single panelled figure

inc_prev_panel <- ggarrange(plot_CD_inc, plot_UC_inc, plot_CD_prev, plot_UC_prev,
                            ncol = 2,
                            nrow = 2,
                            labels = "AUTO",
                            font.label = list(size = 40)) # "auto" for lowercase

# save plot output

ggsave(file = "~/your/file_path/heatmap.png", device = "png", width = 9500, height = 11000, units = "px", dpi = 300)
dev.off()