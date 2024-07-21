# Event study model

library(fixest)
library(tidyverse)
library(clipr)
library(broom)
library(magrittr)
library(stargazer)
library(xtable)
library(ggplot2)

# Main dataset format
# source - source of FDI, ISO3 code
# target - destination of FDI, ISO3 code
# year - calendar year
# fdi - in nominal millions of USD
# bri = 1 if target country is part of BRI, 0 otherwise
# bri_year - the year the target country joined BRI, should be NA if bri is 0

# Load main dataset
data <- read_csv('../Data/fdi_and_obor.csv')

# Add inverse hyperbolic sine
data$ihs_fdi <- log(data$fdi + sqrt(data$fdi^2 + 1))

# Keep only complete rows, and only BRI countries
data <- data[!is.na(data$fdi) & !is.na(data$bri_year) & data$bri == 1,]

# Add fixed effects
data$source_year <- paste0(data$source, '_', data$year)
data$target_year <- paste0(data$target, '_', data$year)
data$source_target <- paste0(data$source, '_', data$target)
data$source_year <- as.factor(data$source_year)
data$target_year <- as.factor(data$target_year)
data$source_target <- as.factor(data$source_target)

# Add source country indicators
data$chn <- ifelse(data$source == 'CHN', 1, 0)
data$usa <- ifelse(data$source == 'USA', 1, 0)
data$jpn <- ifelse(data$source == 'JPN', 1, 0)
data$ind <- ifelse(data$source == 'IND', 1, 0)

# The below steps follow the steps described in A and S's paper

# Step 1: Estimate Cohort Average Treatment on the Treated, for each cohort/relative year pair
# This step estimates the unaggregated event study model
# Uses sunab function to create cohort/relative year variables, 
# Below it is set up to bin relative years -12 to -5, and 5 to 8
# The bins can only be changed manually for some reason
# model contains interactions with each of the four source country indicators
# Fixed effects are source_year, target_year, and source_target

# Event study model
model <- feols(ihs_fdi ~ chn:sunab(bri_year, year, no_agg = TRUE, bin.rel = list('-5' = -12:-5, '5' = 5:8)) + 
                         usa:sunab(bri_year, year, no_agg = TRUE, bin.rel = list('-5' = -12:-5, '5' = 5:8)) +
                         jpn:sunab(bri_year, year, no_agg = TRUE, bin.rel = list('-5' = -12:-5, '5' = 5:8)) +
                         ind:sunab(bri_year, year, no_agg = TRUE, bin.rel = list('-5' = -12:-5, '5' = 5:8)) |
                         source_year + target_year + source_target, data = data)

# The unaggregated variables have two name formats:
#   1. [country_code]:year::[relative_year]:cohort::[cohort_year]
#          for example: chn:year::-5:cohort::2014
#   2. year::[relative_year]:cohort::[cohort_year]:[country_code]
#          for example: year::-3:cohort::2020:usa

# So sometimes the country code is at the beginning, and sometimes it is at the end. 
# In most of my tests, the first country uses pattern 1, and all others use pattern 2.
# I usually just kept China as the first country, so I would know which pattern was used.

# Step 2: Estimate cohort weights to be used when aggregating across cohorts
# This step is taken care of automatically by the aggregate function

# Step 3: Aggregate estimates across cohorts
# plot_aggregated_coefficients takes as input the unaggregated model and an ISO3
# country code. It plots a graph without returning anything.
# The variables are aggregated across cohorts using the aggregate function
# This function uses regular expressions to match variables
# When the input country code is 'chn', the function uses pattern 1,
# for other country codes it uses patter 2. 
# Variables that do not match the expression are discarded.
# The parts of the regular expression to be retained are put in parentheses.
# For the parts that are not in parentheses, variables matching the pattern are
# are aggregated based on a weighted average.

# Plot coefficients aggregated across cohorts, for each country
plot_aggregated_coefficients <- function(model, c) {
  #n <- c('jpn' = 'Japan', 'usa' = 'US', 'ind' = 'India', 'chn' = 'China')
  if(c == 'chn') {
    ac <- aggregate(model, '(chn:year::-?[[:digit:]]+):cohort::[[:digit:]]+')
  }
  else {
    ac <- aggregate(model, paste0('(year::-?[[:digit:]]+):cohort::[[:digit:]]+:(', c, ')'))
  }
  x <- c(-5:-2, 0:5)
  qplot(x, ac[,1]) + 
    theme_bw() +
    theme(panel.grid.minor = element_blank()) +
    geom_hline(yintercept = 0) + 
    scale_x_continuous(breaks = c(-5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5),
                       labels = c('< -4', -4, -3, -2, -1, 0, 1, 2, 3, 4, '> 4')) +
    xlab('Years from signing BRI Memorandum') +
    ylab('Estimate and 95% Confidence Interval') +
    geom_errorbar(aes(x = x, ymin = ac[,1] - ac[,2]*1.96, ymax = ac[,1] + ac[,2]*1.96), width = 0.1)
}


plot_aggregated_coefficients(model, 'usa')
