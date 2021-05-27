# US hub anomaly check
library(forecast)
library(zoo)
library(dplR)
source("https://raw.githubusercontent.com/reichlab/covidData/master/R/identify_outliers.R")


methods = tibble(
  method = c("tsoutliers", "rolling_median", "loess", "filter", "weekly_extrema_loess", "weekly_extrema_loess_loo"),
  transform = c(rep("none", 4)),
)
max_iter = 10
include_outliers_by_method = TRUE


data <- filter(full_data, 
               # only accepts 1 location
               location_name == "France" & 
                 source == "JHU" &
                 target_variable == "inc_case" &
                 date >= as.Date("2021-04-01")) %>%
  # rename to accepted cols
  mutate(location = location_name,
         inc = value)
