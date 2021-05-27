library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)

# Get data from all sources 
#  as a complete grid by date/location/source/target

source("R/get-euro-data.R")
grid <- get_euro_data()

# Summarise missing ------------------------------------------------------
# Missing dates/locations as a % all dates/locations
missing <- grid %>%
  group_by(target_variable, source) %>%
  summarise(
    # Count all missing dates/locations
    missing = sum(is.na(value)) / n() * 100,
    # Truncation - count missing in last week
    miss_last_week = sum((date >= (max(days) - 7) & is.na(value)), 
                                 na.rm = TRUE) / n() * 100,
    # Count negative values
    negative = sum(value < 0, na.rm = TRUE) / n() * 100,
    .groups = "drop")  
