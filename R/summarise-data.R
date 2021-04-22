library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)

# Get data from all sources 
#  as a complete grid by date/location/source/target
source("R/get-data.R")

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

# Summarise weekly ----------------------------------------------------
grid_weekly <- grid %>%
  mutate(epiweek = epiweek(date),
         year = epiyear(date)) %>%
  group_by(epiweek, year, location_name, target_variable, source) %>%
  mutate(value = replace_na(value, 0)) %>%
  summarise(value_weekly = sum(value),
            date = max(date),
            n = n(),
            .groups = "drop") %>%
  # remove incomplete weeks
  filter(n == 7) %>%
  select(-c(n, year))

grid_day_week <- left_join(grid, grid_weekly,
                           by = c("date", "location_name", 
                                  "target_variable", "source"))