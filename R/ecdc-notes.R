# Summarise health data


# Overall:
# ECDC
# - Daily data: occupancy - hospital and ICU
# - Weekly data: admissions (sum of preceding week) - hospital and ICU
# - Both weekly and daily data are published once a week on Thursday
# - Covers preceding week
# 
# Incidence
# - Weekly incidence data not available for all countries (e.g Belgium missing incidence data)
# - Weekly data uses a Monday to Sunday week definition
# - eg 2021-W22 covers Monday 2021-05-31 to Sunday 2021-06-06 inclusive
# 
# Lagged data
# - Compare to e.g. case data with lag of 1 day (data published today goes up to yesterday)
# - This means latest data can be lagged at least 4 days and up to 10 days
# - Data downloaded on 15 June has latest data as of 6 June
# - So a "1 week ahead forecast" would be a target end date of 1 week earlier than case/death forecasts
# 
# - Daily data = prevalence (hospital occupancy) only, published once a week
# - Weekly data includes incidence (new admissions per week), published once a week
# - public ECDC data being best if we can get hold of their population denominators?


# what the status is of which data is available for which countries (maybe a table) 


# what the limitations are that you found (missing data?

# Differences to forecasting case/deaths
# - Weekly incidence data should use the same population denominator - we can provide this
# - Lagged data means a different definition of "week ahead forecast", e.g. 1 week ahead
# - Monday to Sunday week definition matters if using incidence data, only available as aggregated weekly

# Data status by country
# ECDC data
status <- healthcare_all %>%
  group_by(location_name, source, target_variable, date) %>%
  summarise(mean_prev = mean(value, na.rm = TRUE),
            total_inc = sum(value_weekly, na.rm = TRUE),
            n = n())


# Occupancy
occ <- healthcare_all %>%
  filter(target_variable == "prev_hosp")


