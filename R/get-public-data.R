# Get public data for European countries
# - Data includes incident cases and deaths
# - Source data from ECDC, JHU, JRC, WHO
# - Get raw data into standard format
# - Expand data to include any missing dates in timeseries

library(dplyr)
library(tidyr)
library(here)
library(readr)
library(lubridate)


get_euro_data <- function() {
  
  locations <- read_csv("https://raw.githubusercontent.com/epiforecasts/covid19-forecast-hub-europe/main/data-locations/locations_eu.csv")
  
  # ECDC  --------------------------------------------------------------------
  ecdc <- read_csv("https://opendata.ecdc.europa.eu/covid19/nationalcasedeath_eueea_daily_ei/csv/data.csv") %>%
    mutate(date = dmy(dateRep),
           source = "ECDC-public") %>%
    select(date, inc_case = cases, inc_death = deaths, 
           location_name = countriesAndTerritories, source) %>%
    pivot_longer(cols = c(inc_case, inc_death), names_to = "target_variable") %>%
    group_by(location_name, target_variable) %>%
    arrange(date) %>%
    mutate(value = ifelse(row_number() == 1 & !is.na(value), NA, value))
  
  # JHU ---------------------------------------------------------------------
  jhu_cases <- read_csv("https://raw.githubusercontent.com/epiforecasts/covid19-forecast-hub-europe/main/data-truth/JHU/truth_JHU-Incident%20Cases.csv") %>%
    mutate(target_variable = as.factor("inc_case"),
           source = "JHU") %>%
    select(-location)
  
  jhu_deaths <- read_csv("https://raw.githubusercontent.com/epiforecasts/covid19-forecast-hub-europe/main/data-truth/JHU/truth_JHU-Incident%20Deaths.csv") %>%
    mutate(target_variable = as.factor("inc_death"),
           source = "JHU") %>%
    select(-location)
  
  jhu <- bind_rows(jhu_cases, jhu_deaths)
  
  # JRC ---------------------------------------------------------------------
  jrc <- read_csv("https://raw.githubusercontent.com/ec-jrc/COVID-19/master/data-by-country/jrc-covid-19-all-days-by-country.csv") %>%
    rename_with(tolower) %>%
    filter(countryname %in% locations$location_name) %>%
    group_by(countryname) %>%
    mutate(inc_case = cumulativepositive - lag(cumulativepositive),
           inc_death = cumulativedeceased - lag(cumulativedeceased)) %>%
    pivot_longer(cols = c(inc_case, inc_death), 
                 names_to = "target_variable",
                 values_to = "value") %>%
    mutate(source = "JRC") %>%
    select(location_name = countryname, date,
           target_variable, value, source)
  
  # WHO ---------------------------------------------------------------------
  who <- read_csv("https://covid19.who.int/WHO-COVID-19-global-data.csv") %>%
    select(location_name = Country, 
           date = Date_reported,
           inc_case = New_cases,
           inc_death = New_deaths) %>%
    filter(location_name %in% locations$location_name) %>%
    pivot_longer(cols = c(inc_case, inc_death),
                 names_to = "target_variable") %>%
    mutate(source = "WHO")
  
  
  # Expand to include implicit missing dates ------------------------------
  expand_data <- function(data) {
    grid <- expand(data,
                   date = full_seq(date, 1), 
                   location_name, target_variable, source) %>%
      left_join(data, by = c("location_name", "date", "target_variable", "source"))
    return(grid)
  }
  
  grid <- bind_rows(expand_data(ecdc),
                    expand_data(jhu),
                    expand_data(jrc), 
                    expand_data(who)
  )
  
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
  
  # Return
  return(grid_day_week)
  
}


