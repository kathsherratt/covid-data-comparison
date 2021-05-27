# Get hospitalisation / ICU data
library(dplyr)
library(readr)
library(tidyr)
library(lubridate)


# ECDC public -------------------------------------------------------------
ecdc_health <- read_csv("https://opendata.ecdc.europa.eu/covid19/hospitalicuadmissionrates/csv/data.csv") %>%
  select(location_name = country, 
         year_week, date,
         target_variable = indicator, value) %>%
  mutate(date = ymd(date))

# Prevalence - daily
ecdc_prev <- ecdc_health %>%
  filter(target_variable %in% c("Daily hospital occupancy", "Daily ICU occupancy")) %>%
  expand(date = full_seq(date, 1), 
         location_name, target_variable) %>%
  left_join(select(ecdc_health,
                   -year_week), 
            by = c("location_name", "date", "target_variable")) %>%
  mutate(source = "ECDC-public-daily",
         target_variable = factor(target_variable,
                                  levels = unique(.$target_variable),
                                  labels = c("prev_hosp",
                                             "prev_icu")))

# Incidence - weekly per 100k
week_date <- ecdc_health %>%
  group_by(year_week) %>%
  summarise(date = max(date, na.rm = TRUE))

pop <- read_csv("https://raw.githubusercontent.com/epiforecasts/covid19-forecast-hub-europe/main/data-locations/locations_eu.csv") %>%
  select(-location)

# Rates to counts
ecdc_inc <- ecdc_health %>%
  filter(target_variable %in% c("Weekly new hospital admissions per 100k", 
                                "Weekly new ICU admissions per 100k")) %>%
  expand(year_week, location_name, target_variable) %>%
  left_join(ecdc_health, by = c("location_name", 
                                       "year_week",
                                       "target_variable")) %>%
  left_join(week_date, by = "year_week") %>%
  select(date = date.y, value, everything(), -date.x) %>%
  left_join(pop, by = "location_name") %>%
  mutate(value_weekly = (value * population) / 100000,
         source = "ECDC-public-weekly") %>%
  select(-population, -year_week, -value) %>%
  mutate(target_variable = factor(target_variable,
                                  levels = unique(.$target_variable),
                                  labels = c("inc_hosp",
                                             "inc_icu"))) 

# JRC ---------------------------------------------------------------------
jrc <- read_csv("https://raw.githubusercontent.com/ec-jrc/COVID-19/master/data-by-country/jrc-covid-19-all-days-by-country.csv") %>%
  rename_with(tolower) %>%
  filter(countryname %in% pop$location_name) %>%
  group_by(countryname) %>%
  mutate(inc_case = cumulativepositive - lag(cumulativepositive),
         inc_death = cumulativedeceased - lag(cumulativedeceased)) %>%
  pivot_longer(cols = c(inc_case, inc_death, 
                        hospitalized, intensivecare), 
               names_to = "target_variable",
               values_to = "value") %>%
  mutate(source = "JRC",
         target_variable = recode(target_variable, 
                                  "hospitalized" = "prev_hosp",
                                  "intensivecare" = "prev_icu")) %>%
  select(location_name = countryname, date,
         target_variable, value, source) %>%
  filter(target_variable %in% c("prev_hosp", "prev_icu"))

jrc_health_grid <- jrc %>%
  expand(date = full_seq(date, 1),
         location_name, target_variable, source) %>%
  left_join(jrc, by = c("location_name", "date", "target_variable", "source"))


# Bind --------------------------------------------------------------------
health <- bind_rows(jrc_health_grid,
                    ecdc_prev)

# Weekly ------------------------------------------------------------------
health_weekly <- health %>%
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

health_day_week <- left_join(health, health_weekly,
                           by = c("date", "location_name", 
                                  "target_variable", "source")) %>%
  bind_rows(ecdc_inc)
  