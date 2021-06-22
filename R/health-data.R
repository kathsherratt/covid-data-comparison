# Get hospitalisation / ICU data
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)

# Set up ------------------------------------------------------------------
full_date_seq <- seq.Date(from =  as.Date("2021-01-01"), to = as.Date("2021-06-19"), by = 1)
full_week_seq <- seq.Date(from =  as.Date("2021-01-10"), to = as.Date("2021-06-19"), by = 7)
pop <- read_csv("https://raw.githubusercontent.com/epiforecasts/covid19-forecast-hub-europe/main/data-locations/locations_eu.csv")
setting <- c("General hospital", "Intensive care unit")
count = c("Occupancy", "Admissions")
source = c("ECDC", "OWID", "JRC")

# Expand to all possible combinations of location, target, date
grid <- expand_grid(source = source,
                    date = full_date_seq, 
                    location = pop$location,
                    setting = setting,
                    count = count) %>%
  # remove daily rows for admissions data provided in weekly aggregates
  filter(!(count == "Admissions" & !date %in% full_week_seq))

# Raw data ----------------------------------------------------------------

# ECDC data
ecdc_raw <- read_csv("https://opendata.ecdc.europa.eu/covid19/hospitalicuadmissionrates/csv/data.csv") %>%
  select(location_name = country, 
         year_week, 
         date,
         indicator, 
         value) %>%
  left_join(pop, by = "location_name") %>%
  mutate(date = ymd(date),
         setting = case_when(str_detect(indicator, "hospital") ~ "General hospital", 
                                     str_detect(indicator, "ICU") ~ "Intensive care unit"),
         count = case_when(str_detect(indicator, "admissions") ~ "Admissions",
                           str_detect(indicator, "occupancy") ~ "Occupancy"),
         # convert rate to count
         value = case_when(count == "Admissions" ~ (value * population) / 100000,
                           count == "Occupancy" ~ value),
         source = "ECDC") %>%
  select(source, date, location, setting, count, value)

# OWID
owid_raw <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv",
                 col_types = cols(.default = "c")) %>%
  select(location_name = location, 
         date, 
         icu_patients,
         weekly_icu_admissions,
         hosp_patients,
         weekly_hosp_admissions) %>%
  right_join(pop, by = "location_name") %>%
  select(-population, -location_name) %>%
  pivot_longer(contains(c("hosp", "icu")),
               names_to = "setting_count",
               values_to = "value") %>%
  mutate(date = ymd(date),
         value = as.numeric(value),
         setting = case_when(str_detect(setting_count, "hosp") ~ "General hospital", 
                             str_detect(setting_count, "icu") ~ "Intensive care unit"),
         count = case_when(str_detect(setting_count, "admissions") ~ "Admissions",
                           str_detect(setting_count, "patients") ~ "Occupancy"),
         source = "OWID") %>%
  select(source, date, location, setting, count, value)

# JRC data
jrc_raw <- read_csv("https://raw.githubusercontent.com/ec-jrc/COVID-19/master/data-by-country/jrc-covid-19-all-days-by-country.csv") %>%
  rename_with(tolower) %>%
  select(date, countryname, 
         "General hospital" = hospitalized, 
         "Intensive care unit" = intensivecare) %>%
  right_join(pop, by = c("countryname" = "location_name")) %>%
  group_by(location) %>%
  pivot_longer(cols = c(`General hospital`, `Intensive care unit`), 
               names_to = "setting",
               values_to = "value") %>%
  mutate(count = "Occupancy",
         source = "JRC") %>%
  select(source, date, location, setting, count, value)


# Expanded data -----------------------------------------------------------
raw <- bind_rows(jrc_raw, owid_raw, ecdc_raw)

grid_data <- left_join(grid, raw, by = c("source", "date", "location", 
                                         "setting", "count"))

grid_data <- grid_data %>%
  mutate(source = factor(source,
                         levels = c("ECDC", "OWID", "JRC")))

rm(ecdc_raw, jrc_raw, owid_raw, grid)
