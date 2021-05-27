# Get raw data into standard format
library(dplyr)
library(tidyr)
library(here)
library(readr)
library(lubridate)

locations <- read_csv("locations_eu.csv")
max_date <- as.Date("2021-03-15")
days <- seq.Date(from = max_date,
                 length.out = 12*7, by = -1)

# ECDC  --------------------------------------------------------------------
# Public - daily
ecdc_public <- read_csv("https://opendata.ecdc.europa.eu/covid19/nationalcasedeath_eueea_daily_ei/csv/data.csv") %>%
  mutate(date = dmy(dateRep),
         source = "ECDC-public") %>%
  select(date, inc_case = cases, inc_death = deaths, 
         location_name = countriesAndTerritories, source) %>%
  pivot_longer(cols = c(inc_case, inc_death), names_to = "target_variable") %>%
  group_by(location_name, target_variable) %>%
  arrange(date) %>%
  mutate(value = ifelse(row_number() == 1 & !is.na(value), NA, value))

days_ecdc_public <- seq.Date(from = min(ecdc_public$date), to = max(ecdc_public$date), by = 1)


# JHU ---------------------------------------------------------------------
jhu_cases <- read_csv("https://raw.githubusercontent.com/epiforecasts/covid19-forecast-hub-europe/main/data-truth/JHU/truth_JHU-Incident%20Cases.csv") %>%
  mutate(target_variable = as.factor("inc_case"),
         source = "JHU") %>%
  select(-location)

jhu_deaths <- read_csv("https://raw.githubusercontent.com/epiforecasts/covid19-forecast-hub-europe/main/data-truth/JHU/truth_JHU-Incident%20Deaths.csv") %>%
  mutate(target_variable = as.factor("inc_death"),
         source = "JHU") %>%
  select(-location)

# JRC ---------------------------------------------------------------------
jrc <- read_csv("https://raw.githubusercontent.com/ec-jrc/COVID-19/master/data-by-country/jrc-covid-19-all-days-by-country.csv") %>%
  rename_with(tolower) %>%
  filter(countryname %in% locations$location_name) %>%
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


# Bind all ----------------------------------------------------------------
all_data <- bind_rows(ecdc_private, ecdc_public, jhu_cases, jhu_deaths, jrc, who)

expand_data <- function(data) {
  grid <- expand(data,
                 date = full_seq(date, 1), 
                 location_name, target_variable, source) %>%
      left_join(data, by = c("location_name", "date", "target_variable", "source"))
  return(grid)
}

grid <- bind_rows(expand_data(ecdc_public),
                   expand_data(jhu_cases), 
                   expand_data(jhu_deaths), 
                   expand_data(jrc), 
                   expand_data(who)
)


# Plotting defaults -------------------------------------------------------
source_colours <- c(
  "WHO" = "#377eb8",
  "JRC" = "#4daf4a",
  "JHU" = "#984ea3",
  "ECDC-public" = "#ff7f00",
  "ECDC-private" = "#e41a1c",
  "ECDC-public-daily" = "#ff7f00",
  "ECDC-public-weekly" = "#ffff33"
)

