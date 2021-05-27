# Private - daily
ecdc_private <- read_csv("C:/Users/kaths/Documents/private-data/COVID.csv") %>%
  select(location_name = CountryName, date = Date,
         value = Value, target_variable = Indicator) %>%
  filter(target_variable %in% c("New_Cases", "New_Deaths")) %>%
  mutate(target_variable = factor(target_variable,
                                  levels = unique(.$target_variable),
                                  labels = c("inc_case", "inc_death")),
         source = "ECDC-private")

# ECDC private ------------------------------------------------------------
ecdc_private_health <- read_csv("C:/Users/kaths/Documents/private-data/COVID.csv") %>%
  select(location_name = CountryName, date = Date,
         value = Value, target_variable = Indicator) %>%
  filter(!target_variable %in% c("New_Cases", "New_Deaths", 
                                 "New_Respirator", "Current_Respirator",
                                 "New_Tested")) %>%
  mutate(target_variable = factor(target_variable,
                                  levels = unique(.$target_variable),
                                  labels = c("prev_hosp", # "Current_Hospitalised"
                                             "prev_icu", # "Current_ICU"
                                             "inc_hosp", # "New_Hospitalised"
                                             "inc_icu")), # "New_ICU"
         source = "ECDC-private")

ecdc_private_health_grid <- ecdc_private_health %>%
  expand(date = full_seq(date, 1), 
         location_name, target_variable, source) %>%
  left_join(ecdc_private_health, by = c("location_name", "date", "target_variable", "source"))