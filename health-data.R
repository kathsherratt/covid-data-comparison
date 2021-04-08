# Summarise hospitalisation / ICU data

# source("get-data.R")

# ECDC public -------------------------------------------------------------
ecdc_public_health <- read_csv("https://opendata.ecdc.europa.eu/covid19/hospitalicuadmissionrates/csv/data.csv") %>%
  select(location_name = country, date,
         value, target_variable = indicator) %>%
  filter(target_variable %in% c("Daily hospital occupancy", "Daily ICU occupancy")) %>%
  mutate(target_variable = factor(target_variable,
                                  levels = unique(.$target_variable),
                                  labels = c("prev_hosp", 
                                             "prev_icu")),
         source = "ECDC-public")

ecdc_public_health_grid <- ecdc_public_health %>%
  expand(date, location_name, target_variable, source) %>%
  left_join(ecdc_public_health, by = c("location_name", "date", "target_variable", "source"))


# ECDC private ------------------------------------------------------------
ecdc_private_health <- read_csv("C:/Users/kaths/Downloads/v4.COVID.filtered_for_model.csv") %>%
  select(location_name = CountryName, date = Date,
         value = Value, target_variable = Indicator) %>%
  filter(!target_variable %in% c("New_Cases", "New_Deaths", "Current_Respirator")) %>%
  mutate(target_variable = factor(target_variable,
                                  levels = unique(.$target_variable),
                                  labels = c("prev_hosp", # "Current_Hospitalised"
                                             "prev_icu", # "Current_ICU"
                                             "inc_hosp", # "New_Hospitalised"
                                             "inc_icu")), # "New_ICU"
         source = "ECDC-private")

ecdc_private_health_grid <- ecdc_private_health %>%
  expand(date, location_name, target_variable, source) %>%
  left_join(ecdc_private_health, by = c("location_name", "date", "target_variable", "source"))



# JRC ---------------------------------------------------------------------
jrc_health_grid <- jrc %>%
  filter(target_variable %in% c("prev_hosp", "prev_icu")) %>%
  expand(date, location_name, target_variable, source) %>%
  left_join(jrc, by = c("location_name", "date", "target_variable", "source"))

# OWID --------------------------------------------------------------------

owid <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv",
                guess_max = 13500) %>%
  filter(location %in% locations$location_name) %>%
  select(date, location_name = location, 
         prev_hosp = hosp_patients, prev_icu = icu_patients) %>%
  mutate(source = "OWID") %>%
  pivot_longer(cols = c("prev_icu", "prev_hosp"), names_to = "target_variable")

owid_grid <- owid %>%
  expand(date, location_name, target_variable, source) %>%
  left_join(owid, by = c("location_name", "date", "target_variable", "source"))

# Bind --------------------------------------------------------------------

health <- bind_rows(jrc_health_grid, ecdc_private_health_grid, ecdc_public_health_grid)

health_sum <- health %>%
  filter(date <= "2021-03-15") %>% #date >= "2021-03-01" & 
  group_by(target_variable, source) %>%
  summarise(missing = sum(is.na(value)),
            all = n(),
            present = (1 - (missing / all)) * 100,
            miss_p = missing / all * 100)


health %>%
  filter(date >= min_date & date <= max_date) %>%
  filter(target_variable == "prev_hosp") %>%
  ggplot(aes(x = date, y = value, colour = source, fill = source)) +
  geom_point(size = 1) +
  geom_line() +
  labs(x = NULL, y = "Daily hospital occupancy") +
  facet_wrap("location_name", scales = "free_y") +
  theme_classic() +
  theme(legend.position = "bottom",
        strip.background = element_blank())

ggsave("all-source-hosp.jpg", height = 8, width = 15)

health %>%
  filter(date >= min_date & date <= max_date) %>%
  filter(target_variable == "prev_icu") %>%
  ggplot(aes(x = date, y = value, colour = source, fill = source)) +
  geom_point(size = 1) +
  geom_line() +
  labs(x = NULL, y = "Daily ICU occupancy") +
  facet_wrap("location_name", scales = "free_y") +
  theme_classic() +
  theme(legend.position = "bottom",
        strip.background = element_blank())

ggsave("all-source-icu.jpg", height = 8, width = 15)


# left join compare -------------------------------------------------------

# join for comparison over 2 weeks
all_join <- full_join(ecdc_public_health_grid, ecdc_private_health_grid,
                       by = c("location_name", "date", "target_variable")) %>%
  rename(value_public = value.x, value_private = value.y) %>%
  filter(date >= "2021-03-01" &
           date <= max(ecdc_private_health_grid$date)) %>%
  left_join(jrc_health_grid, by = c("location_name", "date", "target_variable")) %>%
  rename(value_jhu = value)

mutate(value_diff = ifelse((value_public - value_private) %in% c(0, NA),
                           NA, (value_public - value_private)))

# % date/locations where differ
sum_ecdc_join <- ecdc_join %>%
  group_by(target_variable, location_name) %>%
  summarise(n_public = sum(!is.na(value_public)),
            n_private = sum(!is.na(value_private)))


