# Summarise hospitalisation / ICU data

# source("R/get-data.R")

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
  expand(date = full_seq(date, 1), 
         location_name, target_variable, source) %>%
  left_join(ecdc_private_health, by = c("location_name", "date", "target_variable", "source"))

# ECDC public -------------------------------------------------------------
ecdc_public_health <- read_csv("https://opendata.ecdc.europa.eu/covid19/hospitalicuadmissionrates/csv/data.csv") %>%
  select(location_name = country, 
         year_week, date,
         target_variable = indicator, value) %>%
  mutate(date = ymd(date))

# Prevalence - daily
ecdc_public_prev <- ecdc_public_health %>%
  filter(target_variable %in% c("Daily hospital occupancy", "Daily ICU occupancy")) %>%
  expand(date = full_seq(date, 1), 
         location_name, target_variable) %>%
  left_join(select(ecdc_public_health,
                   -year_week), 
            by = c("location_name", "date", "target_variable")) %>%
  mutate(source = "ECDC-public-daily",
         target_variable = factor(target_variable,
                                  levels = unique(.$target_variable),
                                  labels = c("prev_hosp",
                                             "prev_icu")))

# Incidence - weekly per 100k
week_date <- ecdc_public_health %>%
  group_by(year_week) %>%
  summarise(date = max(date, na.rm = TRUE))

pop <- read_csv("https://raw.githubusercontent.com/epiforecasts/covid19-forecast-hub-europe/main/data-locations/locations_eu.csv") %>%
  select(-location)

ecdc_public_inc <- ecdc_public_health %>%
  filter(target_variable %in% c("Weekly new hospital admissions per 100k", 
                                "Weekly new ICU admissions per 100k")) %>%
  expand(year_week, location_name, target_variable) %>%
  left_join(ecdc_public_health, by = c("location_name", 
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
jrc_health_grid <- jrc %>%
  filter(target_variable %in% c("prev_hosp", "prev_icu")) %>%
  expand(date = full_seq(date, 1),
         location_name, target_variable, source) %>%
  left_join(jrc, by = c("location_name", "date", "target_variable", "source"))


# Bind --------------------------------------------------------------------

health <- bind_rows(jrc_health_grid, ecdc_private_health_grid, 
                    ecdc_public_prev)

# health_sum <- health %>%
#   filter(date <= "2021-03-15") %>% #date >= "2021-03-01" & 
#   group_by(target_variable, source) %>%
#   summarise(missing = sum(is.na(value)),
#             all = n(),
#             present = (1 - (missing / all)) * 100,
#             miss_p = missing / all * 100)

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
  bind_rows(ecdc_public_inc)
  

# Plot --------------------------------------------------------------------
# Set dates when all sources have data (i.e. ECDC private data)
min_date <- as.Date("2021-02-14")
max_date <- as.Date("2021-03-15")

health_day_week %>%
  filter(date >= min_date & date <= max_date) %>%
  filter(target_variable == "prev_hosp") %>%
  ggplot(aes(x = date, colour = source, fill = source)) +
  geom_line(aes(y = value_weekly), 
            data = filter(health_weekly,
                          date >= min_date & date <= max_date &
                          target_variable == "prev_hosp")) +
  geom_point(aes(y = value), size = 1) +
  scale_fill_manual(values = source_colours) +
  scale_colour_manual(values = source_colours) +
  labs(x = NULL, y = "Daily and weekly hospital occupancy") +
  facet_wrap("location_name", scales = "free_y") +
  theme_classic() +
  theme(legend.position = "bottom",
        strip.background = element_blank())

ggsave("figures/all-source-hosp2.jpg", height = 8, width = 15)

health_day_week %>%
  filter(date >= min_date & date <= max_date) %>%
  filter(target_variable == "prev_icu") %>%
  ggplot(aes(x = date, colour = source, fill = source)) +
  geom_line(aes(y = value_weekly), 
            data = filter(health_weekly,
                          date >= min_date & date <= max_date &
                            target_variable == "prev_icu")) +
  geom_point(aes(y = value), size = 1) +
  scale_fill_manual(values = source_colours) +
  scale_colour_manual(values = source_colours) +
  labs(x = NULL, y = "Daily and weekly hospital occupancy") +
  facet_wrap("location_name", scales = "free_y") +
  theme_classic() +
  theme(legend.position = "bottom",
        strip.background = element_blank())

ggsave("figures/all-source-icu2.jpg", height = 8, width = 15)


 # Incidence
health_day_week %>%
  filter(date >= min_date & date <= max_date) %>%
  filter(target_variable == "inc_hosp") %>%
  ggplot(aes(x = date, colour = source, fill = source)) +
  geom_line(aes(y = value_weekly), 
            data = filter(health_weekly,
                          date >= min_date & date <= max_date &
                            target_variable == "inc_hosp")) +
  geom_point(aes(y = value), size = 1) +
  scale_fill_manual(values = source_colours) +
  scale_colour_manual(values = source_colours) +
  labs(x = NULL, y = "Daily and weekly hospital admissions") +
  facet_wrap("location_name", scales = "free_y") +
  theme_classic() +
  theme(legend.position = "bottom",
        strip.background = element_blank())

ggsave("figures/all-source-inc-hosp.jpg", height = 8, width = 15)

health_day_week %>%
  filter(date >= min_date & date <= max_date) %>%
  filter(target_variable == "inc_icu") %>%
  ggplot(aes(x = date, colour = source, fill = source)) +
  geom_line(aes(y = value_weekly), 
            data = filter(health_weekly,
                          date >= min_date & date <= max_date &
                            target_variable == "inc_icu")) +
  geom_point(aes(y = value), size = 1) +
  scale_fill_manual(values = source_colours) +
  scale_colour_manual(values = source_colours) +
  labs(x = NULL, y = "Daily and weekly ICU admissions") +
  facet_wrap("location_name", scales = "free_y") +
  theme_classic() +
  theme(legend.position = "bottom",
        strip.background = element_blank())

ggsave("figures/all-source-inc-icu.jpg", height = 8, width = 15)

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


