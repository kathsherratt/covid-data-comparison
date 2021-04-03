# Compare data sources
library(dplyr)
library(tidyr)
library(here)
library(ggplot2)
library(scales)
library(covidHubUtils)
library(forcats)

ecdc_raw <- readr::read_csv("C:/Users/kaths/Downloads/v4.COVID.filtered_for_model.csv") %>%
  select(
    location_name = CountryName,
    date = Date,
    value = Value,
    target_variable = Indicator,
    source = Source
)

# variables <- count(ecdc, target_variable)

ecdc_daily <- ecdc_raw %>%
  filter(target_variable %in% c("New_Cases", "New_Deaths")) %>%
  mutate(target_variable = recode(target_variable,
                                  "New_Cases" = "inc case",
                                  "New_Deaths" = "inc death"),
         source_missing = 0) %>%
  complete(nesting(location_name, target_variable), date) %>%
  mutate(source_missing = ifelse(is.na(source_missing), 1, source_missing))

# JHU ---------------------------------------------------------------------
jhu_daily <- readr::read_csv(here("data-truth/JHU/truth_JHU-Incident Cases.csv")) %>%
  mutate(target_variable = "inc case") %>%
  bind_rows(readr::read_csv(here("data-truth/JHU/truth_JHU-Incident Deaths.csv")) %>%
              mutate(target_variable = "inc death")) %>%
  filter(date <= max(ecdc_daily$date)) %>%
  mutate(source_missing = ifelse(is.na(target_variable), 1, 0))

# JRC ---------------------------------------------------------------------

jrc <- readr::read_csv("https://raw.githubusercontent.com/ec-jrc/COVID-19/master/data-by-country/jrc-covid-19-all-days-by-country.csv")
names(jrc) <- tolower(names(jrc))
  
jrc_daily <- jrc %>%
  filter(countryname %in% ecdc_daily$location_name &
           date %in% ecdc_daily$date) %>%
  mutate(source_missing = ifelse(is.na(cumulativepositive | cumulativedeceased),
                                 1, 0)) %>%
  group_by(countryname) %>%
  mutate(`inc case` = cumulativepositive - lag(cumulativepositive),
         `inc death` = cumulativedeceased - lag(cumulativedeceased)) %>%
  pivot_longer(cols = c(`inc case`, `inc death`), 
               names_to = "target_variable",
               values_to = "value") %>%
  select(location_name = countryname, 
         source_missing, date,
         target_variable, value) %>%
  complete(nesting(location_name, target_variable), date) %>%
  mutate(source_missing = ifelse(is.na(value), 1, source_missing))


# Compare -----------------------------------------------------------------
comparison_long <- bind_rows(ecdc_daily, jhu_daily, jrc_daily,
                             .id = "org_source") %>%
  mutate(org_source = recode(org_source, 
                             "1" = "ecdc", "2" = "jhu", "3" = "jrc"))


comparison_12wk <- filter(comparison_long, date >= (max(date) - (7*12)))

n_days_12wk <- length(unique(comparison_12wk$date))

# Missing - recent
comparison_12wk %>%
  group_by(org_source, target_variable, location_name) %>%
  summarise(source_missing = sum(source_missing) / 
              length(unique(.data$date))* 100) %>%
  filter(org_source == "ecdc") %>%
  ggplot(aes(col = target_variable, fill = target_variable)) +
  geom_col(aes(x = fct_reorder(location_name, -source_missing), 
               y = source_missing), 
           position = position_stack(reverse = TRUE)) +
  labs(y = "% dates missing value", x = NULL, 
       fill = "target", col = "target",
       title = "ECDC daily data: recent",
       subtitle = "21 Dec to 15 March 2021") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1),
        legend.position = "bottom")
  
# Missing - all time
comparison_long %>%
  group_by(org_source, target_variable, location_name) %>%
  summarise(source_missing = sum(source_missing) / 
              length(unique(.data$date))* 100) %>%
  filter(org_source == "ecdc") %>%
  ggplot(aes(col = target_variable, fill = target_variable)) +
  geom_col(aes(x = fct_reorder(location_name, -source_missing), 
               y = source_missing), 
           position = position_stack(reverse = TRUE)) +
  labs(y = "% dates missing value", x = NULL, 
       fill = "target", col = "target",
       title = "ECDC daily data: all time",
       subtitle = "1 Jan 2020 to 15 March 2021") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1),
        legend.position = "bottom")

comparison_12wk %>%
  filter(org_source %in% c("ecdc", "jhu")) %>%
  filter(target_variable == "inc case") %>%
  #mutate(location_name = fct_reorder(location_name, value, .fun = median, na.rm = TRUE)) %>%
  ggplot(aes(x = date, y = value, colour = org_source)) +
  geom_line() +
  facet_wrap(~ location_name, scales = "free_y") +
  scale_y_continuous(labels = label_number(accuracy = 1)) +
  labs(subtitle = "Daily incident cases",
       x = NULL, y = "Incident cases", colour = "source") +
  theme_classic() +
  theme(legend.position = "bottom",
        strip.background = element_blank()) +
  ggsave(filename = "daily-comparison-cases.png", height = 8, width = 16)

comparison_12wk %>%
  filter(org_source %in% c("ecdc", "jhu")) %>%
  filter(target_variable == "inc death") %>%
  #mutate(location_name = fct_reorder(location_name, value, .fun = median, na.rm = TRUE)) %>%
  ggplot(aes(x = date, y = value, colour = org_source)) +
  geom_line() +
  facet_wrap(~ location_name, scales = "free_y") +
  scale_y_continuous(labels = label_number(accuracy = 1)) +
  labs(subtitle = "Daily incident deaths",
       x = NULL, y = "Incident deaths", colour = "source") +
  theme_classic() +
  theme(legend.position = "bottom",
        strip.background = element_blank()) +
  ggsave(filename = "daily-comparison-deaths.png", height = 8, width = 16)


# Weekly comparison -------------------------------------------------------

weekly_comparison_12wk <- comparison_12wk %>%
  mutate(epidate = paste0(lubridate::epiyear(date), "-", lubridate::epiweek(date))) %>%
  group_by(org_source, location_name, target_variable, epidate) %>%
  summarise(value = sum(value, na.rm = TRUE),
            date = max(date))

weekly_comparison_12wk %>%
  filter(org_source %in% c("ecdc", "jhu")) %>%
  filter(target_variable == "inc case") %>%
  #mutate(location_name = fct_reorder(location_name, value, .fun = median, na.rm = TRUE)) %>%
  ggplot(aes(x = date, y = value, colour = org_source)) +
  geom_line() +
  facet_wrap(~ location_name, scales = "free_y") +
  scale_y_continuous(labels = label_number(accuracy = 1)) +
  labs(subtitle = "Weekly incident cases",
       x = NULL, y = "Incident cases", colour = "source") +
  theme_classic() +
  theme(legend.position = "bottom",
        strip.background = element_blank()) +
  ggsave(filename = "weekly-comparison-case.png", height = 8, width = 16)

weekly_comparison_12wk %>%
  filter(org_source %in% c("ecdc", "jhu")) %>%
  filter(target_variable == "inc death") %>%
  #mutate(location_name = fct_reorder(location_name, value, .fun = median, na.rm = TRUE)) %>%
  ggplot(aes(x = date, y = value, colour = org_source)) +
  geom_line() +
  facet_wrap(~ location_name, scales = "free_y") +
  scale_y_continuous(labels = label_number(accuracy = 1)) +
  labs(subtitle = "Weekly incident deaths",
       x = NULL, y = "Incident deaths", colour = "source") +
  theme_classic() +
  theme(legend.position = "bottom",
        strip.background = element_blank()) +
  ggsave(filename = "weekly-comparison-deaths.png", height = 8, width = 16)


# Differences -------------------------------------------------------------
ecdc_jhu <- comparison_12wk %>%
  select(-c(source, source_missing, location)) %>%
  filter(org_source %in% c("ecdc", "jhu")) %>%
  group_by(location_name, target_variable, date) %>%
  arrange(org_source) %>%
  mutate(diff = value - lag(value, 1),
         abs_diff = abs(diff),
         diff_value = diff / value) %>%
  ungroup()

ecdc_jhu_diff <- ecdc_jhu %>%
  filter(org_source == "jhu") %>%
  group_by(location_name, target_variable) %>%
  summarise(mean_abs_diff = mean(abs_diff, na.rm = T),
            mean_diff_value = mean(diff_value, na.rm = T),
            date_count = sum(!diff == 0, na.rm = TRUE))

# Compare ECDC / JHU ------------------------------------------------------
comparison_12wk_wide <- comparison_12wk %>%
  select(org_source, location_name, date, target_variable, value) %>%
  pivot_wider(id_cols = c(location_name, target_variable, date),
              names_from = org_source, values_from = value) %>%
  mutate(ecdc_jhu = ecdc - jhu,
         ecdc_jrc = ecdc - jrc) %>%
  select(-c(jhu, jrc)) %>%
  pivot_longer(cols = c(ecdc_jhu, ecdc_jrc), 
               names_to = "alt_source", values_to = "diff")

comparison_sum <- comparison_12wk_wide %>%
  group_by(target_variable, location_name, alt_source) %>%
  summarise(diff_mean = mean(diff, na.rm = TRUE),
            n_diff = sum(diff != 0, na.rm = TRUE)) %>%
  mutate(percent_differ = n_diff / n_days_12wk * 100)

comparison_sum %>%
  mutate(location_name = fct_reorder(location_name, -percent_differ)) %>%
  ggplot(aes(col = target_variable, fill = alt_source)) +
  geom_col(aes(x = location_name, y = diff_mean)) +
  labs(y = "Mean difference in count") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1),
        legend.position = "bottom")

comparison_sum %>%
  mutate(location_name = fct_reorder(location_name, percent_na)) %>%
  ggplot(aes(col = target_variable, fill = target_variable)) +
  geom_col(aes(x = location_name, y = percent_na)) +
  coord_cartesian(ylim = c(0, 30)) +
  labs(y = "Percent days with one source NA") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1),
        legend.position = "bottom")





