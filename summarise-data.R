library(dplyr)
library(tidyr)
library(here)
library(lubridate)
library(purrr)
library(ggplot2)

# source("get-data.R")

max_date <- as.Date("2021-03-15")
min_date <- as.Date("2021-02-28")

# Summarise data ------------------------------------------------------
  # Summarise missing
  by_loc <- grid %>%
    group_by(target_variable, source) %>%
    summarise(missing = sum(is.na(value)) / n() *100,
              miss_last_week = sum((date >= (max(days) - 7) & 
                                      is.na(value)), 
                                    na.rm = TRUE) / n() *100,
              negative = sum(value < 0, na.rm = TRUE) / n() *100)

# Plot --------------------------------------------------------------------
grid %>%
  filter(location_name == "Lithuania" & 
           target_variable == "inc_death" &
           date >= "2020-12-01" & date <= max_date) %>%
    ggplot(aes(x = date, y = value, fill = source, colour = source)) +
    geom_point() +
  geom_line() +
    labs(x = NULL, y = "Daily incidence cases",
         subtitle = "Lithuania") +
    theme_classic() +
    theme(legend.position = "bottom")
  
  # All sources - deaths
  grid %>%
    filter(date >= min_date & 
             date <= max_date &
             target_variable == "inc_death") %>%
    ggplot(aes(x = date, y = value, fill = source, colour = source)) +
    geom_line() +
    labs(x = NULL, y = "Daily incidence deaths") +
    facet_wrap("location_name",
               scales = "free_y") +
    theme_classic() +
    theme(legend.position = "bottom",
          strip.background = element_blank())
  
  ggsave("all-source-deaths.jpg", height = 8, width = 15)
  
  # All sources - cases
  grid %>%
    filter(date >= min_date & 
             date <= max_date &
             target_variable == "inc_case") %>%
    ggplot(aes(x = date, y = value, fill = source, colour = source)) +
    geom_point(size = 1, position = position_jitter(height = 0.1)) +
    geom_line() +
    geom_hline(yintercept = 0, lty = 2) +
    labs(x = NULL, y = "Daily incidence cases") +
    facet_wrap("location_name",
               scales = "free_y") +
    theme_classic() +
    theme(legend.position = "bottom",
          strip.background = element_blank())
  
 ggsave("all-source-cases.jpg", height = 8, width = 15)
  