source("R/health-data.R")

library(ggplot2)

source_colours <- c(
  "WHO" = "#377eb8",
  "JRC" = "#4daf4a",
  "JHU" = "#984ea3",
  "ECDC-public" = "#ff7f00",
  "ECDC-private" = "#e41a1c",
  "ECDC-public-daily" = "#ff7f00",
  "ECDC-public-weekly" = "#FFD700"
)

# # Public data only:
# grid_day_week <- grid_day_week %>%
#   filter(!source %in% "ECDC-private")
# 
# Set folder for saving 
if (any(unique(health$source) %in% "ECDC-private")) {
  figure_folder <- here("figures", "with-private", "/")
} else {
  figure_folder <- here("figures", "/")
}

# Set dates as last 6 weeks
min_date <- Sys.Date() - weeks(6)
max_date <- Sys.Date() - days(1)

# Plot function -----------------------------------------------------------

plot_health <- function(data, data_weekly,
                        variable, 
                        min_date, max_date) {
  data %>%
    filter(date >= min_date & date <= max_date) %>%
    filter(target_variable == variable) %>%
    ggplot(aes(x = date, colour = source, fill = source)) +
    geom_line(aes(y = value_weekly), 
              data = filter(data_weekly,
                            date >= min_date & date <= max_date &
                              target_variable == variable)) +
    geom_point(aes(y = value), size = 1) +
    scale_fill_manual(values = source_colours) +
    scale_colour_manual(values = source_colours) +
    facet_wrap("location_name", scales = "free_y") +
    theme_classic() +
    theme(legend.position = "bottom",
          strip.background = element_blank())
}


# Plot --------------------------------------------------------------------

# Occupancy
plot_health(health_day_week, health_weekly,
            "prev_hosp",
            min_date = min_date, 
            max_date = max_date) +
  labs(x = NULL, y = "Daily and weekly hospital occupancy") +
  ggsave(paste0(figure_folder, "all-source-prev-hosp.jpg"), height = 8, width = 15)

plot_health(health_day_week, health_weekly,
            "prev_icu",
            min_date = min_date, 
            max_date = max_date) +
  labs(x = NULL, y = "Daily and weekly ICU occupancy") +
  ggsave(paste0(figure_folder, "all-source-prev-icu.jpg"), height = 8, width = 15)


# Incidence
plot_health(health_day_week, health_weekly,
            "inc_hosp",
            min_date = min_date, 
            max_date = max_date)
  labs(x = NULL, y = "Daily and weekly hospital admissions") +
  ggsave(paste0(figure_folder, "all-source-inc-hosp.jpg"), height = 8, width = 15)

plot_health(health_day_week, health_weekly,
            "inc_icu",
            min_date = min_date, 
            max_date = max_date) +
  labs(x = NULL, y = "Daily and weekly ICU admissions") +
  ggsave(paste0(figure_folder, "all-source-inc-icu.jpg"), height = 8, width = 15)

