source("R/health-data.R")

source_colours <- c(
  "WHO" = "#377eb8",
  "JRC" = "#4daf4a",
  "JHU" = "#984ea3",
  "ECDC-public" = "#ff7f00",
  "ECDC-private" = "#e41a1c",
  "ECDC-public-daily" = "#ff7f00",
  "ECDC-public-weekly" = "#ffff33"
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
min_date <- Sys.Date() - 43
max_date <- Sys.Date() - 1

# Plot --------------------------------------------------------------------

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
        strip.background = element_blank()) +
  ggsave(paste0(figure_folder, "all-source-prev-hosp.jpg"), height = 8, width = 15)

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
  labs(x = NULL, y = "Daily and weekly ICU occupancy") +
  facet_wrap("location_name", scales = "free_y") +
  theme_classic() +
  theme(legend.position = "bottom",
        strip.background = element_blank()) +
  ggsave(paste0(figure_folder, "all-source-prev-icu.jpg"), height = 8, width = 15)


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
        strip.background = element_blank()) +
  ggsave(paste0(figure_folder, "all-source-inc-hosp.jpg"), height = 8, width = 15)

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
        strip.background = element_blank()) +
  ggsave(paste0(figure_folder, "all-source-inc-icu.jpg"), height = 8, width = 15)
