# Plot cases/deaths

source("R/summarise-data.R")

# # Public data only:
# grid_day_week <- grid_day_week %>%
#   filter(!source %in% "ECDC-private")

easter_min_date <- as.Date("2021-03-29")
easter_max_date <- as.Date("2021-04-12")

# Plot --------------------------------------------------------------------
source_colours <- c(
  "WHO" = "#377eb8",
  "JRC" = "#4daf4a",
  "JHU" = "#984ea3",
  "ECDC-public" = "#ff7f00",
  "ECDC-private" = "#e41a1c",
  "ECDC-public-daily" = "#ff7f00",
  "ECDC-public-weekly" = "#ffff33"
)

plot_by_source <- function(grid, target,
                           weekly = TRUE,
                           location_wrap = TRUE,
                           min_date = NA, max_date = NA) {
  
  grid <- grid %>%
    mutate(zero = ifelse(value < 0, 0, NA)) %>%
    filter(date >= as.Date(min_date) & 
             date <= as.Date(max_date) &
             target_variable == target) 
  
  grid_plot <- grid %>%
    ggplot(aes(x = date, fill = source, colour = source))
  
  if (weekly) {
    grid_plot <- grid_plot +
      geom_line(aes(y = value_weekly), 
                data = filter(grid, !is.na(epiweek))) +
      geom_point(aes(y = value), size = 1, alpha = 1)
  } else {
    grid_plot <- grid_plot +
      geom_point(aes(y = value), size = 1, alpha = 1) +
      geom_line(aes(y = value), alpha = 1)
  }
  
  grid_plot <- grid_plot +
    scale_y_continuous(labels = label_number(accuracy = 1)) +
    scale_fill_manual(values = source_colours) +
    scale_colour_manual(values = source_colours) +
    labs(x = NULL, fill = "Source", colour = "Source") +
    theme_classic() +
    theme(legend.position = "bottom",
          strip.background = element_blank())
  
  if (location_wrap) {
    grid_plot <- grid_plot +
      facet_wrap("location_name",
                 scales = "free_y") +
      geom_hline(aes(yintercept = zero), lty = 2)
  }
  
  return(grid_plot)
}


# JHU different source ----------------------------------------------------
# Countries where JHU may use a different data source (not WHO)
jhu_non_who <- c(
  "Belgium", "Czechia", "France", "Germany", "Iceland", 
  "Ireland", "Italy", "Kosovo", "Lithuania", "Luxembourg", 
  "Netherlands", "Serbia", "Slovakia", "Spain", "Sweden", 
  "Turkey", "UK")

jhu_diverge_who <- c(
  "Belgium",  "France", "Germany", 
  "Ireland",  "Kosovo",  "Luxembourg", 
  "Serbia", "Spain", "Sweden", 
  "Turkey", "UK")

grid_day_week %>%
  filter(location_name %in% jhu_diverge_who) %>%
  plot_by_source(target = "inc_case", weekly = TRUE,
                 min_date = easter_min_date, max_date = easter_max_date) +
  geom_vline(xintercept = as.Date("2021-04-01"), lty = 3) +
  geom_vline(xintercept = as.Date("2021-04-05"), lty = 3) +
  labs(x = NULL, y = "Daily and weekly incident cases")  

ggsave("figures/all-source-cases-jhu.jpg", height = 5, width = 10)

grid_day_week %>%
  filter(location_name %in% jhu_diverge_who) %>%
  plot_by_source(target = "inc_death", weekly = TRUE, 
                 min_date = easter_min_date, max_date = easter_max_date) +
  geom_vline(xintercept = as.Date("2021-04-01"), lty = 3) +
  geom_vline(xintercept = as.Date("2021-04-05"), lty = 3) +
  labs(x = NULL, y = "Daily and weekly incident deaths")  

ggsave("figures/all-source-deaths-jhu.jpg", height = 5, width = 10)


# Examples ----------------------------------------------------------------

# Set dates when all sources have data (i.e. ECDC private data)
min_date <- as.Date("2020-12-01")
max_date <- as.Date("2021-03-15")

# Lithuania example
grid_day_week %>%
  filter(location_name == "Lithuania") %>%
  plot_by_source(target = "inc_death",
                 location_wrap = FALSE,
                 min_date = min_date, max_date = Sys.Date()) +
  labs(y = "Daily and weekly incident deaths",
       subtitle = "Lithuania")

ggsave("figures/lithuania-deaths.jpg", height = 4, width = 10)  


# All countries daily/weekly plots -------------------------------------------------------------
# Set dates when all sources have data (i.e. ECDC private data)
min_date <- as.Date("2021-02-14")
max_date <- Sys.Date()

# All sources - cases
grid_day_week %>%
  plot_by_source(target = "inc_case",
                 min_date = min_date, max_date = max_date) +
  geom_vline(xintercept = as.Date("2021-04-01"), lty = 3) +
  geom_vline(xintercept = as.Date("2021-04-05"), lty = 3) +
  labs(x = NULL, y = "Daily and weekly incident cases")  

ggsave("figures/all-source-cases.jpg", height = 8, width = 15)


# All sources - deaths
grid_day_week %>%
  plot_by_source(target = "inc_death",
                 min_date = min_date, max_date = max_date) +
  geom_vline(xintercept = as.Date("2021-04-01"), lty = 3) +
  geom_vline(xintercept = as.Date("2021-04-05"), lty = 3) +
  labs(x = NULL, y = "Daily and weekly incident deaths")

ggsave("figures/all-source-deaths.jpg", height = 8, width = 15)  


