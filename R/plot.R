# Plot cases/deaths

source("R/summarise-data.R")

# Set plotting dates
plot_min_date <- Sys.Date() - 43
plot_max_date <- Sys.Date() - 1

easter_start <- as.Date("2021-04-01")
easter_end <- as.Date("2021-04-05")

# # Public data only:
# grid_day_week <- grid_day_week %>%
#   filter(!source %in% "ECDC-private")
#   
# Set folder for saving ---------------------------------------------------
if (any(unique(grid_day_week$source) %in% "ECDC-private")) {
  figure_folder <- here("figures", "with-private", "/")
} else {
  figure_folder <- here("figures", "/")
}

# Plot --------------------------------------------------------------------
plot_by_source <- function(grid, target,
                           weekly = TRUE,
                           location_wrap = TRUE,
                           min_date = plot_min_date, 
                           max_date = plot_max_date) {
  
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
  plot_by_source(target = "inc_case", weekly = TRUE) +
  geom_vline(xintercept = easter_start, lty = 3) +
  geom_vline(xintercept = easter_end, lty = 3) +
  labs(x = NULL, y = "Daily and weekly incident cases")  +
  ggsave(paste0(figure_folder, "all-source-cases-jhu.jpg"), height = 5, width = 10)

grid_day_week %>%
  filter(location_name %in% jhu_diverge_who) %>%
  plot_by_source(target = "inc_death", weekly = TRUE) +
  geom_vline(xintercept = easter_start, lty = 3) +
  geom_vline(xintercept = easter_end, lty = 3) +
  labs(x = NULL, y = "Daily and weekly incident deaths")  +
  ggsave(paste0(figure_folder, "all-source-deaths-jhu.jpg"), height = 5, width = 10)


# Examples ----------------------------------------------------------------
# Lithuania
grid_day_week %>%
  filter(location_name == "Lithuania") %>%
  plot_by_source(target = "inc_death",
                 location_wrap = FALSE) +
  labs(y = "Daily and weekly incident deaths",
       subtitle = "Lithuania") +
  ggsave(paste0(figure_folder, "lithuania-deaths.jpg"), height = 4, width = 10)  


# All countries daily/weekly plots -------------------------------------------------------------
# Set dates when all sources have data (i.e. ECDC private data)
min_date <- as.Date("2021-02-14")
max_date <- Sys.Date()

# All sources - cases
grid_day_week %>%
  plot_by_source(target = "inc_case") +
  geom_vline(xintercept = easter_start, lty = 3) +
  geom_vline(xintercept = easter_end, lty = 3) +
  labs(x = NULL, y = "Daily and weekly incident cases")  +
  ggsave(paste0(figure_folder, "all-source-cases.jpg"), height = 8, width = 15)


# All sources - deaths
grid_day_week %>%
  plot_by_source(target = "inc_death") +
  geom_vline(xintercept = easter_start, lty = 3) +
  geom_vline(xintercept = easter_end, lty = 3) +
  labs(x = NULL, y = "Daily and weekly incident deaths") +
  ggsave(paste0(figure_folder, "all-source-deaths.jpg"), height = 8, width = 15)  


