# France only

# Set up ------------------------------------------------------------------
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

# Get data
source("R/health-data.R")

france_weekly <- filter(health_weekly, location_name == "France") %>%
  bind_rows(ecdc_public_inc %>%
              filter(location_name == "France")) %>%
  mutate(target_variable = recode(target_variable,
                                  "inc_hosp" = "Hospital admissions",
                                  "inc_icu" = "ICU admissions",
                                  "prev_hosp" = "Hospital occupancy",
                                  "prev_icu" = "ICU occupancy")) %>%
  filter(date > Sys.Date() - weeks(12))


# Plot --------------------------------------------------------------------

ggplot(france_weekly, 
       aes(x = date, y = value_weekly, 
           colour = source, fill = source)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ target_variable, scales = "free_y") +
  scale_fill_manual(values = source_colours) +
  scale_colour_manual(values = source_colours) +
  labs(x = NULL, y = "Weekly count",
       caption = "Admissions
       Data available: ECDC-private; ECDC-public-weekly
       ECDC-public-weekly is transformed from a rate per 100k population to a count.
       Hence ECDC-public-weekly does not exactly equal 7-day sum of ECDC-private.
       
       Occupancy
       Data available: ECDC-private; ECDC-public-daily; JRC
       All available data are identical counts") +
  theme_classic() +
  theme(legend.position = "bottom",
        strip.background = element_blank())

ggsave("figures/with-private/france.jpg", height = 5, width = 5)

