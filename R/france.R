# France only
library(ggplot2)
library(vroom)
library(dplyr)
library(here)
library(lubridate)
library(tidyr)

# Set up ------------------------------------------------------------------
source_colours <- c(
  "WHO" = "#377eb8",
  "JRC" = "#4daf4a",
  "JHU" = "#984ea3",
  "ECDC-public" = "#ff7f00",
  "ECDC-private" = "#e41a1c",
  "ECDC-public-daily" = "#ff7f00",
  "ECDC-public-weekly" = "#FFD700",
  "Sante Publique France" = "#0000FF"
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

# Add France national data
frtest <- vroom(here("public-data", 
                          "donnees-hospitalieres-nouveaux-covid19-2021-05-16-19h08.csv"))

fr_csv <- tempfile(fileext = ".csv")
download.file("https://www.data.gouv.fr/fr/datasets/r/6fadff46-9efd-4c53-942a-54aca783c30c",
              fr_csv)
fr_csv <- vroom(fr_csv, delim = ";") 
fr_national <- fr_csv %>%
  mutate(date = ymd(jour),
         epiweek = epiweek(date),
         epiyear = epiyear(date)) %>%
  group_by(epiweek, epiyear) %>%
  summarise(date = max(date),
            `Hospital admissions` = sum(incid_hosp),
            `ICU admissions` = sum(incid_rea)) %>%
  pivot_longer(cols = -c(date, epiweek, epiyear), 
               names_to = "target_variable",
               values_to = "value_weekly") %>%
  mutate(source = "Sante Publique France")

# Match French national data with ECDC/JRC data
france_incident <- france_weekly %>%
  filter(grepl("admissions", .$target_variable)) %>%
  bind_rows(filter(fr_national, 
                 date %in% france_weekly$date))
  
  
  

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

# ggsave("figures/with-private/france.jpg", height = 5, width = 5)

# Incidence

ggplot(france_incident, 
       aes(x = date, y = value_weekly, 
           colour = source, fill = source)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ target_variable, scales = "free_y") +
  scale_fill_manual(values = source_colours) +
  scale_colour_manual(values = source_colours) +
  labs(x = NULL, y = "Weekly count",
       caption = "Admissions
       Data available:
       ECDC-private is identical to Sante Publique France (SPF; in blue).
       ECDC-public-weekly is transformed from a rate per 100k population to a count."
       ) +
  theme_classic() +
  theme(legend.position = "bottom",
        strip.background = element_blank())

ggsave("figures/with-private/france-incidence.jpg", height = 5, width = 5)