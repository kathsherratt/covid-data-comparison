library(sf)
library(scales)
library(patchwork)

missing_by_location <- grid_data %>%
  group_by(source, location, setting, count) %>%
  summarise(n = n(), # either length(full_date_seq) or length(full_week_seq)
            na = sum(is.na(value)),
            complete = 1 - (na / n))

# locations with no data
no_data <- missing_by_location %>%
  group_by(location, source) %>%
  summarise(n = n(),
            max_data_coverage = max(complete))

location_grid <- missing_by_location %>%
  select(-n, -na) %>%
  group_by(setting, count) %>%
  pivot_wider(names_from = source,
              values_from = complete)
write_csv(location_grid, "data_by_source_location.csv")

# general hospital occupancy
missing_by_location %>%
  filter(setting == "Intensive care unit" & 
           source %in% c("JRC", "OWID") &
           !(source == "JRC" & count == "Admissions")) %>%
  ggplot(aes(shape = count,
             colour = source,
             y = location, x = complete)) +
  geom_point(size = 3, alpha = 0.8,
             position = position_jitter(h = 0.2)) +
  labs(colour = NULL, fill = NULL, shape = NULL,
       x = NULL, y = "% complete data",
       caption = "Note: JRC does not publish admissions data",
       subtitle = "ICU data: completeness for hub countries by source and type") +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size = 12))

ggsave("missing-all-location-source-icu.jpg",
       height = 6, width = 7)

# overall missing
missing_overall <- grid_data %>%
  group_by(source, setting, count) %>%
  summarise(n = n(),
            na = sum(is.na(value)),
            complete = 1 - (na / n))

missing_overall %>%
  # filter(!(source == "JRC" & count == "Admissions")) %>%
  # mutate(count = fct_inseq(count, ordered = complete)) %>%
  ggplot(aes(x = count,
             y = complete * 100, 
             fill = source)) +
  geom_col(position = "dodge") +
  scale_fill_viridis_d(option = "D",
                       begin = 0.1,
                       end = 0.6) +
  labs(x = NULL, 
       y = "% available data, 01/01/21 - 19/06/21",
       fill = NULL,
       subtitle = "Data availability by source, setting, and type across 32 European countries") +
  facet_wrap(~ setting) +
  theme_classic() +
  theme(legend.position = "bottom",
        strip.background = element_blank())

ggsave("missing-overall-by-source.jpg",
       height = 5, width = 6)

# Map ---------------------------------------------------------------------
eu_map <- rnaturalearth::ne_countries(returnclass = "sf") %>%
  select(iso_a2) %>%
  filter(iso_a2 %in% pop$location) %>%
  st_crop(xmin = -30, xmax = 45,
          ymin = 30, ymax = 73) %>%
  left_join(missing_by_location,
            by = c("iso_a2" = "location"))

eu_map %>%
  filter(source %in% c("JRC", "OWID")) %>%
  mutate(setting = paste(setting, count)) %>%
  ggplot() +
  geom_sf(aes(fill = complete * 100)) +
  scale_fill_viridis_c(direction = -1) +
  facet_grid(rows = vars(source),
             cols = vars(setting)) +
  labs(fill = "% data availability,
       01/01/21 - 19/06/21") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin(2,2,0,0),
        strip.background = element_blank(),
        text = element_text(size = 12))

ggsave("eu-data-status-owid-jrc.jpg", 
       width = 23, height = 20, units = "cm")


# Diffs -------------------------------------------------------------------

# Differences by source

diffs <- grid_data %>%
  filter(source %in% c("OWID", "JRC")) %>%
  group_by(date, location, setting, count) %>%
  arrange(source, .by_group = TRUE) %>%
  mutate(jrc_diff = value - lag(value, 1))

jrc_diffs <- filter(diffs,  jrc_diff > 10)
median(jrc_diffs$jrc_diff)
unique(jrc_diffs$location)


# Summarise differences over all dates
diff_sum <- jrc_diffs %>%
  group_by(setting, count, location) %>%
  summarise(n_diffs = n(),
            mean_diff = mean(jrc_diff),
            median_diff = median(jrc_diff)
            )

# Dates with differing counts by source
map_diff_n <- rnaturalearth::ne_countries(returnclass = "sf") %>%
  select(iso_a2) %>%
  filter(iso_a2 %in% pop$location) %>%
  st_crop(xmin = -30, xmax = 45,
          ymin = 30, ymax = 73) %>%
  left_join(diff_sum %>%
              filter(setting == "General hospital"),
            by = c("iso_a2" = "location")) %>%
  ggplot() +
  geom_sf(aes(fill = cut_number(n_diffs/170*100,
                                n = 3,
                                labels = c("1<25%",
                                           ">25%",
                                           ">75%")))) +
  scale_fill_viridis_d(option = 7,
                       direction = 1,
                       na.value = "grey") +
  labs(fill = NULL,
       subtitle = "% dates with differing counts") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin(2,2,0,0),
        strip.background = element_blank(),
        text = element_text(size = 12))

# Map median difference in counts between sources
map_diff_median <- rnaturalearth::ne_countries(returnclass = "sf") %>%
  select(iso_a2) %>%
  filter(iso_a2 %in% pop$location) %>%
  st_crop(xmin = -30, xmax = 45,
          ymin = 30, ymax = 73) %>%
  left_join(diff_sum %>%
              filter(setting == "General hospital"),
            by = c("iso_a2" = "location")) %>%
  ggplot() +
  geom_sf(aes(fill = cut_number(median_diff, 
                                n = 3, 
                                labels = c("1<50",
                                           "50<2,000",
                                           ">2,000")))) +
  scale_fill_viridis_d(option = 7,
                       direction = 1,
                       na.value = "grey") +
  labs(fill = NULL,
       subtitle = "Median difference in count") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin(2,2,0,0),
        strip.background = element_blank(),
        text = element_text(size = 12))

# Join maps
map_diff_n +
  map_diff_median +
  plot_layout(nrow = 1) &
  plot_annotation(subtitle = "Daily hospital occupancy",
       caption = "JRC - ECDC data (OWID count conversion), 
01/01/21 - 19/06/21")

ggsave("jrc-differ.jpg", height = 5, width = 8)

# Plot Spain
diffs %>%
  filter(location == "ES" & 
           setting == "General hospital" &
           count == "Occupancy") %>%
  mutate(source_combo = case_when(source == "OWID" ~ "OWID/ECDC",
                                  source == "JRC" ~ "JRC")) %>%
  ggplot(aes(x = date, y = value, colour = source_combo)) +
  geom_line(lwd = 1) +
  labs(x = NULL, y = "Beds occupied",
       colour = NULL,
       subtitle = "Spain, general hospital bed occupancy") +
  scale_colour_viridis_d(begin = 0.1, end = 0.8, direction = -1) +
  scale_y_continuous(labels = comma) +
  theme_classic() +
  theme(legend.position = "bottom")

ggsave("jrc-differ-spain.jpg", height = 5, width = 5)
