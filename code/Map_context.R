
# clean working environment
rm(list = ls())

# load in packages
library(tidyverse)

# read in data
emperical <- read.csv("data/Cleaned sheets - Full-text screening - Emperical papers_Context_Design_Final.csv", na.strings = "/") %>%
  janitor::clean_names()

modeling <- read.csv("data/Cleaned sheets - Full-text screening - Modeling papers.csv", na.strings = "/") %>%
  janitor::clean_names()

country_location <- read.csv("data/country_location.csv")

# clean data

# emperical data
emperical_clean <- emperical %>%
  # remove the first column and the first row
  select(-x) %>%
  slice(-1) %>%
  # rename the column 
  rename(case_studied_clean = number_of_closures_studied_in_case_paper_if_paper_is_one_case,
         actors_type_governance_clean = x_actors_type_governance_clean,
         size_stan_ha = size_of_closure_standardized_to_ha_1_km2_100_ha,
         size_bin = size_of_closure_clean,
         length_closed_stan_years = length_of_time_closed_range_standardized_to_years,
         length_closed_bin = length_close_clean,
         length_open_stan_days = length_of_time_open_range_standardized_to_days,
         length_open_bin = length_open_clean
         ) %>%
  # keep only the cleaned column (remove the original quote column)
  select(source, # context
         country_clean, 
         location_clean, 
         continent_clean,
         types_of_gear_used_clean, 
         case_studied_clean, 
         goverance_type_overall_clean,
         actors_type_governance_clean,
         traditional_history_clean,
         start_date_clean,
         turf_clean,
         ecosystem_clean,
         short_type_of_closure_clean, # design
         governance_type_design_clean,
         actors_type_design_clean,
         motivations_clean,
         criteria_open_clean,
         decision_making_clean,
         size_stan_ha,
         size_bin,
         length_closed_stan_years,
         length_closed_bin,
         length_open_stan_days,
         length_open_bin,
         percentage_fishing_ground_closed_clean,
         number_of_species_clean,
         species_category_clean,
         enforcement_clean,
         information_collected_clean)

# modeling data
modeling_clean <- modeling %>%
  select(ref_id, 
         country_clean,
         location_clean,
         continent_clean, 
         case_number_clean, 
         fishery_data_type_clean, 
         closure_data_type_clean,
         model_target_clean,
         model_target_type_clean,
         objectives_scenarios_clean,
         objectives_summarized_clean,
         evaluation_metrics_clean)

# further clean the data to make map

# emperical 
emperical_map <- emperical_clean %>%
  select(source,country_clean, location_clean, location_clean, case_studied_clean) %>%
  mutate(case_studied_clean = 1) %>%
  # clean country name
  mutate(country_clean = case_when(country_clean == "Madagascar "~"Madagascar",
                                   country_clean == "Papua New Guinea "~"Papua New Guinea",
                                   country_clean == "Norway, and countries in EU (Danish, Swedish, German, Dutch, Belgium)"~"Norway",
                                   country_clean == "French Polynesia "~"French Polynesia",
                                   country_clean == "Soloman Islands"~"Solomon Islands",
                                   .default = country_clean)) %>%
  # differentiate Chile(Easter Island), U.S. (Hawaii) and U.S.(Alaska)
  mutate(country_clean = case_when(location_clean == "Hawaii"~"U.S.(Hawaii)",
                                   location_clean == "Hawaii, Waikiki"~"U.S.(Hawaii)",
                                   location_clean == "Alaska"~"U.S.(Alaska)",
                                   location_clean == "Easter Island, Rapa Nui"~"Chile(Easter Island)",
                                   .default = country_clean)) %>%
  group_by(country_clean) %>%
  summarize(case_studied_total = sum(case_studied_clean)) %>%
  # combine with the country geographic locations
  left_join(country_location, by = "country_clean") %>%
  # add paper type
  mutate(paper_type = "Empirical")

# modeling
modeling_map <- modeling_clean %>%
  filter(!is.na(country_clean)) %>%
  select(ref_id, country_clean, location_clean, case_number_clean) %>%
  mutate(country_clean = case_when(country_clean == "U.S. (Guam)"~"U.S.(Guam)",
                                   .default = country_clean)) %>%
  group_by(country_clean) %>%
  summarize(case_studied_total = sum(case_number_clean)) %>%
  # combine with the country geographic location
  left_join(country_location, by = "country_clean") %>%
  # add paper type
  mutate(paper_type = "Modeling")

# Combine the two dataframes
map_total <- bind_rows(emperical_map, modeling_map)


# read in the world map
world <- map_data("world") %>%
  filter(region != "Antarctica")

##############################################################
# Make figure below

# Set theme
world_theme <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5),
  legend.key = element_rect(fill = NA, color=NA),
  legend.background = element_rect(fill=alpha('blue', 0)),
  legend.title = element_text(size = 5),
  legend.text = element_text(size = 5)
)


worldplot <- ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "gray") +
  geom_point(data = map_total, aes(x = country_long, y = country_lat, color = paper_type, size = case_studied_total, alpha = 0.5)) +
  geom_text(data = map_total, mapping = aes(x = country_long, y = country_lat, label = country_clean, hjust = hjust, vjust = vjust), size = 2.5) +
  coord_fixed(1.3) +
  world_theme + theme(legend.position = "none")

worldplot

#################################################################
# Pie chart

# emperical_cases <- emperical_clean %>%
#   select(source,country_clean, location_clean, location_clean, case_studied_clean) %>%
#   mutate(case_studied_clean = 1) %>%
#   # clean country name
#   mutate(country_clean = case_when(country_clean == "Madagascar "~"Madagascar",
#                                    country_clean == "Papua New Guinea "~"Papua New Guinea",
#                                    country_clean == "Norway, and countries in EU (Danish, Swedish, German, Dutch, Belgium)"~"Norway",
#                                    country_clean == "French Polynesia "~"French Polynesia",
#                                    country_clean == "Soloman Islands"~"Solomon Islands",
#                                    .default = country_clean)) %>%
#   # differentiate Chile(Easter Island), U.S. (Hawaii) and U.S.(Alaska)
#   mutate(country_clean = case_when(location_clean == "Easter Island, Rapa Nui"~"Chile(Easter Island)",
#                                    .default = country_clean)) %>%
#   group_by(country_clean) %>%
#   summarize(case_studied_total = sum(case_studied_clean))
# 
# modeling_cases <- modeling_clean %>%
#   filter(!is.na(country_clean)) %>%
#   select(ref_id, country_clean, location_clean, case_number_clean) %>%
#   mutate(country_clean = case_when(country_clean == "U.S. (Guam)"~"United States",
#                                    .default = country_clean)) %>%
#   group_by(country_clean) %>%
#   summarize(case_studied_total = sum(case_number_clean))
# 
# 
# total_cases <- bind_rows(emperical_cases, modeling_cases) %>%
#   group_by(country_clean) %>%
#   summarize(case_studied_total = sum(case_studied_total))
  




  