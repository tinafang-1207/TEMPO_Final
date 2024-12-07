
# clean working environment
rm(list = ls())

# load in packages
library(tidyverse)
library(scatterpie)

# read in data
empirical <- read.csv("data/Cleaned sheets - Full-text screening - Emperical papers_Context_Design_Final.csv", na.strings = "/") %>%
  janitor::clean_names()

modeling <- read.csv("data/Cleaned sheets - Full-text screening - Modeling papers.csv", na.strings = "/") %>%
  janitor::clean_names()

country_location <- read.csv("data/country_location.csv")

# clean data

# emperical data
empirical_clean <- empirical %>%
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
empirical_count <- empirical_clean %>%
  select(source,country_clean, location_clean, location_clean, case_studied_clean) %>%
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
  # Madagascar should be 36 - the other 3 cases from different papers but focus on the same region
  mutate(case_studied_total = ifelse(country_clean == "Madagascar", 36, case_studied_total)) %>%
  # add paper type
  mutate(paper_type = "Empirical")

# modeling
modeling_count <- modeling_clean %>%
  filter(!is.na(country_clean)) %>%
  select(ref_id, country_clean, location_clean, case_number_clean) %>%
  mutate(country_clean = case_when(country_clean == "U.S. (Guam)"~"U.S.(Guam)",
                                   .default = country_clean)) %>%
  group_by(country_clean) %>%
  summarize(case_studied_total = sum(case_number_clean)) %>%
  # combine with the country geographic location
  # add paper type
  mutate(paper_type = "Modeling")

# calculate total modeling & empirical cases
total_cases <- bind_rows(empirical_map, modeling_map) %>%
  group_by(country_clean) %>%
  summarize(total_cases = sum(case_studied_total)) %>%
  left_join(empirical_count, by = "country_clean") %>%
  rename(case_studied_empirical = case_studied_total) %>%
  select(-paper_type) %>%
  left_join(modeling_count, by = "country_clean") %>%
  rename(case_studied_modeling = case_studied_total) %>%
  select(-paper_type) %>%
  # change the na to 0 for empirical/modeling paper
  mutate(case_studied_empirical=ifelse(is.na(case_studied_empirical),0,case_studied_empirical)) %>%
  mutate(case_studied_modeling = ifelse(is.na(case_studied_modeling),0,case_studied_modeling))

# calculate percentage of empirical and modeling cases
total_cases_percentage <- total_cases %>%
  mutate(percentage_empirical = (case_studied_empirical/total_cases)*100) %>%
  mutate(percentage_empirical = round(percentage_empirical,0)) %>%
  mutate(percentage_modeling = (case_studied_modeling/total_cases)*100) %>%
  mutate(percentage_modeling = round(percentage_modeling,0)) %>%
  #join with the country location in lat/long
  left_join(country_location, by = "country_clean") %>%
  #set up the radius
  mutate(radius = case_when(total_cases>=1 & total_cases <10~3,
                            total_cases>=10 & total_cases<20~6,
                            total_cases>=20 & total_cases<30~9,
                            total_cases>=30 & total_cases<40~12))


##############################################################

# read in the world map
world <- map_data("world") %>%
  filter(region != "Antarctica")

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
  geom_scatterpie(data = total_cases_percentage,
                  aes(x = country_long, y = country_lat,r=radius),
                  cols = c("percentage_empirical", "percentage_modeling"),
                  color = NA)+
  coord_fixed(1.3) +
  theme_bw() + world_theme + theme(legend.position = "bottom")

worldplot

