
# clean working environment
rm(list = ls())

# load in packages
library(tidyverse)

# set plot directory
plotdir <- "figures"

# read in data
empirical <- read.csv("data/Cleaned sheets - Full-text screening - Emperical papers_Context_Design_Final.csv", na.strings = "/") %>%
  janitor::clean_names()

modeling <- read.csv("data/Cleaned sheets - Full-text screening - Modeling papers.csv", na.strings = "/") %>%
  janitor::clean_names()


# clean data

# empirical data
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


# further clean the data to make bar chart
empirical_continent <- empirical_clean%>%
  select(continent_clean) %>%
  group_by(continent_clean) %>%
  summarize(continent_cases = n())

modeling_continent <- modeling_clean %>%
  select(continent_clean) %>%
  group_by(continent_clean) %>%
  summarize(continent_cases = n()) %>%
  mutate(continent_clean = ifelse(is.na(continent_clean), "Simulated studies", continent_clean))

total_cases <- bind_rows(empirical_continent, modeling_continent) %>%
  group_by(continent_clean) %>%
  summarize(total_cases = sum(continent_cases))




empirical_bar <- empirical_clean %>%
  select(continent_clean, short_type_of_closure_clean) %>%
  group_by(continent_clean, short_type_of_closure_clean) %>%
  summarize(continent_per_closure_type = n()) %>%
  mutate(short_type_of_closure_clean = ifelse(short_type_of_closure_clean == "Dynamic/Triggered Closure", "Dynamic/Triggered closure", short_type_of_closure_clean)) %>%
  group_by(continent_clean, short_type_of_closure_clean) %>%
  summarize(continent_per_closure_type = sum(continent_per_closure_type)) %>%
  mutate(source = "Empirical")

modeling_bar <- modeling_clean %>%
  select(continent_clean, objectives_scenarios_clean) %>%
  mutate(continent_clean = ifelse(is.na(continent_clean), "Simulated studies", continent_clean)) %>%
  group_by(continent_clean, objectives_scenarios_clean) %>%
  summarize(continent_per_closure_type = n()) %>%
  mutate(source = "Modeling") %>%
  rename(short_type_of_closure_clean = objectives_scenarios_clean)

total_bar <- bind_rows(empirical_bar, modeling_bar) %>% 
  left_join(total_cases, by = "continent_clean") %>%
  select(continent_clean, source, short_type_of_closure_clean, continent_per_closure_type, total_cases) %>%
  mutate(percentage = (continent_per_closure_type/total_cases)*100) %>%
  mutate(percentage = round(percentage, 0)) %>%
  mutate(label = paste0(percentage, "%")) %>%
  mutate(label_closure_type = paste0("n=", total_cases))
















