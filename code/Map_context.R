
# clean working environment
rm(list = ls())

# load in packages
library(tidyverse)

# read in data
emperical <- read.csv("data/Cleaned sheets - Full-text screening - Emperical papers_Context_Design_Final.csv", na.strings = "/") %>%
  janitor::clean_names()

modeling <- read.csv("data/Cleaned sheets - Full-text screening - Modeling papers.csv", na.strings = "/") %>%
  janitor::clean_names()

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
         country, 
         continent_clean, 
         paper_number_clean, 
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
  select()








  