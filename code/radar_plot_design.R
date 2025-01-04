
### clean working environment ###
rm(list = ls())

### load in packages ###
library(tidyverse)
library(ggradar)

### specify plot directory ###
plotdir <- "figure"

### read in data ###
empirical <- read.csv("data/Cleaned sheets - Full-text screening - Emperical papers_Context_Design_Final.csv", na.strings = "/") %>%
  janitor::clean_names()

#### clean empirical data ####
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
         length_open_stan_years = length_of_time_open_range_standardized_to_years,
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
         length_open_stan_years,
         length_open_bin,
         percentage_fishing_ground_closed_clean,
         number_of_species_clean,
         species_category_clean,
         enforcement_clean,
         information_collected_clean) %>%
  # correct the syntex error
  mutate(short_type_of_closure_clean = ifelse(short_type_of_closure_clean == "Dynamic/Triggered Closure", "Dynamic/Triggered closure", short_type_of_closure_clean))

# Assign the cluster to observations based on the cluster analysis
empirical_group <- empirical_clean %>%
  mutate(cluster = case_when(
    row_number() %in% c(23,40,41,50,44,42,43)~"cluster 1",
    row_number() %in% c(16,22,1,38,37,39,18,36,17,24)~"cluster 2",
    row_number() %in% c(29,34,35)~"cluster 3",
    row_number() %in% c(12,28,8,20,21)~"cluster 4",
    row_number() %in% c(46,45,49,47,48)~"cluster 5",
    row_number() %in% c(25,32,33,30,31,14,27,13,15,3,4,2,5,9,19,6,10,7,11)~"cluster 6"
  ))

##############################################################################
#Do an experimental plot of motivation

motivation <- empirical_group %>%
  filter(!is.na(cluster)) %>%
  select(motivations_clean, cluster) %>%
  mutate(motivations_clean = ifelse(motivations_clean == "Unknown", "Social", motivations_clean)) %>%
  separate(motivations_clean, into = c("moti_first", "moti_second"), sep = ";", fill = "right") %>%
  mutate(moti_second = case_when(moti_second == " Social"~"Social",
                                 moti_second == " Ecosystem/Conservation"~"Ecosystem/Conservation",
                                 moti_second == " Others"~"Others"))

motivation_summarize <- motivation %>%
  pivot_longer(cols = c(moti_first, moti_second), names_to = "motivation_type", values_to = "motivation") %>%
  group_by(cluster, motivation) %>%
  summarise(motivation_count = n(), .groups = "drop") %>%
  pivot_wider(names_from = motivation, values_from = motivation_count, values_fill = list(motivation_count = 0)) %>%
  select(-"NA") %>%
  mutate(case_total = rowSums(across("Ecosystem/Conservation":"Social")))

# calculate percentage
motivation_percent <- motivation_summarize %>%
  mutate(eco_percentage = round((`Ecosystem/Conservation`/case_total)*100),
         fish_percentage = round((`Fisheries management/Fishing income`/case_total)*100),
         gov_percentage = round((Governance/case_total)*100),
         other_percentage = round((Others/case_total)*100),
         social_percentage = round((Social/case_total)*100)) %>%
  select(-`Ecosystem/Conservation`,
         -`Fisheries management/Fishing income`,
         -`Governance`,
         -`Others`,
         -`Social`,
         -`case_total`)


ggradar(
  motivation_percent, 
  values.radar = c("0", "50", "100"),
  grid.min = 0, grid.mid = 50, grid.max = 100,
  # Polygons
  group.line.width = 1, 
  group.point.size = 3,
  group.colours = c("#00468b", "#ed0000", "#42b540", "#0099b4", "#925e9f", "#fdaf91"),
  # Background and grid lines
  background.circle.colour = "white",
  gridline.mid.colour = "grey",
  legend.position = "bottom"
)











