
#### clean working environment ####
rm(list = ls())

#### read in packages ####
library(tidyverse)
library(cluster)  # For clustering algorithms, including Gower distance
library(factoextra)  # For visualization of clustering
library(Rtsne)

#### read in data ####
empirical <- read.csv("data/Cleaned sheets - Full-text screening - Emperical papers_Context_Design_Final.csv", na.strings = "/") %>%
  janitor::clean_names()

#### clean empirical data ####
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

#### Select the variables for clustering ####
data_cluster <- empirical_clean %>%
  select(governance_type_design_clean, 
         decision_making_clean, 
         size_bin, 
         length_closed_bin, 
         length_open_bin, 
         enforcement_clean, 
         information_collected_clean) %>%
  mutate(length_open_bin = case_when(length_open_bin == "Daily opening; Quarterly opening"~"Daily opening",
                                     .default = length_open_bin)) %>%
  # convert to factors
  mutate(governance_type_design_clean = as.factor(governance_type_design_clean),
         decision_making_clean = as.factor(decision_making_clean),
         size_bin = as.factor(size_bin),
         length_open_bin = as.factor(length_open_bin),
         length_closed_bin = as.factor(length_closed_bin),
         enforcement_clean = as.factor(enforcement_clean),
         information_collected_clean = as.factor(information_collected_clean))


# calculate gower distance
gower_dist <- daisy(data_cluster, metric = "gower")
gower_dist_mx <- as.matrix(gower_dist)

tsne_result <- Rtsne(gower_dist_mx, is_distance = TRUE, perplexity = 10)

# Create a data frame with t-SNE results
tsne_data <- as.data.frame(tsne_result$Y)
colnames(tsne_data) <- c("tSNE1", "tSNE2")
data_cluster$continent_clean <- empirical_clean$continent_clean
data_cluster$x <- tsne_data$tSNE1
data_cluster$y <- tsne_data$tSNE2


# Plot t-SNE results
ggplot(data_cluster) +
  geom_point(aes(x = x, y = y,  color = continent_clean), size = 3, alpha = 0.7) +
  labs(x = "t-SNE Dimension 1",
       y = "t-SNE Dimension 2") +
  theme_minimal()



