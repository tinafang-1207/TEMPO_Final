
#### clean working environment ####
rm(list = ls())

#### read in packages ####
library(tidyverse)
library(factoextra)  # For perform famd visualization
library(FactoMineR) # For perform famd analysis
library(cluster) # For hierarchical clustering

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

#### Select the variables for FAMD ####
data_famd <- empirical_clean %>%
  select(source, #Supplementary variable (not involved in mca analysis)
         country_clean, #Supplementary variable
         continent_clean, #Supplementary variable
         short_type_of_closure_clean,
         governance_type_design_clean,
         decision_making_clean,
         criteria_open_clean,
         size_stan_ha, 
         length_closed_stan_years,
         length_open_stan_years,
         species_category_clean, 
         enforcement_clean) %>%
  # clean the variable categories
  mutate(governance_type_design_clean = ifelse(governance_type_design_clean == "Co-managment", "Co-management", governance_type_design_clean))%>%
  mutate(decision_making_clean = ifelse(decision_making_clean == "Community village led", "Community/village led", decision_making_clean)) %>%
  mutate(enforcement_clean = ifelse(enforcement_clean == "Goverment led", "Government led", enforcement_clean)) %>%
  mutate(species_category_clean = ifelse(species_category_clean == "Finfish", "Reef fish", species_category_clean)) %>%
  # change the variable category labels
  mutate(governance_type_design_clean = case_when(governance_type_design_clean == "Bottom-up"~"B",
                                                  governance_type_design_clean == "Co-management"~"C",
                                                  governance_type_design_clean == "Top-down"~"T")) %>%
  mutate(decision_making_clean = case_when(decision_making_clean == "Community/village led"~"CL",
                                           decision_making_clean == "Collaboration community-government"~"CGC",
                                           decision_making_clean == "Government led"~"GL",
                                           decision_making_clean == "Collaboration community-NGO"~"CNC",
                                           decision_making_clean == "Others (Academia institution led)"~"O")) %>%
  mutate(criteria_open_clean = case_when(criteria_open_clean == "Social"~"S",
                                         criteria_open_clean == "Fisheries management/Fishing income"~"FM/FI",
                                         criteria_open_clean == "Governance"~"G",
                                         criteria_open_clean == "Ecosystem/Conservation"~"E/C")) %>%
  mutate(species_category_clean = case_when(species_category_clean == "Reef fish"~"RF",
                                            species_category_clean == "Pelagic fish"~"PF",
                                            species_category_clean == "Invertebrates"~"IN",
                                            species_category_clean == "Marine mammals/sea turtles/seabirds"~"MM",
                                            species_category_clean == "Demersal fish"~"DF")) %>%
  mutate(enforcement_clean = case_when(enforcement_clean == "Community/village led"~"CL",
                                       enforcement_clean == "Collaboration community-government"~"CGC",
                                       enforcement_clean == "Government led"~"GL",
                                       enforcement_clean == "No enforcement"~"NE",
                                       enforcement_clean == "Collaboration community-NGO"~"CNC",
                                       enforcement_clean == "NGO led"~"NL")) %>%
  mutate(size_stan_ha = as.numeric(gsub(",", "", size_stan_ha)),
         length_closed_stan_years = as.numeric(gsub(",","", length_closed_stan_years)),
         length_open_stan_years = as.numeric(gsub(",","", length_open_stan_years))) %>%
  na.omit() %>%
  mutate(size_stan_log = log(size_stan_ha+1),
         length_closed_stan_years_log = log(length_closed_stan_years+1),
         length_open_stan_years_log = log(length_open_stan_years+1))%>%
  select(-size_stan_ha, -length_closed_stan_years, -length_open_stan_years, -decision_making_clean, -enforcement_clean, -criteria_open_clean, -species_category_clean)

# The ideal result contains only government_type_design_clean, size_stan_log, length_closed_stan_years_log, length_open_stan_years_log

# Perform factor analysis of mixed data

design_famd <- FAMD(data_famd, sup.var = 1:4, graph = FALSE, ncp = 5)

eig_val <- get_eigenvalue(design_famd)

fviz_famd_var(design_famd, repel = TRUE)

# screeplots of the two dimensions
fviz_contrib(design_famd, "var", axes = 1)
fviz_contrib(design_famd, "var", axes = 2)

# visualize quantitative variables
quanti_var <- get_famd_var(design_famd, "quanti.var")
quanti_var

fviz_famd_var(design_famd, "quanti.var", repel = TRUE, col.var = "black")

# visualize individual case


fviz_famd_ind(design_famd, col.ind = "cos2", gradient.cols = c("#00afbb", "#e7b800", "#fc4e07"),
              repel = TRUE)

# fviz_mfa_ind(design_famd, 
#              habillage = "governance_type_design_clean",
#              palette = c("#00AFBB", "#E7B800", "#FC4E07"), 
#              addEllipse = TRUE, ellipse.type = "confidence",
#              repel = TRUE)

# plot the individual clusters grouped by supplementary variable
# ind <- get_famd_ind(design_famd)
# 
# ind_coord <- as.data.frame(ind[["coord"]]) %>%
#   select(-Dim.3, -Dim.4, -Dim.5) %>%
#   rename(coord_dim1 = Dim.1,
#          coord_dim2 = Dim.2)
# 
# ind_cos2 <- as.data.frame(ind[["cos2"]]) %>%
#   select(-Dim.3, -Dim.4, -Dim.5) %>%
#   rename(cos_dim1 = Dim.1,
#          cos_dim2 = Dim.2)
# 
# ind_contrib <- as.data.frame(ind[["contrib"]]) %>%
#   select(-Dim.3, -Dim.4, -Dim.5) %>%
#   rename(contrib_dim1 = Dim.1,
#          contrib_dim2 = Dim.2)
# 
# ind_total <- cbind(ind_coord, ind_cos2, ind_contrib) %>%
#   mutate(case_num = rownames(ind_total)) %>%
#   select(case_num, everything()) %>%
#   cbind(data_famd)
# 
# # color by type of closure
# ggplot(data = ind_total, aes(x = coord_dim1, y = coord_dim2, color = short_type_of_closure_clean)) +
#   geom_point()
# 
# # color by continent
# ggplot(data = ind_total, aes(x = coord_dim1, y = coord_dim2, color = continent_clean)) +
#   geom_point()

##########################################################################
# Hierarchical clustering below (dendrogram)
# the cluster distance is the factor score - the coordinates of each observation after the FAMD

# extract the factor scores
factor_scores <- design_famd$ind$coord

# calculate the distance, using ward.2D
# ward.2D is the most commonly used method for linkage
distance_matrix <- dist(factor_scores)

hc <- hclust(distance_matrix, method = "ward.D2")
plot(hc)

rect.hclust(hc, k = 6, border = "red")

# calculate sillhouette scores
# the cluster of 4 gives the clusters in 0.5, which is the acceptable threshold for clear clusters
clusters <- cutree(hc, k = 6)
silhouette_score <- silhouette(clusters, dist(factor_scores))
plot(silhouette_score)

# visualize the cluster in dendrogram
fviz_dend(hc, k = 6,
          cex = 0.5, # label size
          k_colors= c("#e9c39b", "#ea6b73", "#6ba3d6", "#ac613c", "#f02720", "#2c69b0"),
          color_labels_by_k = TRUE,
          rect = TRUE)

# visualize the cluster in scatter plot
data_cluster <- data_famd %>%
  select(governance_type_design_clean, size_stan_log, length_closed_stan_years_log, length_open_stan_years_log) %>%
  mutate(governance_type_design_clean = as.numeric(factor(governance_type_design_clean)))

fviz_cluster(list(data = factor_scores, cluster = clusters),
             palette = c("#e9c39b", "#ea6b73", "#6ba3d6", "#ac613c", "#f02720", "#2c69b0"),
             ellipse.type = "convex",
             repel = TRUE,
             show.cluster.cent = FALSE, 
             ggtheme = theme_minimal())

######################################################################








