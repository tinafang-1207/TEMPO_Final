
#### clean working environment ####
rm(list = ls())

#### read in packages ####
library(tidyverse)
library(factoextra)  # For perform famd visualization
library(FactoMineR) # For perform famd analysis
library(cluster) # For hierarchical clustering
library(psych) # For KMO and Bartlett test

### specify plot directory ###
plotdir <- "figure"

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
  mutate(size_stan_log = log(size_stan_ha+1),
         length_closed_stan_years_log = log(length_closed_stan_years+1),
         length_open_stan_years_log = log(length_open_stan_years+1)) %>%
  select(-size_stan_ha, -length_closed_stan_years, -length_open_stan_years) %>%
  na.omit()

#########################################################################################
# Test data for the assumption of colliniarity

# quantitative variables
quant_variables <-data_famd %>%
  select(size_stan_ha, length_closed_stan_years, length_open_stan_years,
         size_stan_log, length_closed_stan_years_log, length_open_stan_years_log)

# kmo and Barttlett test
kmo_result <- KMO(quant_variables)
print(kmo_result)

bartlett_result <- bartlett.test(quant_variables)
print(bartlett_result)

# multicollinearity
cor_matrix <- cor(quant_variables)
cor_matrix

# qualitative variables
quali_variables <- data_famd %>%
  select(governance_type_design_clean,
         decision_making_clean,
         criteria_open_clean,
         species_category_clean,
         enforcement_clean)


contingency_table_test <- table(quali_variables$governance_type_design_clean, quali_variables$decision_making_clean)
chisq_test_exp <- chisq.test(contingency_table_test, simulate.p.value = TRUE)
chisq_test_exp



chi_squared_results <- list()

categorical_pairs <- combn(names(quali_variables), 2, simplify = FALSE)



for(pair in categorical_pairs) {
  var1 <- pair[1]
  var2 <- pair[2]
  
  # perform chi_squared test
  chisq_test <- chisq.test(table(quali_variables[[var1]], quali_variables[[var2]]))
  
  chi_squared_results[[paste(var1, var2, sep = "_vs_")]] <- chisq_test$p.value
}

chi_squared_results
###############################################################################

# Perform factor analysis of mixed data

design_famd <- FAMD(data_famd, sup.var = 1:4, graph = FALSE, ncp = 10)

design_eig <- get_eigenvalue(design_famd)

# screeplots of eigenvalues
eig_values <- design_famd$eig[,1]
dimensions <- seq_along(eig_values)
scree_data <- data.frame(Dimension = dimensions, Eigenvalue = eig_values)

ggplot(scree_data, aes(x = Dimension, y = Eigenvalue)) +
  geom_line() +
  geom_hline(yintercept = mean(scree_data$Eigenvalue))+
  geom_point() +
  theme_minimal()



# screeplots of the two dimensions
fviz_contrib(design_famd, "var", axes = 1)
fviz_contrib(design_famd, "var", axes = 2)


# visualize individual case


g_indv <- fviz_famd_ind(design_famd, col.ind = "cos2", gradient.cols = c("#00afbb", "#e7b800", "#fc4e07"),
              repel = TRUE)

g_indv


####################################################################
# Draw ellipse around the scattered point

ind <- design_famd$ind

ind_coord <- as.data.frame(ind[["coord"]]) %>%
  select(-Dim.3, -Dim.4, -Dim.5, -Dim.6, -Dim.7, -Dim.8, -Dim.9, -Dim.10) %>%
  rename(coord_dim1 = Dim.1,
         coord_dim2 = Dim.2)

ind_cos2 <- as.data.frame(ind[["cos2"]]) %>%
  select(-Dim.3, -Dim.4, -Dim.5,-Dim.6, -Dim.7, -Dim.8, -Dim.9, -Dim.10) %>%
  rename(cos_dim1 = Dim.1,
         cos_dim2 = Dim.2)

ind_contrib <- as.data.frame(ind[["contrib"]]) %>%
  select(-Dim.3, -Dim.4, -Dim.5, -Dim.6, -Dim.7, -Dim.8, -Dim.9, -Dim.10) %>%
  rename(contrib_dim1 = Dim.1,
         contrib_dim2 = Dim.2)

ind_total <- cbind(ind_coord, ind_cos2, ind_contrib) %>%
  mutate(cluster = case_when(
    rownames(ind_total) %in% c(27,28,12,14,13,15,3,4,2,5,25,7,11,6,9,10,8,20,21)~"A",
    rownames(ind_total) %in% c(16,38,37,39,18,36,1,17,24,19,22)~"B",
    rownames(ind_total) %in% c(29,32,33,30,31,34,35)~"C",
    rownames(ind_total) %in% c(40,50,23,41,44,42,43)~"D",
    rownames(ind_total) %in% c(45,46)~"E",
    rownames(ind_total) %in% c(49,47,48)~"F"
  ))

# make plot to show the individual group in barplot
point_color <- c("A"="#00468b", "B"="#ed0000", "C"="#42b540", "D"="#0099b4", "E"="#925e9f", "F"="#fdaf91")

ind_barplot <- ggplot(data = ind_total, aes(x = coord_dim1, y = coord_dim2, color = cluster)) + 
  geom_point(size = 0.8) +
  scale_color_manual(name = "Cluster", values = point_color) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  labs(x = "Dim 1", y = "Dim 2") +
  theme_bw() + theme(legend.position = "none")

ind_barplot





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

rect.hclust(hc, k = 5, border = "red")

# calculate sillhouette scores
# the cluster of 4 gives the clusters in 0.5, which is the acceptable threshold for clear clusters
clusters <- cutree(hc, k = 5)
silhouette_score <- silhouette(clusters, dist(factor_scores))
plot(silhouette_score)

# visualize the cluster in dendrogram
g1 <- fviz_dend(hc, k = 6,
          cex = 0.5, # label size
          k_colors= c("#00468b", "#ed0000", "#42b540", "#0099b4", "#925e9f", "#fdaf91"),
          color_labels_by_k = TRUE,
          rect = TRUE)

g1

# visualize the cluster in scatter plot
g2 <- fviz_cluster(list(data = factor_scores, cluster = clusters),
             palette = c("#00468b", "#ed0000", "#42b540", "#0099b4", "#925e9f", "#fdaf91"),
             ellipse.type = "convex",
             repel = TRUE,
             show.cluster.cent = FALSE)

g2

## save the cluster plot (preliminary)
ggsave(g1, filename = file.path(plotdir, "Figx_dendrogram_cluster.png"), width = 5, height = 3, units = "in", dpi = 600)

ggsave(g2, filename = file.path(plotdir, "Figx_scatter_cluster.png"), width = 4, height = 3, units = "in", dpi = 600)

ggsave(ind_barplot, filename = file.path(plotdir, "Figx_individual_cluster.png"), width = 5, height = 3, units = "in", dpi = 600 )

######################################################################








