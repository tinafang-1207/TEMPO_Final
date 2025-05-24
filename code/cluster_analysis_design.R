
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
empirical <- read.csv("data/Cleaned sheets - Full-text screening - Empirical papers_FINAL.csv", na.strings = "/")

# remove the first row and make the second row as column name
colnames(empirical) <- empirical %>% 
  slice(1) %>%
  unlist() %>%
  as.character()

data_orig <- empirical %>%
  slice(-1) %>%
  janitor::clean_names() %>%
  select(-number)

#### clean empirical data ####

data_famd <- data_orig %>%
  select(ref_id, 
         country_clean,
         governance_type_design_clean, 
         criteria_open_clean,
         decision_making_clean,
         closure_size_ha,
         time_closed_years,
         time_open_years,
         species_category_clean,
         enforcement_clean) %>%
  # standardize the quantitative columns
  mutate(closure_size_ha = as.numeric(gsub(",", "", closure_size_ha)),
         time_closed_years = as.numeric(gsub(",", "", time_closed_years)),
         time_open_years = as.numeric(gsub(",", "", time_open_years))) %>%
  mutate(closure_size_log = log(closure_size_ha + 1),
         time_closed_log = log(time_closed_years + 1),
         time_open_log = log(time_open_years + 1)) %>%
  select(-closure_size_ha, -time_closed_years, -time_open_years) %>%
  na.omit()
  
# #### Select the variables for FAMD ####
# data_famd <- empirical_clean %>%
#   select(source, #Supplementary variable (not involved in mca analysis)
#          country_clean, #Supplementary variable
#          continent_clean, #Supplementary variable
#          short_type_of_closure_clean,
#          governance_type_design_clean,
#          decision_making_clean,
#          criteria_open_clean,
#          size_stan_ha, 
#          length_closed_stan_years,
#          length_open_stan_years,
#          species_category_clean, 
#          enforcement_clean) %>%
#   # clean the variable categories
#   mutate(governance_type_design_clean = ifelse(governance_type_design_clean == "Co-managment", "Co-management", governance_type_design_clean))%>%
#   mutate(decision_making_clean = ifelse(decision_making_clean == "Community village led", "Community/village led", decision_making_clean)) %>%
#   mutate(enforcement_clean = ifelse(enforcement_clean == "Goverment led", "Government led", enforcement_clean)) %>%
#   mutate(species_category_clean = ifelse(species_category_clean == "Finfish", "Reef fish", species_category_clean)) %>%
#   # change the variable category labels
#   mutate(governance_type_design_clean = case_when(governance_type_design_clean == "Bottom-up"~"B",
#                                                   governance_type_design_clean == "Co-management"~"C",
#                                                   governance_type_design_clean == "Top-down"~"T")) %>%
#   mutate(decision_making_clean = case_when(decision_making_clean == "Community/village led"~"CL",
#                                            decision_making_clean == "Collaboration community-government"~"CGC",
#                                            decision_making_clean == "Government led"~"GL",
#                                            decision_making_clean == "Collaboration community-NGO"~"CNC",
#                                            decision_making_clean == "Others (Academia institution led)"~"O")) %>%
#   mutate(criteria_open_clean = case_when(criteria_open_clean == "Social"~"S",
#                                          criteria_open_clean == "Fisheries management/Fishing income"~"FM/FI",
#                                          criteria_open_clean == "Governance"~"G",
#                                          criteria_open_clean == "Ecosystem/Conservation"~"E/C")) %>%
#   mutate(species_category_clean = case_when(species_category_clean == "Reef fish"~"RF",
#                                             species_category_clean == "Pelagic fish"~"PF",
#                                             species_category_clean == "Invertebrates"~"IN",
#                                             species_category_clean == "Marine mammals/sea turtles/seabirds"~"MM",
#                                             species_category_clean == "Demersal fish"~"DF")) %>%
#   mutate(enforcement_clean = case_when(enforcement_clean == "Community/village led"~"CL",
#                                        enforcement_clean == "Collaboration community-government"~"CGC",
#                                        enforcement_clean == "Government led"~"GL",
#                                        enforcement_clean == "No enforcement"~"NE",
#                                        enforcement_clean == "Collaboration community-NGO"~"CNC",
#                                        enforcement_clean == "NGO led"~"NL")) %>%
#   mutate(size_stan_ha = as.numeric(gsub(",", "", size_stan_ha)),
#          length_closed_stan_years = as.numeric(gsub(",","", length_closed_stan_years)),
#          length_open_stan_years = as.numeric(gsub(",","", length_open_stan_years))) %>%
#   mutate(size_stan_log = log(size_stan_ha+1),
#          length_closed_stan_years_log = log(length_closed_stan_years+1),
#          length_open_stan_years_log = log(length_open_stan_years+1)) %>%
#   select(-size_stan_ha, -length_closed_stan_years, -length_open_stan_years) %>%
#   na.omit()

#########################################################################################
# Test data for the assumption of colliniarity

# # quantitative variables
# quant_variables <-data_famd %>%
#   select(size_stan_ha, length_closed_stan_years, length_open_stan_years,
#          size_stan_log, length_closed_stan_years_log, length_open_stan_years_log)
# 
# # kmo and Barttlett test
# kmo_result <- KMO(quant_variables)
# print(kmo_result)
# 
# bartlett_result <- bartlett.test(quant_variables)
# print(bartlett_result)
# 
# # multicollinearity
# cor_matrix <- cor(quant_variables)
# cor_matrix
# 
# # qualitative variables
# quali_variables <- data_famd %>%
#   select(governance_type_design_clean,
#          decision_making_clean,
#          criteria_open_clean,
#          species_category_clean,
#          enforcement_clean)
# 
# 
# contingency_table_test <- table(quali_variables$governance_type_design_clean, quali_variables$decision_making_clean)
# chisq_test_exp <- chisq.test(contingency_table_test, simulate.p.value = TRUE)
# chisq_test_exp
# 
# 
# 
# chi_squared_results <- list()
# 
# categorical_pairs <- combn(names(quali_variables), 2, simplify = FALSE)
# 
# 
# 
# for(pair in categorical_pairs) {
#   var1 <- pair[1]
#   var2 <- pair[2]
#   
#   # perform chi_squared test
#   chisq_test <- chisq.test(table(quali_variables[[var1]], quali_variables[[var2]]))
#   
#   chi_squared_results[[paste(var1, var2, sep = "_vs_")]] <- chisq_test$p.value
# }
# 
# chi_squared_results
###############################################################################

# Perform factor analysis of mixed data

design_famd <- FAMD(data_famd, sup.var = 1:2, graph = FALSE, ncp = 4)

design_eig <- get_eigenvalue(design_famd)
head(design_eig)

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
  select(-Dim.3, -Dim.4) %>%
  rename(coord_dim1 = Dim.1,
         coord_dim2 = Dim.2)

ind_cos2 <- as.data.frame(ind[["cos2"]]) %>%
  select(-Dim.3, -Dim.4) %>%
  rename(cos_dim1 = Dim.1,
         cos_dim2 = Dim.2)

ind_contrib <- as.data.frame(ind[["contrib"]]) %>%
  select(-Dim.3, -Dim.4) %>%
  rename(contrib_dim1 = Dim.1,
         contrib_dim2 = Dim.2)

ind_total <- cbind(ind_coord, ind_cos2, ind_contrib)

ind_total_clusters <- ind_total %>%
  mutate(cluster = case_when(
    rownames(ind_total) %in% c(13,10,3,4,2,5,6,15,14,16)~"A",
    rownames(ind_total) %in% c(31,32,19,30,29,33)~"B",
    rownames(ind_total) %in% c(1,17,28,27,35,38,36,37,7,8,9,12,40,18,20,25)~"C",
    rownames(ind_total) %in% c(44,54,24,45,48,46,47)~"D",
    rownames(ind_total) %in% c(49,50)~"E",
    rownames(ind_total) %in% c(53,51,52)~"F"
  ))

# make plot to show the individual group in barplot
point_color <- c("A"="#00468b", "B"="#ed0000", "C"="#42b540", "D"="#0099b4", "E"="#925e9f", "F"="#fdaf91")

ind_barplot <- ggplot(data = ind_total_clusters, aes(x = coord_dim1, y = coord_dim2, color = cluster)) + 
  geom_point(size = 0.8) +
  scale_color_manual(name = "Cluster", values = point_color) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  labs(x = "Dim 1", y = "Dim 2") +
  theme_bw() + theme(legend.position = "none")

ind_barplot

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


# add the cluster back to the dataframe
data_clusters <- data_orig %>%
  mutate(Cluster = case_when(
    rownames(data_orig) %in% c(13,6,15,14,16,3,4,2,5,7,10)~"A",
    rownames(data_orig) %in% c(1,35,37,36,38,17,28,18,27,20,25,9,12,8,40)~"B",
    rownames(data_orig) %in% c(31,32,19,30,29,33)~"C",
    rownames(data_orig) %in% c(44,54,24,45,48,46,47)~"D",
    rownames(data_orig) %in% c(49,50)~"E",
    rownames(data_orig) %in% c(51,52,53)~"F"
  ))





##############################################################

## save the cluster plot (preliminary)
ggsave(g1, filename = file.path(plotdir, "Figx_dendrogram_cluster.png"), width = 5, height = 3, units = "in", dpi = 600)

ggsave(g2, filename = file.path(plotdir, "Figx_scatter_cluster.png"), width = 4, height = 3, units = "in", dpi = 600)

ggsave(ind_barplot, filename = file.path(plotdir, "Figx_individual_cluster.png"), width = 5, height = 3, units = "in", dpi = 600 )

# save the famd data
write.csv(data_clusters, "data/empirical_cluster.csv", row.names = FALSE)









