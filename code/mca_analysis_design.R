
#### clean working environment ####
rm(list = ls())

#### read in packages ####
library(tidyverse)
library(cluster)  # For clustering algorithms, including Gower distance
library(factoextra)  # For perform mca visualization
library(FactoMineR) # For perform mca analysis

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
data_mca <- empirical_clean %>%
  select(source, #Supplementary variable (not involved in mca analysis)
         country_clean, #Supplementary variable
         continent_clean, #Supplementary variable
         short_type_of_closure_clean, # Supplementary variable
         governance_type_design_clean, # active variable below
         decision_making_clean, 
         size_bin, 
         length_closed_bin, 
         length_open_bin, 
         enforcement_clean, 
         information_collected_clean) %>%
  mutate(length_open_bin = case_when(length_open_bin == "Daily opening; Quarterly opening"~"Daily opening",
                                     .default = length_open_bin))
  # convert to factors
  # mutate(governance_type_design_clean = as.factor(governance_type_design_clean),
  #        decision_making_clean = as.factor(decision_making_clean),
  #        size_bin = as.factor(size_bin),
  #        length_open_bin = as.factor(length_open_bin),
  #        length_closed_bin = as.factor(length_closed_bin),
  #        enforcement_clean = as.factor(enforcement_clean),
  #        information_collected_clean = as.factor(information_collected_clean))


# Perform the MCA analysis
design_mca <- MCA(data_mca, ncp = 5, quali.sup = 1:4, graph = FALSE)

# check the title of design_mca
print(design_mca)

# check the eigenvalues/variances
eig_val <- get_eigenvalue(design_mca)
head(eig_val)

# check the eigenvalues in screeplot
# This plot demonstrates the percentage of inertia explained by each MCA dimension
g_screeplot <- fviz_screeplot(design_mca, addlabels = TRUE, ylim = c(0, 20))
g_screeplot

# Biplot of individuals and variable categories (global patterns weithin the data)
g_biplot <- fviz_mca_biplot(design_mca, repel = TRUE, ggtheme=theme_minimal())
g_biplot

# Variable biplot
# closer to the axis represents the variable mostly correlated with the dimension
# no other variables seem important to dimension 1 except size bin?
# nothing contributes to dimension 2???
g_var_biplot <- fviz_mca_var(design_mca, choice = "mca.cor", repel = TRUE, ggtheme = theme_minimal())
g_var_biplot

# Variable category biplot
g_var_cat_biplot <- fviz_mca_var(design_mca, repel = TRUE, ggtheme = theme_minimal())
g_var_cat_biplot

#Quality of representation for variable categories
g_var_cat_qua_biplot <- fviz_mca_var(design_mca, col.var = "cos2", gradient.cols = c("#00afbb", "#e7b800", "#fc4e07"),
                                     repel = FALSE,
                                     ggtheme = theme_minimal())
g_var_cat_qua_biplot

# Check quality of representation in histogram
g_var_cat_qua_hist <- fviz_cos2(design_mca, choice = "var", axes = 1:2)
g_var_cat_qua_hist

# Contribution of variable categories to the dimension
# histogram
g_var_cat_cont_hist_d1 <- fviz_contrib(design_mca, choice = "var", axes=1, top=15)
g_var_cat_cont_hist_d1

g_var_cat_cont_hist_d2 <- fviz_contrib(design_mca, choice = "var", axes=2, top=15)
g_var_cat_cont_hist_d2

#biplot
g_var_cat_cont_biplot <- fviz_mca_var(design_mca, col.var = "contrib", gradient.cols = c("#00afbb", "#e7b800", "#fc4e07"),
                                      repel = FALSE,
                                      ggtheme = theme_minimal())
g_var_cat_cont_biplot

# individual quality
g_ind_qua_biplot <- fviz_mca_ind(design_mca, col.ind = "cos2", gradient.cols = c("#00afbb", "#e7b800", "#fc4e07"),
                                 repel = TRUE,
                                 ggtheme = theme_minimal())
g_ind_qua_biplot

# individual contribution
g_ind_cont_biplot <- fviz_mca_ind(design_mca, col.ind = "contrib", gradient.cols = c("#00afbb", "#e7b800", "#fc4e07"),
                                  repel = TRUE,
                                  ggtheme = theme_minimal())
g_ind_cont_biplot





