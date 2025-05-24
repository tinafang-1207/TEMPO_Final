
### clean working environment ###
rm(list = ls())

### load in packages ###
library(tidyverse)

### set the plot directory ###
plotdir <- "figure"

#### read in data ####
empirical_cluster <- read.csv("data/empirical_cluster.csv") %>%
  janitor::clean_names()

# clean data
design_famd_cluster <- empirical_cluster %>% select(
  ref_id,
  governance_type_design_clean,
  criteria_open_clean,
  decision_making_clean,
  closure_size_ha,
  time_closed_years,
  time_open_years,
  species_category_clean,
  enforcement_clean,
  cluster
) %>%
  mutate(closure_size_ha = as.numeric(gsub(",", "", closure_size_ha)),
         time_closed_years = as.numeric(gsub(",", "", time_closed_years)),
         time_open_years = as.numeric(gsub(",", "", time_open_years))) %>%
  na.omit()



########################################################################
# Make figure to see the difference

# Quantitative variable (size, length of open, length of close)
empirical_quanti <- design_famd_cluster %>%
  select(closure_size_ha, time_closed_years, time_open_years, cluster) %>%
  gather(key = "Variable", value = "Value", closure_size_ha, time_closed_years, time_open_years) %>%
  mutate(Variable = case_when(Variable == "closure_size_ha"~"Size (ha)",
                              Variable == "time_closed_years"~"Length of closure (years)",
                              Variable == "time_open_years"~"Length of open (years)"))

# Qualitative variable 

# governance type
empirical_quali_govern <- design_famd_cluster %>%
  select(governance_type_design_clean, cluster) %>%
  group_by(cluster, governance_type_design_clean) %>%
  summarize(type_count = n()) %>%
  mutate(variable_type = "Governance type") %>%
  mutate(governance_type_design_clean = case_when(governance_type_design_clean == "Co-management"~"Co-mgmt",
                                                  governance_type_design_clean == "Bottom-up"~"Btm-up",
                                                  governance_type_design_clean == "Top-down"~"Top-dwn"))


# Decision making process
empirical_quali_decision <- design_famd_cluster %>%
  select(decision_making_clean, cluster) %>%
  group_by(cluster, decision_making_clean) %>%
  summarize(type_count = n()) %>%
  mutate(variable_type = "Decision making type") %>%
  # change the name of the variable category
  mutate(decision_making_clean = case_when(decision_making_clean == "Collaboration community-NGO"~"Collab comm-NGO",
                                           decision_making_clean == "Collaboration community-government"~"Collab comm-gov",
                                           decision_making_clean == "Community led"~"Comm led",
                                           decision_making_clean == "Government led"~"Gov led",
                                           decision_making_clean == "Collaboration community-academia"~"Collab comm-aca"))

# criteria of opening
empirical_quali_open <- design_famd_cluster %>%
  select(criteria_open_clean, cluster) %>%
  group_by(cluster, criteria_open_clean) %>%
  summarize(type_count = n()) %>%
  mutate(variable_type = "Criteria of opening") %>%
  # change the name of the variable category
  mutate(criteria_open_clean = case_when(criteria_open_clean == "Ecosystem/Conservation"~"Conserv",
                                         criteria_open_clean == "Social"~"Soc",
                                         criteria_open_clean == "Governance"~"Gov",
                                         criteria_open_clean == "Fisheries management/Fishing income"~"Fish mgmt",
                                         criteria_open_clean == "Fisheries management/Fishing income; Social"~"Soc"))

# target species
empirical_quali_species <- design_famd_cluster %>%
  select(species_category_clean, cluster) %>%
  group_by(cluster, species_category_clean) %>%
  summarize(type_count = n()) %>%
  mutate(variable_type = "Target species") %>%
  # change the name of the variable category
  mutate(species_category_clean = case_when(species_category_clean == "Invertebrates"~"Invert",
                                            .default = species_category_clean))

# enforcement type
empirical_quali_enforcement <- design_famd_cluster %>%
  select(enforcement_clean, cluster) %>%
  group_by(cluster, enforcement_clean) %>%
  summarize(type_count = n()) %>%
  mutate(variable_type = "Enforcement type") %>%
 # change the name of the variable category
  mutate(enforcement_clean = case_when(enforcement_clean == "Collaboration community-NGO"~"Collab comm-NGO",
                                       enforcement_clean == "Collaboration community-government"~"Collab comm-gov",
                                       enforcement_clean == "Community led"~"Comm led",
                                       enforcement_clean == "Government led"~"Gov led",
                                       enforcement_clean == "No enforcement"~"No enf"))



#######################################################################
# Boxplot of quantitative variables

# import theme
boxplot_theme <- theme(axis.text=element_text(size=8),
                    axis.title=element_text(size=9),
                    legend.text=element_text(size=6),
                    legend.title=element_text(size=7),
                    strip.text = element_text(size=7),
                    plot.tag = element_text(size=8),
                    plot.title = element_blank(),
                    # Gridlines
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    panel.border = element_rect(colour = "black", fill = NA),
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.background = element_rect(fill=alpha('blue', 0)))

color_scheme <- c("A" = "#00468b", "B" = "#ed0000", "C" = "#42b540", "D" = "#0099b4", "E" = "#925e9f", "F" = "#fdaf91" )

# make plot
g_quanti_final <- ggplot(empirical_quanti, aes(x = cluster, y = Value)) +
  geom_boxplot(outlier.shape = 21, linewidth = 0.2, outlier.size = 1, outlier.stroke = 0.2) +
  geom_jitter(aes(color = cluster), width = 0.1, alpha = 0.8) +
  labs(x = "Cluster", y = "Value") +
  scale_color_manual(name = "Cluster", values = color_scheme) +
  scale_y_continuous(trans = "log10", labels = scales::label_number()) +
  facet_wrap(.~Variable, scales = "free_y", strip.position = "right") +
  boxplot_theme + theme(legend.position = "none",
                        strip.background = element_rect(color = "black",fill = "gray90"),
                        axis.title.x = element_blank())

g_quanti_final

#######################################################################
# Barplot of qualitative variables
# import theme
barplot_theme <- theme(axis.text=element_text(size=8),
                       axis.title=element_text(size=9),
                       legend.text=element_text(size=8),
                       legend.title=element_text(size=9),
                       strip.text=element_text(size=8),
                       plot.title=element_text(size=9),
                       # Gridlines
                       panel.grid.major = element_blank(), 
                       panel.grid.minor = element_blank(),
                       panel.background = element_blank(),
                       axis.line = element_line(colour = "black"),
                       # Legend
                       legend.key = element_rect(fill = NA, color=NA),
                       legend.background = element_rect(fill=alpha('blue', 0)),
                       strip.background = element_rect(color = "black",fill = "gray90"))

# governance
govern_type = c("Co-mgmt" = "#0099b4", "Btm-up" = "#925e9f", "Top-dwn" = "#fdaf91")

g1 <- ggplot(data = empirical_quali_govern, aes(x=cluster, y = type_count, fill = governance_type_design_clean)) +
  facet_grid(variable_type~., space = "free_y", scales = "free_y") +
  geom_bar(position = position_stack(), stat = "identity", color = "grey30", lwd = 0.2) +
  labs(y = "Case count") +
  scale_fill_manual(name = "Categories", values = govern_type) +
  scale_y_continuous(lim = c(0, 20), breaks = seq(0, 20, 5)) +
  theme_bw() + barplot_theme + theme(axis.title.x = element_blank(),
                                     legend.position = c(0.7, 0.75),
                                     legend.key.size = unit(0.3, "cm"),
                                     legend.text = element_text(size = 8),
                                     legend.title = element_text(size = 8))

g1

# decision making process
decision_type = c("Collab comm-gov" = "#ed0000", "Collab comm-NGO" = "#42b540", "Comm led" = "#0099b4", "Gov led" = "#925e9f", "Collab comm-aca" = "#fdaf91")

g2 <- ggplot(data = empirical_quali_decision, aes(x=cluster, y = type_count, fill = decision_making_clean)) +
  facet_grid(variable_type~., space = "free_y", scales = "free_y") +
  geom_bar(position = position_stack(), stat = "identity", color = "grey30", lwd = 0.2) +
  scale_fill_manual(name = "Categories", values = decision_type) +
  scale_y_continuous(lim = c(0, 20), breaks = seq(0, 20, 5)) +
  theme_bw() + barplot_theme + theme(axis.title.x = element_blank(),
                                     axis.title.y = element_blank(),
                                     axis.text.y = element_blank(),
                                     axis.ticks.y = element_blank(),
                                     legend.position = c(0.65, 0.75),
                                     legend.key.size = unit(0.3, "cm"),
                                     legend.text = element_text(size = 8),
                                     legend.title = element_text(size = 8))
g2

# criteria open
open_type <- c("Soc" = "#42b540", "Gov" = "#0099b4", "Fish mgmt" = "#925e9f", "Conserv" = "#fdaf91")

g3 <- ggplot(data = empirical_quali_open, aes(x=cluster, y = type_count, fill = criteria_open_clean)) +
  facet_grid(variable_type~., space = "free_y", scales = "free_y") +
  geom_bar(position = position_stack(), stat = "identity", color = "grey30", lwd = 0.2) +
  labs(x = "Cluster") +
  scale_fill_manual(name = "Categories", values = open_type) +
  scale_y_continuous(lim = c(0, 20), breaks = seq(0, 20, 5)) +
  theme_bw() + barplot_theme + theme(axis.title.x = element_blank(),
                                     axis.title.y = element_blank(),
                                     axis.text.y = element_blank(),
                                     axis.ticks.y = element_blank(),
                                     legend.position = c(0.7, 0.75),
                                     legend.key.size = unit(0.3, "cm"),
                                     legend.text = element_text(size = 8),
                                     legend.title = element_text(size = 8))
g3

# target species

species_type = c("Demersal fish" = "#ed0000", "Reef fish" = "#42b540", "Invert" = "#0099b4", "Pelagic fish" = "#925e9f", "MM/ST/SB" = "#fdaf91")

g4 <- ggplot(data = empirical_quali_species, aes(x=cluster, y = type_count, fill = species_category_clean)) +
  facet_grid(variable_type~., space = "free_y", scales = "free_y") +
  geom_bar(position = position_stack(), stat = "identity", color = "grey30", lwd = 0.2) +
  labs(x = "Cluster", y = "Case count") +
  scale_fill_manual(name = "Categories", values = species_type) +
  scale_y_continuous(lim = c(0, 20), breaks = seq(0, 20, 5)) +
  theme_bw() + barplot_theme + theme(legend.position = c(0.65, 0.75),
                                     legend.key.size = unit(0.3, "cm"),
                                     legend.text = element_text(size = 8),
                                     legend.title = element_text(size = 8))


g4


# enforcement

enforcement_type = c("Collab comm-gov" = "#ed0000", "Collab comm-NGO" = "#42b540", "Comm led" = "#0099b4", "Gov led" = "#925e9f", "No enf" = "#fdaf91")

g5 <- ggplot(data = empirical_quali_enforcement, aes(x=cluster, y = type_count, fill = enforcement_clean)) +
  facet_grid(variable_type~., space = "free_y", scales = "free_y") +
  geom_bar(position = position_stack(), stat = "identity", color = "grey30", lwd = 0.2) +
  labs(x = "Cluster") +
  scale_fill_manual(name = "Categories", values = enforcement_type) +
  scale_y_continuous(lim = c(0, 20), breaks = seq(0, 20, 5)) +
  theme_bw() + barplot_theme + theme(axis.title.y = element_blank(),
                                     axis.text.y = element_blank(),
                                     axis.ticks.y = element_blank(),
                                     legend.position = c(0.7, 0.75),
                                     legend.key.size = unit(0.3, "cm"),
                                     legend.text = element_text(size = 8),
                                     legend.title = element_text(size = 8))

g5

 
# Merge qualitative variable
 
g_quali <- gridExtra::grid.arrange(g1,g2,g3,g4,g5, ncol = 3, widths = c(1/3, 1/3, 1/3), heights = c(0.5, 0.5))

layout_matrix <- matrix(data = c(1,2), nrow = 2, byrow = TRUE)

g_total <- gridExtra::grid.arrange(g_quanti_final, g_quali, layout_matrix = layout_matrix, heights = c(0.36, 0.64))


# save the final figure
ggsave(g_total, filename = file.path(plotdir, "Figx_understand_cluster.png"), width = 8, height = 7, units = "in", dpi = 600)





