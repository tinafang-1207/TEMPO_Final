
### clean working environment ###
rm(list = ls())

### load in packages ###
library(tidyverse)
library(gtable)

### set the plot directory ###
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
                                           decision_making_clean == "Others (Academia institution led)"~"AL")) %>%
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
         length_open_stan_years_log = log(length_open_stan_years+1))%>%
  select(-size_stan_ha, -length_closed_stan_years, -length_open_stan_years) %>%
  mutate(cluster = case_when(
    row_number() %in% c(27,28,12,14,13,15,3,4,2,5,25,7,11,6,9,10,8,20,21)~"A",
    row_number() %in% c(16,38,37,39,18,36,1,17,24,19,22)~"B",
    row_number() %in% c(29,32,33,30,31,34,35)~"C",
    row_number() %in% c(40,50,23,41,44,42,43)~"D",
    row_number() %in% c(45,46)~"E",
    row_number() %in% c(47,48,49)~"F"
  )) %>%
  na.omit()

########################################################################
# Make figure to see the difference

# Quantitative variable (size, length of open, length of close)
empirical_quanti <- empirical_group %>%
  select(size_stan_log, length_closed_stan_years_log, length_open_stan_years_log, cluster) %>%
  gather(key = "Variable", value = "Value", size_stan_log, length_closed_stan_years_log, length_open_stan_years_log) %>%
  mutate(Variable = case_when(Variable == "size_stan_log"~"Size (ha)",
                              Variable == "length_closed_stan_years_log"~"Length of closure (years)",
                              Variable == "length_open_stan_years_log"~"Length of open (years)"))

# Qualitative variable 

# governance type
empirical_quali_govern <- empirical_group %>%
  select(governance_type_design_clean, cluster) %>%
  group_by(cluster, governance_type_design_clean) %>%
  summarize(type_count = n()) %>%
  mutate(total_case = case_when(cluster == "A"~19,
                                cluster == "B"~11,
                                cluster == "C"~7,
                                cluster == "D"~7,
                                cluster == "E"~2,
                                cluster == "F"~3)) %>%
  mutate(cluster = factor(cluster, levels = c("F", "E", "D", "C", "B", "A"))) %>%
  mutate(percentage = (type_count/total_case)*100) %>%
  mutate(percentage = round(percentage, 0)) %>%
  mutate(label = paste0(percentage, "%")) %>%
  mutate(variable_type = "Governance type") %>%
  rename(variable_category = governance_type_design_clean) %>%
  select(cluster, variable_type, variable_category, everything())


# Decision making process
empirical_quali_decision <- empirical_group %>%
  select(decision_making_clean, cluster) %>%
  group_by(cluster, decision_making_clean) %>%
  summarize(type_count = n()) %>%
  mutate(cluster = factor(cluster, levels = c("F", "E", "D", "C", "B", "A"))) %>%
  mutate(total_case = case_when(cluster == "A"~19,
                                cluster == "B"~11,
                                cluster == "C"~7,
                                cluster == "D"~7,
                                cluster == "E"~2,
                                cluster == "F"~3)) %>%
  mutate(percentage = (type_count/total_case)*100) %>%
  mutate(percentage = round(percentage, 0)) %>%
  mutate(label = paste0(percentage, "%")) %>%
  mutate(variable_type = "Decision making type") %>%
  rename(variable_category = decision_making_clean) %>%
  select(cluster, variable_type, variable_category, everything())

# criteria of opening
empirical_quali_open <- empirical_group %>%
  select(criteria_open_clean, cluster) %>%
  group_by(cluster, criteria_open_clean) %>%
  summarize(type_count = n()) %>%
  mutate(cluster = factor(cluster, levels = c("F", "E", "D", "C", "B", "A"))) %>%
  mutate(total_case = case_when(cluster == "A"~19,
                                cluster == "B"~11,
                                cluster == "C"~7,
                                cluster == "D"~7,
                                cluster == "E"~2,
                                cluster == "F"~3)) %>%
  mutate(percentage = (type_count/total_case)*100) %>%
  mutate(percentage = round(percentage, 0)) %>%
  mutate(label = paste0(percentage, "%")) %>%
  mutate(variable_type = "Criteria of opening") %>%
  rename(variable_category = criteria_open_clean) %>%
  select(cluster, variable_type, variable_category, everything())

# target species
empirical_quali_species <- empirical_group %>%
  select(species_category_clean, cluster) %>%
  group_by(cluster, species_category_clean) %>%
  summarize(type_count = n()) %>%
  mutate(cluster = factor(cluster, levels = c("F", "E", "D", "C", "B", "A"))) %>%
  mutate(total_case = case_when(cluster == "A"~19,
                                cluster == "B"~11,
                                cluster == "C"~7,
                                cluster == "D"~7,
                                cluster == "E"~2,
                                cluster == "F"~3)) %>%
  mutate(percentage = (type_count/total_case)*100) %>%
  mutate(percentage = round(percentage, 0)) %>%
  mutate(label = paste0(percentage, "%")) %>%
  mutate(variable_type = "Target species") %>%
  rename(variable_category = species_category_clean) %>%
  select(cluster, variable_type, variable_category, everything())

# enforcement type
empirical_quali_enforcement <- empirical_group %>%
  select(enforcement_clean, cluster) %>%
  group_by(cluster, enforcement_clean) %>%
  summarize(type_count = n()) %>%
  mutate(cluster = factor(cluster, levels = c("F", "E", "D", "C", "B", "A"))) %>%
  mutate(total_case = case_when(cluster == "A"~19,
                                cluster == "B"~11,
                                cluster == "C"~7,
                                cluster == "D"~7,
                                cluster == "E"~2,
                                cluster == "F"~3)) %>%
  mutate(percentage = (type_count/total_case)*100) %>%
  mutate(percentage = round(percentage, 0)) %>%
  mutate(label = paste0(percentage, "%")) %>%
  mutate(variable_type = "Enforcement type") %>%
  rename(variable_category = enforcement_clean) %>%
  select(cluster, variable_type, variable_category, everything())



#######################################################################
# Boxplot of quantitative variables

# import theme
boxplot_theme <- theme(axis.text=element_text(size=6),
                    axis.title=element_text(size=7),
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
g_quanti_final <- ggplot(empirical_quanti, aes(x = cluster, y = Value, fill = cluster)) +
  geom_boxplot(outlier.shape = 21, linewidth = 0.2, outlier.size = 1, outlier.stroke = 0.2) +
  labs(x = "Cluster", y = "Value(in log scale)") +
  scale_fill_manual(name = "Cluster", values = color_scheme) +
  facet_wrap(.~Variable, scales = "free_y") +
  boxplot_theme + theme(legend.position = "none")

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
                       legend.background = element_rect(fill=alpha('blue', 0)))

# governance
govern_type = c("C" = "#0099b4", "B" = "#925e9f", "T" = "#fdaf91")

g_quali_govern <- ggplot(data = empirical_quali_govern, aes(x=percentage, y = cluster, fill = variable_category)) +
  facet_grid(variable_type~., space = "free_y", scales = "free_y") +
  geom_bar(position = position_stack(), stat = "identity", color = "grey30", lwd = 0.2) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 2.1, color = "black", fontface = "bold") +
  scale_fill_manual(name = "Types", values = govern_type) +
  scale_x_continuous(lim = c(0, 120), breaks = seq(0,100,25)) +
  theme_bw() + barplot_theme + theme(axis.title.x = element_blank(),
                                     axis.text.x = element_blank(),
                                     axis.ticks.x = element_blank(),
                                     axis.title.y = element_blank(),
                                     legend.position = c(0.9, 0.8))

g_quali_govern

# decision making process
decision_type = c("CGC" = "#ed0000", "CNC" = "#42b540", "CL" = "#0099b4", "GL" = "#925e9f", "AL" = "#fdaf91")


g_quali_decision <- ggplot(data = empirical_quali_decision, aes(x=percentage, y = cluster, fill = variable_category)) +
  facet_grid(variable_type~., space = "free_y", scales = "free_y") +
  geom_bar(position = position_stack(), stat = "identity", color = "grey30", lwd = 0.2) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 2.1, color = "black", fontface = "bold") +
  scale_fill_manual(name = "Types", values = decision_type) +
  scale_x_continuous(lim = c(0, 120), breaks = seq(0,100,25)) +
  theme_bw() + barplot_theme + theme(axis.title.x = element_blank(),
                                     axis.text.x = element_blank(),
                                     axis.ticks.x = element_blank(),
                                     axis.title.y = element_blank(),
                                     legend.position = c(0.9, 0.75))
g_quali_decision

# criteria open
open_type <- c("S" = "#42b540", "G" = "#0099b4", "FM/FI" = "#925e9f", "E/C" = "#fdaf91")

g_quali_open <- ggplot(data = empirical_quali_open, aes(x=percentage, y = cluster, fill = variable_category)) +
  facet_grid(variable_type~., space = "free_y", scales = "free_y") +
  geom_bar(position = position_stack(), stat = "identity", color = "grey30", lwd = 0.2) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 2.1, color = "black", fontface = "bold") +
  scale_fill_manual(name = "Types", values = open_type) +
  scale_x_continuous(lim = c(0, 120), breaks = seq(0,100,25)) +
  theme_bw() + barplot_theme + theme(axis.title.x = element_blank(),
                                     axis.text.x = element_blank(),
                                     axis.ticks.x = element_blank(),
                                     axis.title.y = element_blank(),
                                     legend.position = c(0.9, 0.8))
g_quali_open

# target species

species_type = c("DF" = "#ed0000", "RF" = "#42b540", "IN" = "#0099b4", "PF" = "#925e9f", "MM" = "#fdaf91")

g_quali_species <- ggplot(data = empirical_quali_species, aes(x=percentage, y = cluster, fill = variable_category)) +
  facet_grid(variable_type~., space = "free_y", scales = "free_y") +
  geom_bar(position = position_stack(), stat = "identity", color = "grey30", lwd = 0.2) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 2.1, color = "black", fontface = "bold") +
  scale_fill_manual(name = "Types", values = species_type) +
  scale_x_continuous(lim = c(0, 120), breaks = seq(0,100,25)) +
  theme_bw() + barplot_theme + theme(axis.title.x = element_blank(),
                                     axis.text.x = element_blank(),
                                     axis.ticks.x = element_blank(),
                                     axis.title.y = element_blank(),
                                     legend.position = c(0.9, 0.75))
g_quali_species


# enforcement

enforcement_type = c("CGC" = "#ed0000", "CNC" = "#42b540", "CL" = "#0099b4", "GL" = "#925e9f", "NE" = "#fdaf91")

g_quali_enforcement <- ggplot(data = empirical_quali_enforcement, aes(x=percentage, y = cluster, fill = variable_category)) +
  facet_grid(variable_type~., space = "free_y", scales = "free_y") +
  geom_bar(position = position_stack(), stat = "identity", color = "grey30", lwd = 0.2) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 2.1, color = "black", fontface = "bold") +
  scale_fill_manual(name = "Types", values = enforcement_type) +
  scale_x_continuous(lim = c(0, 120), breaks = seq(0,100,25)) +
  theme_bw() + barplot_theme + theme(axis.title.x = element_blank(),
                                     axis.title.y = element_blank(),
                                     legend.position = c(0.9, 0.75))

g_quali_enforcement

g_quali_govern_f <- ggplotGrob(g_quali_govern)
g_quali_decision_f <- ggplotGrob(g_quali_decision)
g_quali_open_f <- ggplotGrob(g_quali_open)
g_quali_species_f <- ggplotGrob(g_quali_species)
g_quali_enforcement_f <- ggplotGrob(g_quali_enforcement)

g_quali_final <- rbind(g_quali_govern_f,
                       g_quali_decision_f,
                       g_quali_open_f,
                       g_quali_species_f,
                       g_quali_enforcement_f)
g_quali_final

ggsave(g_quali_final, filename = file.path(plotdir, "Figx_qualitative_variable_cluster.png"), width = 7.5, height = 18, units = "in", dpi = 600)

ggsave(g_quanti_final, filename = file.path(plotdir, "Figx_quantitative_variable_cluster.png"), width = 5, height = 4, units = "in", dpi = 600)




