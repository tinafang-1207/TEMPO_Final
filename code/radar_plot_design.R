
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
    row_number() %in% c(27,28,12,14,13,15,3,4,2,5,25,7,11,6,9,10,8,20,21)~"A",
    row_number() %in% c(16,38,37,39,18,36,1,17,24,19,22)~"B",
    row_number() %in% c(29,32,33,30,31,34,35)~"C",
    row_number() %in% c(40,50,23,41,44,42,43)~"D",
    row_number() %in% c(45,46)~"E",
    row_number() %in% c(47,48,49)~"F"
  )) %>%
  filter(!is.na(cluster))


# Cluster 1 Top-down dynamic closures
# Cluster 2 medium term-closure (<5), short opening time (in days)
# Cluster 3 Determinate closure
# Cluster 4 long term-closure (>5), short opening time (in days)
# Cluster 5 co-managed dynamic closures
# Cluster 6 annual closure (<1 or around 1 year), relative long opening (days-weeks-month)

##############################################################################

# Radar plot of the context variable
empirical_radar <- empirical_group %>%
  select(types_of_gear_used_clean, 
         actors_type_governance_clean, 
         traditional_history_clean, 
         start_date_clean, 
         turf_clean,
         motivations_clean,
         cluster)

##############################################################################

# Type of gear used
gear <- empirical_radar %>%
  select(types_of_gear_used_clean, cluster) %>%
  rename(gear = types_of_gear_used_clean) %>%
  mutate(gear = ifelse(gear == "Unknown", NA, gear)) %>%
  mutate(gear = ifelse(gear == "Spears; Hook and line; Nets", "Spears; Hook and line fishing; Nets", gear)) %>%
  filter(!is.na(gear)) %>%
  separate(gear, into = c("gear_one", "gear_two", "gear_three", "gear_four"), sep = ";") %>%
  #clean the separated groups
  mutate(gear_two = case_when(gear_two == " Hook and line fishing"~"Hook and line fishing",
                              gear_two == " Nets"~"Nets", 
                              gear_two ==" Gleaning"~"Gleaning",
                              gear_two == " Seiners"~"Seiners")) %>%
  mutate(gear_three = case_when(gear_three == " Nets"~"Nets",
                                gear_three == " Gleaning"~"Gleaning",
                                gear_three == " Diving"~"Diving")) %>%
  mutate(gear_four = case_when(gear_four == " Gleaning"~"Gleaning"))


gear_summarize <- gear %>%
  pivot_longer(cols = c(gear_one, gear_two, gear_three, gear_four), names_to = "gear_type", values_to = "gear") %>%
  group_by(cluster, gear) %>%
  summarize(gear_count = n(), .groups = "drop") %>%
  pivot_wider(names_from = gear, values_from = gear_count,values_fill = list(gear_count = 0)) %>%
  select(-"NA") %>%
  mutate(case_total = rowSums(across("Gleaning":"Scallop dredges")))
  

gear_percentage <- gear_summarize %>%
  mutate(gleaning_percentage = round((Gleaning/case_total)*100),
         hook_percentage = round((`Hook and line fishing`/case_total)*100),
         nets_percentage = round((Nets/case_total)*100),
         spears_percentage = round((Spears/case_total)*100),
         diving_percentage = round((Diving/case_total)*100),
         traps_percentage = round((Traps/case_total)*100),
         longline_percentage = round((Longline/case_total)*100),
         seiners_percentage = round((Seiners/case_total)*100),
         trawlers_percentage = round((Trawlers/case_total)*100),
         scallop_percentage = round((`Scallop dredges`/case_total)*100)) %>%
  select(-Gleaning, -`Hook and line fishing`, -Nets, -Spears, -Diving, -Traps, -Longline, -Seiners, -Trawlers, -`Scallop dredges`, -case_total) %>%
  rename(Gleaning = gleaning_percentage,
         `Hook and line` = hook_percentage,
         Nets = nets_percentage,
         Spears = spears_percentage,
         Diving = diving_percentage,
         Traps = traps_percentage,
         Longline = longline_percentage,
         Seiners = seiners_percentage,
         Trawlers = trawlers_percentage,
         `Scallop dredges` = scallop_percentage)


gear <- ggradar(
  gear_percentage, 
  values.radar = c("0", "50%", "100%"),
  grid.min = 0, grid.mid = 50, grid.max = 100,
  # Polygons
  group.line.width = 0.4, 
  group.point.size = 0.8,
  group.colours = c("#00468b", "#ed0000", "#42b540", "#0099b4", "#925e9f", "#fdaf91"),
  # Background and grid lines
  background.circle.colour = "gray",
  gridline.mid.colour = "skyblue",
  # Label size
  axis.label.size = 2,
  grid.label.size = 3,
  # legend
  legend.position = "none"
) +
  labs(title = "Gear") +
  theme(plot.title.position = "panel",
        plot.title = element_text(
          size = 10,
          face = "bold"
        ))

gear

#######################################################################

#Actors
actors <- empirical_radar %>%
  select(actors_type_governance_clean, cluster) %>%
  rename(actors = actors_type_governance_clean) %>%
  mutate(actors = case_when(actors == "Fishing industry; Others(Academia)"~"Fishing industry; Others",
                            actors == "Fishing industry; Government; Others(Academia)"~"Fishing industry; Government; Others",
                            .default = actors)) %>%
  separate(actors, into = c("actor_one", "actor_two", "actor_three", "actor_four"), sep = ";") %>%
  mutate(actor_two = case_when(actor_two == " Locals"~"Locals",
                              actor_two == " Government"~"Government", 
                              actor_two ==" NGO"~"NGO",
                              actor_two == " Others"~"Others")) %>%
  mutate(actor_three = case_when(actor_three == " Government"~"Government",
                                actor_three == " NGO"~"NGO",
                                actor_three == " Others"~"Others")) %>%
  mutate(actor_four = case_when(actor_four == " Others"~"Others"))
  
actor_summarize <- actors %>%
  pivot_longer(cols = c(actor_one, actor_two, actor_three, actor_four), names_to = "actor_type", values_to = "actor") %>%
  group_by(cluster, actor) %>%
  summarise(actor_count = n(), .groups = "drop") %>%
  pivot_wider(names_from = actor, values_from = actor_count, values_fill = list(actor_count = 0)) %>%
  select(-"NA") %>%
  mutate(case_total = rowSums(across("Community leaders":"Fishing industry")))


actor_percentage <- actor_summarize %>%
  mutate(community_percentage = round((`Community leaders`/case_total)*100),
         government_percentage = round((Government/case_total)*100),
         locals_percentage = round((Locals/case_total)*100),
         ngo_percentage = round((NGO/case_total)*100),
         others_percentage = round((Others/case_total)*100),
         industry_percentage = round((`Fishing industry`/case_total)*100)) %>%
  select(-`Community leaders`,
         -`Government`,
         -`Locals`,
         -`NGO`,
         -`Others`,
         -`Fishing industry`,
         -`case_total`) %>%
  rename(`Community leaders` = community_percentage,
         `Government` = government_percentage,
         Locals = locals_percentage,
         NGO = ngo_percentage,
         Others = others_percentage,
         `Industry` = industry_percentage)

actor <- ggradar(
  actor_percentage, 
  values.radar = c("0", "50%", "100%"),
  grid.min = 0, grid.mid = 50, grid.max = 100,
  # Polygons
  group.line.width = 0.4, 
  group.point.size = 0.8,
  group.colours = c("#00468b", "#ed0000", "#42b540", "#0099b4", "#925e9f", "#fdaf91"),
  # Background and grid lines
  background.circle.colour = "gray",
  gridline.mid.colour = "skyblue",
  # Label size
  axis.label.size = 2,
  grid.label.size = 3,
  # legend
  legend.position = "none"
) +
  labs(title = "Actor") +
  theme(plot.title.position = "panel",
        plot.title = element_text(
          size = 10,
          face = "bold"
        ))

actor
  
  
########################################################################
# Motivation

motivation <- empirical_radar %>%
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
  mutate(case_total = rowSums(across("Ecosystem/Conservation":"Governance")))

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
         -`case_total`) %>%
  rename(`Ecosystem/Conservation` = eco_percentage,
         `Management` = fish_percentage,
          Governance = gov_percentage,
          Social = social_percentage,
          Others = other_percentage)


motivation <- ggradar(
  motivation_percent, 
  values.radar = c("0", "50%", "100%"),
  grid.min = 0, grid.mid = 50, grid.max = 100,
  # Polygons
  group.line.width = 0.4, 
  group.point.size = 0.8,
  group.colours = c("#00468b", "#ed0000", "#42b540", "#0099b4", "#925e9f", "#fdaf91"),
  # Background and grid lines
  background.circle.colour = "gray",
  gridline.mid.colour = "skyblue",
  # Label size
  axis.label.size = 3,
  grid.label.size = 5,
  # legend
  legend.title = "Cluster(Radar panel)"
) +
  labs(title = "Motivation") +
  theme(plot.title.position = "panel",
        plot.title = element_text(
          size = 10,
          face = "bold"),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 6),
        legend.key.size = unit(0.3, "cm")) +
  guides(fill = guide_legend(nrow = 6))


motivation

radar_legend <- ggpubr::get_legend(motivation)

ggpubr::as_ggplot(radar_legend)


moti_no_legend <- ggradar(
  motivation_percent, 
  values.radar = c("0", "50%", "100%"),
  grid.min = 0, grid.mid = 50, grid.max = 100,
  # Polygons
  group.line.width = 0.4, 
  group.point.size = 0.8,
  group.colours = c("#00468b", "#ed0000", "#42b540", "#0099b4", "#925e9f", "#fdaf91"),
  # Background and grid lines
  background.circle.colour = "gray",
  gridline.mid.colour = "skyblue",
  # Label size
  axis.label.size = 2,
  grid.label.size = 3,
  # legend
  legend.position = "none"
) +
  labs(title = "Motivation") +
  theme(plot.title.position = "panel",
        plot.title = element_text(
          size = 10,
          face = "bold"
        ))

moti_no_legend

###################################################################
# Traditional history

history_summarize <- empirical_radar %>%
  select(traditional_history_clean, cluster) %>%
  rename(history = traditional_history_clean) %>%
  mutate(history = ifelse(history == "Unknown", NA, history)) %>%
  mutate(history = ifelse(history == "No ", "No", history)) %>%
  na.omit() %>%
  group_by(cluster, history) %>%
  summarize(history_count = n(), .groups = "drop") %>%
  pivot_wider(names_from = history, values_from = history_count, values_fill = list(history_count = 0)) %>%
  mutate(case_total = rowSums(across("No":"Yes")))

history_percentage <- history_summarize %>%
  mutate(no_percentage = round((No/case_total)*100),
         yes_percentage = round((Yes/case_total)*100)) %>%
  select(-No, -Yes, -case_total) %>%
  rename(Yes = yes_percentage,
         No = no_percentage) %>%
  pivot_longer(cols = c(No, Yes), names_to = "history_type", values_to = "percentage") %>%
  mutate(variable_type = "Traditional history") %>%
  mutate(history_type = factor(history_type, levels = c("Yes", "No")))


#set theme for bar plot
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

history_fill <- c("No" = "#ed0000", "Yes" = "#42b540")

history <-  ggplot(data = history_percentage, aes(x= cluster, y = percentage, fill = history_type)) +
  facet_grid(variable_type~., space = "free_y", scales = "free_y") +
  geom_bar(position = position_stack(), stat = "identity", color = "grey30", lwd = 0.2) +
  scale_fill_manual(name = "Categories", values = history_fill) +
  scale_y_continuous(lim = c(0, 112), breaks = seq(0, 100, 50)) +
  labs(y = "Percentage") +
  theme_bw() + barplot_theme + guides(fill = guide_legend(nrow = 1)) +
  theme(axis.title.x = element_blank(),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.key.size = unit(0.5, "cm"),
        legend.position = c(0.25, 0.93))

history

####################################################################
# TURF

turf_summarize <- empirical_radar %>%
  select(turf_clean, cluster) %>%
  rename(turf = turf_clean) %>%
  mutate(turf = ifelse(turf == "Unknown", NA, turf)) %>%
  na.omit() %>%
  group_by(cluster, turf) %>%
  summarize(turf_count = n(), .groups = "drop") %>%
  pivot_wider(names_from = turf, values_from = turf_count, values_fill = list(turf_count = 0)) %>%
  mutate(case_total = rowSums(across("No":"Yes")))

turf_percentage <- turf_summarize %>%
  mutate(no_percentage = round((No/case_total)*100),
         yes_percentage = round((Yes/case_total)*100)) %>%
  select(-No, -Yes, -case_total) %>%
  rename(Yes = yes_percentage,
         No = no_percentage) %>%
  pivot_longer(cols = c(No, Yes), names_to = "turf_type", values_to = "percentage") %>%
  mutate(variable_type = "TURF") %>%
  mutate(turf_type = factor(turf_type, levels = c("Yes", "No")))

turf_fill <- c("No" = "#ed0000", "Yes" = "#42b540")

turf <-  ggplot(data = turf_percentage, aes(x= cluster, y = percentage, fill = turf_type)) +
  facet_grid(variable_type~., space = "free_y", scales = "free_y") +
  geom_bar(position = position_stack(), stat = "identity", color = "grey30", lwd = 0.2) +
  scale_fill_manual(name = "Categories", values = history_fill) +
  scale_y_continuous(lim = c(0, 112), breaks = seq(0, 100, 50)) +
  theme_bw() + barplot_theme + guides(fill = guide_legend(nrow = 1)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.key.size = unit(0.5, "cm"),
        legend.position = c(0.25, 0.93))

turf

#######################################################################

g_context <- gridExtra::grid.arrange(gear,actor,moti_no_legend,history,turf, radar_legend, ncol = 3, widths = c(1/3, 1/3, 1/3), heights = c(0.5, 0.5))

ggsave(g_context, filename = file.path(plotdir, "Figx_context_radar.png"), width = 9, height = 6.5, units = "in", dpi = 600)



