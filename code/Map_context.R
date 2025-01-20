
# clean working environment
rm(list = ls())

# load in packages
library(tidyverse)
library(scatterpie)

#set up the plot directory
plotdir <- "figure"

# read in data
empirical <- read.csv("data/Cleaned sheets - Full-text screening - Emperical papers_Context_Design_Final.csv", na.strings = "/") %>%
  janitor::clean_names()

modeling <- read.csv("data/Cleaned sheets - Full-text screening - Modeling papers.csv", na.strings = "/") %>%
  janitor::clean_names()

country_location <- read.csv("data/country_location.csv")

# clean data

# emperical data
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
         information_collected_clean)

# modeling data
modeling_clean <- modeling %>%
  select(ref_id, 
         country_clean,
         location_clean,
         continent_clean, 
         case_number_clean, 
         fishery_data_type_clean, 
         closure_data_type_clean,
         model_target_clean,
         model_target_type_clean,
         objectives_scenarios_clean,
         objectives_summarized_clean,
         evaluation_metrics_clean)

# further clean the data to make map

# emperical 
empirical_count <- empirical_clean %>%
  select(source,country_clean, location_clean, location_clean, case_studied_clean) %>%
  # clean country name
  mutate(country_clean = case_when(country_clean == "Madagascar "~"Madagascar",
                                   country_clean == "Papua New Guinea "~"Papua New Guinea",
                                   country_clean == "Norway, and countries in EU (Danish, Swedish, German, Dutch, Belgium)"~"Norway",
                                   country_clean == "French Polynesia "~"French Polynesia",
                                   country_clean == "Soloman Islands"~"Solomon Islands",
                                   .default = country_clean)) %>%
  # differentiate Chile(Easter Island), U.S. (Hawaii) and U.S.(Alaska)
  mutate(country_clean = case_when(location_clean == "Hawaii"~"U.S.(Hawaii)",
                                   location_clean == "Hawaii, Waikiki"~"U.S.(Hawaii)",
                                   location_clean == "Alaska"~"U.S.(Alaska)",
                                   location_clean == "Easter Island, Rapa Nui"~"Chile(Easter Island)",
                                   .default = country_clean)) %>%
  group_by(country_clean) %>%
  summarize(case_studied_total = sum(case_studied_clean)) %>%
  # Madagascar should be 36 - the other 3 cases from different papers but focus on the same region
  mutate(case_studied_total = ifelse(country_clean == "Madagascar", 36, case_studied_total)) %>%
  # add paper type
  mutate(paper_type = "Empirical")

# modeling
modeling_count <- modeling_clean %>%
  filter(!is.na(country_clean)) %>%
  select(ref_id, country_clean, location_clean, case_number_clean) %>%
  mutate(country_clean = case_when(country_clean == "U.S. (Guam)"~"U.S.(Guam)",
                                   .default = country_clean)) %>%
  group_by(country_clean) %>%
  summarize(case_studied_total = sum(case_number_clean)) %>%
  # combine with the country geographic location
  # add paper type
  mutate(paper_type = "Modeling")

# calculate total modeling & empirical cases
total_cases <- bind_rows(empirical_count, modeling_count) %>%
  group_by(country_clean) %>%
  summarize(total_cases = sum(case_studied_total)) %>%
  left_join(empirical_count, by = "country_clean") %>%
  rename(case_studied_empirical = case_studied_total) %>%
  select(-paper_type) %>%
  left_join(modeling_count, by = "country_clean") %>%
  rename(case_studied_modeling = case_studied_total) %>%
  select(-paper_type) %>%
  # change the na to 0 for empirical/modeling paper
  mutate(case_studied_empirical=ifelse(is.na(case_studied_empirical),0,case_studied_empirical)) %>%
  mutate(case_studied_modeling = ifelse(is.na(case_studied_modeling),0,case_studied_modeling))

# calculate percentage of empirical and modeling cases
total_cases_percentage <- total_cases %>%
  mutate(percentage_empirical = (case_studied_empirical/total_cases)*100) %>%
  mutate(percentage_empirical = round(percentage_empirical,0)) %>%
  mutate(percentage_modeling = (case_studied_modeling/total_cases)*100) %>%
  mutate(percentage_modeling = round(percentage_modeling,0)) %>%
  #join with the country location in lat/long
  left_join(country_location, by = "country_clean") %>%
  #set up the radius
  mutate(radius = case_when(total_cases>=1 & total_cases <10~3,
                            total_cases>=10 & total_cases<20~6,
                            total_cases>=20 & total_cases<30~9,
                            total_cases>=30 & total_cases<40~12))


##############################################################

# read in the world map
world <- map_data("world") %>%
  filter(region != "Antarctica")

world_final <- world %>%
  mutate(has_temporary = case_when(region %in% c("Papua New Guinea",
                                                 "Indonesia",
                                                 "Vanuatu",
                                                 "Fiji",
                                                 "Solomon Islands",
                                                 "French Polynesia",
                                                 "USA",
                                                 "New Zealand",
                                                 "Iceland",
                                                 "Norway",
                                                 "UK",
                                                 "Mexico",
                                                 "Madagascar",
                                                 "South Africa",
                                                 "Australia")~"Yes",
                                   .default = "No"))

# Make figure below

# Set theme
base_theme <- theme(axis.text = element_blank(),
                    axis.text.y = element_blank(),
                    axis.title= element_blank(),
                    axis.ticks = element_blank(),
                    legend.text=element_text(size=7),
                    legend.title=element_text(size=8),
                    strip.text = element_text(size=8),
                    plot.tag =element_text(size=9),
                    plot.title=element_blank(),
                    # Gridlines
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.key = element_rect(fill = NA, color=NA),
                    legend.background = element_rect(fill=alpha('blue', 0)))

# set the color scale

country_fill <- c("Yes" = "#7fc97f", "No" = "grey")

paper_fill <- c("percentage_empirical" = "#f0027f", "percentage_modeling" = "#386cb0")

# define the category labeller for scattered pie chart

radius_labeller <- function(radius, label){
  
  label = (radius/3)*10
  
  return (label)
}

worldplot <- ggplot(world_final, aes(long, lat, fill = has_temporary, group = group)) +
  geom_map(map = world_final, aes(map_id = region), color = "grey", linewidth = 0.3, show.legend = F) +
  scale_fill_manual(name = "Has temporary", values = country_fill) +
  ggnewscale::new_scale_fill() +
  geom_scatterpie(data = total_cases_percentage,
                  aes(x = country_long, y = country_lat,r=radius),
                  cols = c("percentage_empirical", "percentage_modeling"),
                  color = "black",
                  linewidth = 0.3)+
  geom_scatterpie_legend(r = total_cases_percentage$radius,
                         x = -170,
                         y = -40,
                         n = 4,
                         labeller = radius_labeller) +
  scale_fill_manual(name = "Case type", values = paper_fill, labels = c("percentage_empirical" = "Empirical", "percentage_modeling" = "Modeling")) +
  coord_fixed(1.3) +
  theme_bw() + base_theme + theme(legend.position = "bottom")

worldplot

# save the world map
ggsave(worldplot, filename = file.path(plotdir, "Figx_closure_location.png"), width = 7.5, height = 5, units = "in", dpi = 600)



