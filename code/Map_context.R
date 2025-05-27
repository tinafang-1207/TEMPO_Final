
# clean working environment
rm(list = ls())

# load in packages
library(tidyverse)
library(scatterpie)

#set up the plot directory
plotdir <- "figure"

# read in data
empirical <- read.csv("data/clean_data/empirical_cluster.csv")

modeling <- read.csv("data/Cleaned sheets - Full-text screening - Modeling papers.csv", na.strings = "/") %>%
  janitor::clean_names()

country_location <- read.csv("data/country_location.csv")

# clean data

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
empirical_count <- empirical %>%
  select(ref_id,country_clean, location_clean, location_clean) %>%
  # clean country name
  mutate(country_clean = case_when(country_clean == "Madagascar "~"Madagascar",
                                   country_clean == "Papua New Guinea "~"Papua New Guinea",
                                   country_clean == "Norway, and countries in EU (Danish, Swedish, German, Dutch, Belgium)"~"Norway",
                                   country_clean == "French Polynesia "~"French Polynesia",
                                   country_clean == "Soloman Islands"~"Solomon Islands",
                                   .default = country_clean)) %>%
  # differentiate Chile(Easter Island), U.S. (Hawaii) and U.S.(Alaska)
  mutate(country_clean = case_when(location_clean == "Hawaii, Waikiki"~"U.S.(Hawaii)",
                                   location_clean == "Alaska"~"U.S.(Alaska)",
                                   .default = country_clean)) %>%
  group_by(country_clean) %>%
  summarize(case_studied_total = n()) %>%
  # add paper type
  mutate(paper_type = "Empirical")

# modeling
modeling_count <- modeling_clean %>%
  filter(!is.na(country_clean)) %>%
  select(ref_id, country_clean, location_clean) %>%
  mutate(country_clean = case_when(country_clean == "U.S. (Guam)"~"U.S.(Guam)",
                                   .default = country_clean)) %>%
  group_by(country_clean) %>%
  summarize(case_studied_total = n()) %>%
  # combine with the country geographic location
  # add paper type
  mutate(paper_type = "Theoretical")

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
  mutate(radius = case_when(total_cases>=1 & total_cases<=2~2,
                            total_cases>2 & total_cases<=6~6,
                            total_cases>=6~10))


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
                                                 "Australia",
                                                 "Tanzania")~"Yes",
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
  
  label = radius
  
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
                         y = -50,
                         n = 3,
                         labeller = radius_labeller) +
  scale_fill_manual(name = "Case types", values = paper_fill, labels = c("percentage_empirical" = "Empirical", "percentage_modeling" = "Theoretical")) +
  geom_text(data = total_cases_percentage, aes(x = country_long, y = country_lat, label = country_clean, hjust = hjust, vjust = vjust), size = 2.5, inherit.aes = FALSE, fontface = "bold") +
  coord_fixed(1.3) +
  labs(tag = "A") +
  theme_bw() + base_theme + theme(legend.position = c(0.2, 0.13),
                                  legend.key.size = unit(0.3, "cm"),
                                  legend.text = element_text(size = 8),
                                  legend.title = element_text(size = 8),
                                  plot.tag = element_text(size = 10, face = "bold"))

worldplot

#################################################################################
# Make bar plot

empirical_total_cases <- empirical %>%
  mutate(short_type_of_closure_clean = ifelse(continent_clean == "Africa", "Determinate closure", short_type_of_closure_clean)) %>%
  mutate(short_type_of_closure_clean = ifelse(location_clean == "Hawaii, Waikiki", "Rotational closure", short_type_of_closure_clean)) %>%
  # clean country name
  mutate(country_clean = case_when(country_clean == "Madagascar "~"Madagascar",
                                   country_clean == "Papua New Guinea "~"Papua New Guinea",
                                   country_clean == "Norway, and countries in EU (Danish, Swedish, German, Dutch, Belgium)"~"Norway",
                                   country_clean == "French Polynesia "~"French Polynesia",
                                   country_clean == "Soloman Islands"~"Solomon Islands",
                                   .default = country_clean)) %>%
  mutate(country_clean = case_when(location_clean == "Hawaii, Waikiki"~"United States",
                                   location_clean == "Alaska"~"United States",
                                   .default = country_clean)) %>%
  group_by(country_clean) %>%
  summarize(total_cases_all = n())
  
empirical_bar <- empirical %>%
  mutate(short_type_of_closure_clean = ifelse(continent_clean == "Africa", "Determinate closure", short_type_of_closure_clean)) %>%
  mutate(short_type_of_closure_clean = ifelse(location_clean == "Hawaii, Waikiki", "Rotational closure", short_type_of_closure_clean)) %>%
  # clean country name
  mutate(country_clean = case_when(country_clean == "Madagascar "~"Madagascar",
                                   country_clean == "Papua New Guinea "~"Papua New Guinea",
                                   country_clean == "Norway, and countries in EU (Danish, Swedish, German, Dutch, Belgium)"~"Norway",
                                   country_clean == "French Polynesia "~"French Polynesia",
                                   country_clean == "Soloman Islands"~"Solomon Islands",
                                   .default = country_clean)) %>%
  mutate(country_clean = case_when(location_clean == "Hawaii, Waikiki"~"United States",
                                   location_clean == "Alaska"~"United States",
                                   .default = country_clean)) %>%
  group_by(country_clean, short_type_of_closure_clean) %>%
  summarize(total_cases = n()) %>%
  left_join(empirical_total_cases, by = "country_clean") %>%
  mutate(source = "Empirical") 

modeling_total_cases <- modeling_clean %>%
  mutate(country_clean = ifelse(country_clean == "U.S. (Guam)", "United States", country_clean)) %>%
  mutate(country_clean = ifelse(is.na(country_clean), "Simulated study", country_clean)) %>%
  group_by(country_clean) %>%
  summarize(total_cases_all = n())

modeling_bar <- modeling_clean %>%
  mutate(country_clean = ifelse(country_clean == "U.S. (Guam)", "United States", country_clean)) %>%
  mutate(country_clean = ifelse(is.na(country_clean), "Simulated study", country_clean)) %>%
  group_by(country_clean, objectives_scenarios_clean) %>%
  summarize(total_cases = n()) %>%
  left_join(modeling_total_cases, by = "country_clean") %>%
  mutate(source = "Theoretical")

#################################################################
# Make bar plot

# Theme
bar_theme <-  theme(axis.text=element_text(size=8),
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

closure_type <- c("Periodic closure" = "#7fc97f", "Dynamic/Triggered closure" = "#beaed4", "Rotational closure" = "#fdc086", "Determinate closure" = "#ffff99")


# make figure
g_empirical_type <- ggplot(data = empirical_bar, aes(x= reorder(country_clean, -total_cases_all), y = total_cases, fill = short_type_of_closure_clean)) +
  facet_grid(source~., space = "free_y", scales = "free_y") +
  geom_bar(position = position_stack(), stat = "identity", color = "grey30", lwd = 0.2) +
  scale_fill_manual(name = "Closure types", values = closure_type) +
  labs(x = "", y = "Number of cases", tags = "B") +
  scale_y_continuous(lim = c(0, 11), breaks = seq(0, 10, 5)) +
  theme_bw() + bar_theme + theme(axis.text.x = element_text(angle = 45, hjust = 1),
                                 legend.position = c(0.7, 0.75),
                                 legend.key.size = unit(0.3, "cm"),
                                 legend.text = element_text(size = 8),
                                 legend.title = element_text(size = 8),
                                 plot.tag = element_text(size = 10, face = "bold"))

g_empirical_type

g_modeling_type <- ggplot(data = modeling_bar, aes(x= reorder(country_clean, -total_cases_all), y = total_cases, fill = objectives_scenarios_clean)) +
  facet_grid(source~., space = "free_y", scales = "free_y") +
  geom_bar(position = position_stack(), stat = "identity", color = "grey30", lwd = 0.2) +
  scale_fill_manual(name = "Closure types", values = closure_type) +
  labs(x = "", y = "Number of cases", tag = "C") +
  scale_y_continuous(lim = c(0, 8), breaks = seq(0, 8, 4)) +
  theme_bw() + bar_theme + theme(axis.text.x = element_text(angle = 45, hjust = 1),
                                 legend.position = "none",
                                 plot.tag = element_text(size = 10, face = "bold"))

g_modeling_type


# Convert to grobs
gA_grob <- ggplotGrob(worldplot)
gB_grob <- ggplotGrob(g_empirical_type)
gC_grob <- ggplotGrob(g_modeling_type)

# Align the bar plots (B and C) so they have matching widths
max_widths <- grid::unit.pmax(gB_grob$widths, gC_grob$widths)
gB_grob$widths <- max_widths
gC_grob$widths <- max_widths

# Combine B and C side by side in one row
bar_row <- gridExtra::arrangeGrob(gB_grob, gC_grob, ncol = 2)

# Stretch A to same width as B+C by aligning widths
gA_grob$widths <- grid::unit.pmax(gA_grob$widths, bar_row$widths)

g_total <- gridExtra::grid.arrange(worldplot, bar_row, ncol = 1, heights = c(1.2, 1))


# save the world map
ggsave(g_total, filename = file.path(plotdir, "Figx_closure_location.png"), width = 7.5, height = 7.5, units = "in", dpi = 600)



