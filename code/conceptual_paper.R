
### Clean working environment ###
rm(list = ls())

### Read in package ###
library(tidyverse)
library(scatterpie)

### set up plot directory ###
plotdir <- "figure"

### read in data ###
data_orig <- read.csv("data/Cleaned sheets - Full-text screening - Emperical_Conceptual Paper Definition.csv", na.strings = "/") %>%
  janitor::clean_names()

country_location <- read.csv("data/country_location.csv")

### clean data ###
data_clean <- data_orig %>%
  select(country_clean, short_type_of_closure_clean, system_duration, cyclicity, access_ratio_annual, primary_open_criteria) %>%
  filter(!is.na(system_duration)) %>%
  mutate(country_clean = stringr::str_squish(country_clean)) %>%
  mutate(country_clean = case_when(country_clean == "Norway, and countries in EU (Danish, Swedish, German, Dutch, Belgium)"~"Norway",
                                   country_clean == "Chile"~"Chile(Easter Island)",
                                   country_clean == "Soloman Islands"~"Solomon Islands",
                                   .default = country_clean)) %>%
  mutate(conceptual_closure_type = paste(system_duration,cyclicity, sep = "; "))

########################################################################
# Create figure for closure type

data_conceptual_type <- data_clean %>%
  group_by(country_clean, conceptual_closure_type) %>%
  summarize(case_count = n()) %>%
  ungroup()

data_total_case <- data_conceptual_type %>%
  group_by(country_clean) %>%
  summarize(total_case = sum(case_count))

data_conceptual_type_combine <- left_join(data_conceptual_type, data_total_case, by = "country_clean") %>%
  pivot_wider(names_from = conceptual_closure_type, values_from = case_count) %>%
  mutate_all(~ifelse(is.na(.), 0, .)) %>%
  rename(nt_c = `Non-time bound; Cyclical`,
         t_c = `Time-bound; Cyclical`,
         t_nc = `Time-bound; Non-cyclical`) %>%
  mutate(p_nt_c = (nt_c/total_case)*100,
         p_t_c = (t_c/total_case)*100,
         p_t_nc = (t_nc/total_case)*100) %>%
  mutate(p_nt_c = round(p_nt_c,0),
         p_t_c = round(p_t_c, 0),
         p_t_nc = round(p_t_nc, 0)) %>%
  left_join(country_location, by = "country_clean") %>%
  mutate(radius = case_when(total_case<= 2~2,
                          total_case>2 & total_case<=4~4,
                          total_case>4 & total_case<=6~6,
                          total_case>6 & total_case<=10~8))

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
                                                 "Tanzania",
                                                 "Chile")~"Yes",
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

paper_fill <- c("p_nt_c" = "#6ba3d6", "p_t_c" = "#ea6b73", "p_t_nc" = "#e9c39b")

# Create radius labeller
radius_labeller <- function(radius, label){
  
  label = radius
  
  return (label)
}


worldplot <- ggplot(world_final, aes(long, lat, fill = has_temporary, group = group)) +
  geom_map(map = world_final, aes(map_id = region), color = "grey", linewidth = 0.3, show.legend = F) +
  scale_fill_manual(name = "Has temporary", values = country_fill) +
  ggnewscale::new_scale_fill() +
  geom_scatterpie(data = data_conceptual_type_combine,
                  aes(x = country_long, y = country_lat,r=radius),
                  cols = c("p_nt_c", "p_t_c", "p_t_nc"),
                  color = "black",
                  linewidth = 0.3)+
  geom_scatterpie_legend(r = data_conceptual_type_combine$radius,
                         x = -170,
                         y = -40,
                         n = 4,
                         labeller = radius_labeller) +
  scale_fill_manual(name = "Closure type", values = paper_fill, labels = c("p_nt_c" = "Non-time bound; Cyclical", "p_t_c" = "Time-bound; Cyclical", "p_t_nc" = "Time-bound; Non-cyclical")) +
  coord_fixed(1.3) +
  theme_bw() + base_theme + theme(legend.position = "bottom")

worldplot

# save the plot
ggsave(worldplot, filename = file.path(plotdir, "Figx_conceptual_closure_type.png"), width = 7.5, height = 5, units = "in", dpi = 600)

################################################################################
# Access ratio



data_access <- data_clean %>%
  select(access_ratio_annual, primary_open_criteria) %>%
  filter(!is.na(access_ratio_annual)) %>%
  mutate(primary_open_criteria = factor(primary_open_criteria, levels = c("Social", "Fisheries management/Fishing income", "Ecosystem/Conservation")))


open_criteria <- c("Fisheries management/Fishing income" = "#6ba3d6", "Ecosystem/Conservation" = "#ea6b73", "Social" = "#e9c39b")


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

g_access_ratio <- ggplot(data_access, aes(x = primary_open_criteria, y = access_ratio_annual)) +
  geom_boxplot(outlier.shape = 21, linewidth = 0.2, outlier.size = 1, outlier.stroke = 0.2) +
  geom_jitter(aes(color = primary_open_criteria), width = 0.1, alpha = 0.8) +
  labs(x = "Open Criteria", y = "Access ratio (Annual)") +
  scale_color_manual(name = "Open criteria", values = open_criteria) +
  scale_y_continuous(trans = "log10", labels = scales::label_number()) +
  boxplot_theme + theme(legend.position = "none",
                        strip.background = element_rect(color = "black",fill = "gray90"),
                        axis.title.x = element_blank())

g_access_ratio

ggsave(g_access_ratio, filename = file.path(plotdir, "Figx_access_ratio.png"), width = 7.5, height = 5, units = "in", dpi = 600)












