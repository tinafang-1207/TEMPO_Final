
### clean working environment ###
rm(list = ls())

### read library ###
library(tidyverse)

### set up plot directory ###
plotdir <- "figure"

### read in data ###
data_orig <- read.csv("data/Cleaned sheets - Full-text screening - RESULTS_ECOLOGICAL.csv", na.strings = "/") %>%
  janitor::clean_names()

### clean data ###
data <- data_orig %>%
  rename(sample_size_control = sample_sizes_control_i_e_number_of_transects_fishing_trips,
         sample_size_closure = sample_sizes_control_i_e_number_of_transects_fishing_trips_1) %>%
  select(ref_id, 
         control_type, 
         reported_ecological_variable, 
         reported_value_control, 
         reported_standard_error_control, 
         reported_value_closure,
         reported_standard_error_closure,
         sample_size_control,
         sample_size_closure,
         clusters) %>%
  filter(!is.na(reported_value_control)) %>%
  mutate(control_type = ifelse(control_type=="oprn access", "open access", control_type)) %>%
  mutate(reported_ecological_variable = ifelse(reported_ecological_variable == "abundance ", "abundance", reported_ecological_variable))

# abundance and biomass
data_response_ratio_bar <- data %>%
  filter(ref_id == "013-BAR-09-NE") %>%
  mutate(reported_ecological_variable = case_when(reported_ecological_variable == "biomass-vulnerable"~"biomass",
                                                  reported_ecological_variable == "biomass-non-vulnerable"~"biomass",
                                                  reported_ecological_variable == "abundance-vulnerable"~"abundance",
                                                  reported_ecological_variable == "abundance-non-vulnerable"~"abundance")) %>%
  mutate(response_ratio_orig = reported_value_closure/reported_value_control) %>%
  mutate(study_variance = ((reported_standard_error_control)^2/(sample_size_control*reported_value_control)) + ((reported_standard_error_closure)^2/(sample_size_closure*reported_value_closure))) %>%
  # calculate the average
  group_by(control_type, reported_ecological_variable) %>% 
  mutate(response_ratio_avg = mean(response_ratio_orig),
         study_variance_avg = mean(study_variance)) %>%
  ungroup() %>%
  filter(!duplicated(study_variance_avg)) %>%
  select(-response_ratio_orig, -study_variance) %>%
  rename(response_ratio_orig = response_ratio_avg,
         study_variance = study_variance_avg)
  
data_response_ratio_non_cpue <- data %>%
  filter(ref_id != "013-BAR-09-NE" & ref_id != "001-CIN-19-MUL") %>%
  filter(str_detect(reported_ecological_variable, "biomass|abundance")) %>%
  mutate(response_ratio_orig = reported_value_closure/reported_value_control) %>%
  mutate(study_variance = ((reported_standard_error_control)^2/(sample_size_control*reported_value_control)) + ((reported_standard_error_closure)^2/(sample_size_closure*reported_value_closure))) %>%
  bind_rows(data_response_ratio_bar) %>%
  mutate(response_ratio_log = log(response_ratio_orig)) %>%
  mutate(reported_ecological_variable = case_when(reported_ecological_variable == "biomass"~"Biomass",
                                                  reported_ecological_variable == "abundance"~"Abundance")) %>%
  mutate(study_variance = ifelse(study_variance >10, NA, study_variance)) %>%
  mutate(ref_id = fct_reorder(ref_id, response_ratio_log)) 

# cpue

data_cpue_through_time <- data %>%
  filter(str_detect(reported_ecological_variable, "cpue")) %>%
  filter(ref_id %in% c("067-OLI-15-VEL", "039-BEN-14-ANK", "039-BEN-14-NM", "039-BEN-14-AMP")) %>%
  mutate(reported_ecological_variable = case_when(reported_ecological_variable == "catch-cpue-ban-before"~"catch-cpue-before",
                                                  reported_ecological_variable == "catch-cpue-ban-after"~"catch-cpue-after",
                                                  .default = reported_ecological_variable)) %>%
  select(-reported_standard_error_closure, -reported_standard_error_control, -sample_size_control, -sample_size_closure) %>%
  pivot_wider(names_from = reported_ecological_variable, values_from = c(reported_value_control, reported_value_closure)) %>%
  mutate(response_ratio_control = `reported_value_control_catch-cpue-after`/`reported_value_control_catch-cpue-before`,
         response_ratio_closure = `reported_value_closure_catch-cpue-after`/`reported_value_closure_catch-cpue-before`) %>%
  mutate(response_ratio_orig = response_ratio_closure/response_ratio_control) %>%
  mutate(response_ratio_log = log(response_ratio_orig)) %>%
  select(ref_id, control_type, clusters, response_ratio_orig, response_ratio_log)

data_response_ratio_cpue <- data %>%
  filter(str_detect(reported_ecological_variable, "cpue")) %>%
  filter(ref_id!="067-OLI-15-VEL" & ref_id!="039-BEN-14-ANK" & ref_id!="039-BEN-14-NM" & ref_id!="039-BEN-14-AMP") %>%
  select(-reported_standard_error_closure, -reported_standard_error_control, -sample_size_control, -sample_size_closure) %>%
  mutate(response_ratio = reported_value_closure/reported_value_control) %>%
  group_by(ref_id) %>%
  mutate(response_ratio_avg = mean(response_ratio)) %>%
  filter(!duplicated(response_ratio_avg)) %>%
  rename(response_ratio_orig = response_ratio_avg) %>%
  select(ref_id, control_type, clusters, response_ratio_orig) %>%
  mutate(response_ratio_log = log(response_ratio_orig)) %>%
  # bind with the through-time study
  bind_rows(data_cpue_through_time) %>%
  mutate(reported_ecological_variable = "Catch-CPUE") %>%
  mutate(ref_id = fct_reorder(ref_id, response_ratio_log))

# percentage change
data_pct_non_cpue <- data_response_ratio_non_cpue %>%
  select(ref_id, control_type, reported_ecological_variable, response_ratio_orig, clusters) %>%
  mutate(pct_change = (response_ratio_orig - 1)*100) 

data_pct_cpue <- data_response_ratio_cpue %>%
  select(ref_id, control_type, reported_ecological_variable, response_ratio_orig, clusters) %>%
  mutate(pct_change = (response_ratio_orig-1) *100)


data_pct_all <- bind_rows(data_pct_non_cpue, data_pct_cpue) %>%
  group_by(reported_ecological_variable) %>%
  mutate(mean_pct_change = mean(pct_change)) %>%
  mutate(reported_ecological_variable = fct_reorder(reported_ecological_variable, -mean_pct_change)) %>%
  mutate(reported_ecological_variable = case_when(reported_ecological_variable == "Biomass"~"Biomass\nN=8",
                                                  reported_ecological_variable == "Abundance"~"Abundance\nN=5",
                                                  reported_ecological_variable == "Catch-CPUE"~"Catch-CPUE\nN=6"))

# data_pct_cpue_avg <- data_pct_cpue %>%
#   group_by(reported_ecological_variable) %>%
#   summarize(mean_pct_change = mean(pct_change))
# 
# data_pct_non_cpue_avg <- data_pct_non_cpue %>%
#   group_by(reported_ecological_variable) %>%
#   summarize(mean_pct_change = mean(pct_change))
# 
# data_pct_mean_all <- bind_rows(data_pct_non_cpue_avg, data_pct_cpue_avg) %>%
#   mutate(reported_ecological_variable = fct_reorder(reported_ecological_variable, -mean_pct_change))
  
 ### make graph below ###
 
 plot_theme <- theme(axis.text=element_text(size=7),
                     axis.title=element_text(size=8),
                     legend.text=element_text(size=6),
                     legend.title=element_text(size=7),
                     strip.text = element_text(size=8),
                     plot.title = element_blank(),
                     # Gridlines
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     panel.border = element_rect(colour = "black", fill = NA),
                     axis.line = element_line(colour = "black"),
                     strip.background = element_rect(colour = "black", fill ="grey90"),
                     # Legend
                     legend.background = element_rect(fill=alpha('blue', 0)))
 
clusters <- c("A" = "#00468b", "B" = "#ed0000", "C" = "#42b540", "NA(Model paper)" = "grey90")
 
g_case_biomass <- ggplot(data = data_response_ratio_non_cpue %>% filter(reported_ecological_variable == "Biomass"), aes(x = response_ratio_log, y = ref_id, shape = control_type, fill = clusters)) +
  geom_errorbar(aes(xmin = response_ratio_log - study_variance, xmax = response_ratio_log + study_variance), width = 0.1, linewidth = 0.3) +
  geom_point(size = 2.5, color = "black") +
  scale_x_continuous(limits = c(-2.5, 2.5), breaks = seq(-2.5, 2.5, by = 1)) +
  scale_shape_manual(name = "Type of control", values = c(21,23)) +
  scale_fill_manual(name = "Cluster", values = clusters) +
  labs(x = "Log (Response ratio)", y = "Study") +
  geom_vline(xintercept = 0, color = "black", linetype = 2, linewidth = 0.3) +
  facet_wrap(.~reported_ecological_variable) +
  # theme
  plot_theme+
  guides(fill = guide_legend(override.aes = list(shape = 21, color = "black")))+
  theme(legend.position = "none")
  

g_case_biomass

g_case_abundance <- ggplot(data = data_response_ratio_non_cpue %>% filter(reported_ecological_variable == "Abundance"), aes(x = response_ratio_log, y = ref_id, shape = control_type, fill = clusters)) +
  geom_errorbar(aes(xmin = response_ratio_log - study_variance, xmax = response_ratio_log + study_variance), width = 0.1, linewidth = 0.3) +
  geom_point(size = 2.5, color = "black") +
  scale_x_continuous(limits = c(-5, 5), breaks = seq(-5, 5, by = 2.5)) +
  scale_shape_manual(name = "Type of control", values = c(21,23)) +
  scale_fill_manual(name = "Cluster", values = clusters) +
  labs(x = "Log (Response ratio)", y = "Study") +
  geom_vline(xintercept = 0, color = "black", linetype = 2, linewidth = 0.3) +
  facet_wrap(.~reported_ecological_variable) +
  # theme
  plot_theme+
  guides(fill = guide_legend(override.aes = list(shape = 21, color = "black")))+
  theme(legend.position = "none",
        axis.title.y = element_blank())

g_case_abundance 

g_case_cpue <- ggplot(data = data_response_ratio_cpue, aes(x = response_ratio_log, y = reorder(ref_id, response_ratio_log), shape = control_type, fill = clusters)) +
  geom_point(size = 2.5, color = "black") +
  #scale_x_continuous(limits = c(-5, 5), breaks = seq(-5, 5, by = 2.5)) +
  scale_shape_manual(name = "Type of control", values = c(21,23)) +
  scale_fill_manual(name = "Cluster", values = clusters) +
  labs(x = "Log (Response ratio)", y = "Study") +
  geom_vline(xintercept = 0, color = "black", linetype = 2, linewidth = 0.3) +
  facet_wrap(.~reported_ecological_variable) +
  # theme
  plot_theme+
  guides(fill = guide_legend(override.aes = list(shape = 21, color = "black")))+
  theme(legend.position = "none")

g_case_cpue

g_pct_change <- ggplot(data_pct_all, aes(x = reorder(reported_ecological_variable, -mean_pct_change), y = pct_change)) +
  stat_summary(fun = mean, geom = "bar", fill = "grey90", color = "black", width = 0.6, linewidth = 0.3) +
  geom_point(aes(fill = clusters, shape = control_type), position = position_nudge(x = 0), size = 1.5) +
  #scale_y_continuous(limits = c(0, 205), sec.axis = sec_axis(~.+300, name = "Zoomed In")) +
  scale_shape_manual(name = "Type of control", values = c(21,23)) +
  scale_fill_manual(name = "Cluster", values = clusters) +
  labs(y = "Change in Biological Measures (%)") +
  guides(fill = guide_legend(override.aes = list(shape = 21, color = "black")))+
  plot_theme +
  theme(axis.title.x = element_blank(),
        legend.position = c(0.8, 0.8),
        legend.text = element_text(size = 6),
        legend.spacing = unit(0.1, "cm"),
        legend.key.size = unit(0.3, "cm"))

g_pct_change

g_total <- gridExtra::grid.arrange(g_case_biomass,g_case_abundance, g_case_cpue, g_pct_change, ncol = 2, widths = c(1/2, 1/2), heights = c(0.5, 0.5))

ggsave(g_total, filename = file.path(plotdir, "Figx_ecological_impacts_response_ratio.png"), width = 7, height = 7, units = "in", dpi = 600)


