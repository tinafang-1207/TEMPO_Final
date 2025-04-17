
### clean working environment ###
rm(list = ls())

### read in library ###
library(tidyverse)

### read in data ###
data_orig <- read.csv("data/Cleaned sheets - Full-text screening - RESULTS_ECOLOGICAL.csv") %>%
  janitor::clean_names()

### set plot dir ###
plotdir <- "figure"

data <- data_orig %>%
  mutate(paper_type = ifelse(paper_type == "Model", "model", paper_type)) %>%
  mutate(control_type = ifelse(control_type == "oprn access"|control_type == "open access", "Open access", control_type)) %>%
  mutate(control_type = ifelse(control_type == "permanent reserves", "Permanent reserves", control_type)) %>%
  mutate(reported_target_species = case_when(reported_target_species == "reef fish"~"Reef fish",
                                             reported_target_species == "invertebrates"~"Invertebrates")) %>%
  mutate(reported_ecological_variable = case_when(reported_ecological_variable == "biomass"~"Biomass",
                                                  reported_ecological_variable == "abundance"~"Abundance",
                                                  reported_ecological_variable == "abundance "~"Abundance",
                                                  reported_ecological_variable == "catch-weight/size"~"Catch-weight/size",
                                                  reported_ecological_variable == "catch-cpue"~"Catch-CPUE",
                                                  reported_ecological_variable == "catch-total landings"~"Catch-total landings")) %>%
  mutate(clusters = ifelse(is.na(clusters), "NA(Model paper)", clusters)) %>%
  mutate(reported_percentage_changes = as.numeric(sub("%", "",reported_percentage_changes)) / 100)


data_biomass <- data %>%
  filter(reported_ecological_variable == "Biomass") %>%
  mutate(paper_type = factor(paper_type, levels = c("model", "empirical"))) %>%
  group_by(ref_id) %>%
  mutate(reported_percentage_total = sum(reported_percentage_changes)) %>%
  ungroup() %>%
  group_by(paper_type) %>%
  mutate(ref_id = fct_reorder(ref_id, reported_percentage_total)) %>%
  ungroup()


data_abundance <- data %>%
  filter(reported_ecological_variable == "Abundance") %>%
  mutate(paper_type = factor(paper_type, levels = c("model", "empirical"))) %>%
  group_by(ref_id) %>%
  mutate(reported_percentage_total = sum(reported_percentage_changes)) %>%
  ungroup() %>%
  group_by(paper_type) %>%
  mutate(ref_id = fct_reorder(ref_id, reported_percentage_total)) %>%
  ungroup()

data_catch_weight <- data %>%
  filter(reported_ecological_variable == "Catch-weight/size") %>%
  mutate(paper_type = factor(paper_type, levels = c("model", "empirical"))) %>%
  group_by(ref_id) %>%
  mutate(reported_percentage_total = sum(reported_percentage_changes)) %>%
  ungroup() %>%
  group_by(paper_type) %>%
  mutate(ref_id = fct_reorder(ref_id, reported_percentage_total)) %>%
  ungroup()

data_catch_landings <- data %>%
  filter(reported_ecological_variable == "Catch-total landings") %>%
  mutate(paper_type = factor(paper_type, levels = c("model", "empirical"))) %>%
  group_by(paper_type) %>%
  mutate(ref_id = fct_reorder(ref_id, reported_percentage_changes)) %>%
  ungroup()

data_catch_cpue <- data %>%
  filter(reported_ecological_variable == "Catch-CPUE") %>%
  mutate(paper_type = factor(paper_type, levels = c("model", "empirical"))) %>%
  group_by(paper_type) %>%
  mutate(ref_id = fct_reorder(ref_id, reported_percentage_changes)) %>%
  ungroup()
  



#########################################################################

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

# set up the color scheme
clusters <- c("A" = "#00468b", "B" = "#ed0000", "C" = "#42b540", "NA(Model paper)" = "grey90")

g_biomass <- ggplot(data = data_biomass, aes(x = reported_percentage_changes, y = ref_id, fill = clusters, shape = control_type)) +
  geom_point(size = 2.5, color = "black") +
  geom_vline(xintercept = 0, color = "black", linetype = 2, linewidth = 0.3) +
  geom_hline(yintercept = 2.5, color = "black", linetype = 1, linewidth = 0.3)+
  geom_text(data = data_biomass %>% filter(reported_percentage_statistical_significance=="yes"),
            aes(x = reported_percentage_changes, y = ref_id, label = "*"),vjust = 0.5,hjust = -1,size = 4.5) +
  scale_shape_manual(name = "Type of control", values = c(21,23)) +
  scale_fill_manual(name = "Cluster", values = clusters) +
  scale_x_continuous(limits = c(-0.6, 7), breaks = seq(0, 6, by = 2), labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Reported percentage changes") +
  facet_wrap(.~reported_ecological_variable) +
  plot_theme+
  guides(fill = guide_legend(override.aes = list(shape = 21, color = "black"))) +
  theme(axis.title.y = element_blank())

g_biomass

point_legend <- ggpubr::get_legend(g_biomass)

ggpubr::as_ggplot(point_legend)


#########################################################################

g_biomass_no_legend <- ggplot(data = data_biomass, aes(x = reported_percentage_changes, y = ref_id, fill = clusters, shape = control_type)) +
  geom_point(size = 2.5, color = "black") +
  geom_vline(xintercept = 0, color = "black", linetype = 2, linewidth = 0.3) +
  geom_hline(yintercept = 2.5, color = "black", linetype = 1, linewidth = 0.3)+
  geom_text(data = data_biomass %>% filter(reported_percentage_statistical_significance=="yes"),
            aes(x = reported_percentage_changes, y = ref_id, label = "*"),vjust = 0.5,hjust = -1,size = 4.5) +
  scale_shape_manual(name = "Type of control", values = c(21,23)) +
  scale_fill_manual(name = "Cluster", values = clusters) +
  scale_x_continuous(limits = c(-0.6, 7), breaks = seq(0, 6, by = 2), labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Reported percentage changes") +
  facet_wrap(.~reported_ecological_variable) +
  plot_theme+
  guides(fill = guide_legend(override.aes = list(shape = 21, color = "black"))) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none")

g_biomass_no_legend

g_abundance <- ggplot(data = data_abundance, aes(x = reported_percentage_changes, y = ref_id, fill = clusters, shape = control_type)) +
  geom_point(size = 2.5, color = "black") +
  geom_vline(xintercept = 0, color = "black", linetype = 2, linewidth = 0.3) +
  geom_hline(yintercept = 1.5, color = "black", linetype = 1, linewidth = 0.3)+
  geom_text(data = data_abundance %>% filter(reported_percentage_statistical_significance=="yes"),
            aes(x = reported_percentage_changes, y = ref_id, label = "*"),vjust = 0.5,hjust = -1,size = 4.5) +
  scale_shape_manual(name = "Type of control", values = c(21,23)) +
  scale_fill_manual(name = "Cluster", values = clusters) +
  scale_x_continuous(limits = c(0, 2), breaks = seq(0, 2, by = 1), labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Reported percentage changes") +
  facet_wrap(.~reported_ecological_variable) +
  plot_theme+
  guides(fill = guide_legend(override.aes = list(shape = 21, color = "black"))) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none")

g_abundance

g_catch_weight <- ggplot(data = data_catch_weight, aes(x = reported_percentage_changes, y = ref_id, fill = clusters, shape = control_type)) +
  geom_point(size = 2.5, color = "black") +
  geom_vline(xintercept = 0, color = "black", linetype = 2, linewidth = 0.3) +
  geom_text(data = data_catch_weight %>% filter(reported_percentage_statistical_significance=="yes"),
            aes(x = reported_percentage_changes, y = ref_id, label = "*"),vjust = 0.5,hjust = -1,size = 4.5) +
  scale_shape_manual(name = "Type of control", values = c(21,23)) +
  scale_fill_manual(name = "Cluster", values = clusters) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.5), labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Reported percentage changes") +
  facet_wrap(.~reported_ecological_variable) +
  plot_theme+
  guides(fill = guide_legend(override.aes = list(shape = 21, color = "black"))) +
  theme(axis.title.y = element_blank(),
        legend.position = "none")

g_catch_weight

g_catch_landings <- ggplot(data = data_catch_landings, aes(x = reported_percentage_changes, y = ref_id, fill = clusters, shape = control_type)) +
  geom_point(size = 2.5, color = "black") +
  geom_vline(xintercept = 0, color = "black", linetype = 2, linewidth = 0.3) +
  geom_hline(yintercept = 2.5, color = "black", linetype = 1, linewidth = 0.3) +
  geom_text(data = data_catch_landings %>% filter(reported_percentage_statistical_significance=="yes"),
            aes(x = reported_percentage_changes, y = ref_id, label = "*"),vjust = 0.5,hjust = -1,size = 4.5) +
  scale_shape_manual(name = "Type of control", values = c(21,23)) +
  scale_fill_manual(name = "Cluster", values = clusters) +
  scale_x_continuous(limits = c(0, 4.5), breaks = seq(0, 4, by = 2), labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Reported percentage changes") +
  facet_wrap(.~reported_ecological_variable) +
  plot_theme+
  guides(fill = guide_legend(override.aes = list(shape = 21, color = "black"))) +
  theme(axis.title.y = element_blank(),
        legend.position = "none")

g_catch_landings

g_catch_cpue <- ggplot(data = data_catch_cpue, aes(x = reported_percentage_changes, y = ref_id, fill = clusters, shape = control_type)) +
  geom_point(size = 2.5, color = "black") +
  geom_vline(xintercept = 0, color = "black", linetype = 2, linewidth = 0.3) +
  geom_hline(yintercept = 1.5, color = "black", linetype = 1, linewidth = 0.3) +
  geom_text(data = data_catch_cpue %>% filter(reported_percentage_statistical_significance=="yes"),
            aes(x = reported_percentage_changes, y = ref_id, label = "*"),vjust = 0.5,hjust = -1,size = 4.5) +
  scale_shape_manual(name = "Type of control", values = c(21,23)) +
  scale_fill_manual(name = "Cluster", values = clusters) +
  scale_x_continuous(limits = c(-1,8), breaks = seq(0, 8, by = 2), labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Reported percentage changes") +
  facet_wrap(.~reported_ecological_variable) +
  plot_theme+
  guides(fill = guide_legend(override.aes = list(shape = 21, color = "black"))) +
  theme(axis.title.y = element_blank(),
        legend.position = "none")

g_catch_cpue


g_total <- gridExtra::grid.arrange(g_biomass_no_legend,g_abundance, g_catch_weight, g_catch_landings, g_catch_cpue, point_legend, ncol = 3, widths = c(1/3, 1/3, 1/3), heights = c(0.5, 0.5))

ggsave(g_total, filename = file.path(plotdir, "Figx_ecological_impacts.png"), width = 9.8, height = 7, units = "in", dpi = 600)








  
