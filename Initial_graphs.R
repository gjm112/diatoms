library(dplyr)

summary_data <- finallong %>%
  group_by(Mean.time.of.deposition, D_level) %>%
  summarise(count = sum(Count), .groups = "drop")


library(ggplot2)

ggplot(summary_data, aes(x = Mean.time.of.deposition, y = count, color = as.factor(D_level))) +
  geom_line() +
  geom_point() +
  labs(
    x = "Mean Time",
    y = "Count",
    color = "D-Level"
  ) +
  theme_minimal()


#completely messed up this graph, but it does show that we do not have the same count of diatoms throughout the different timepoints
#ranges from about 1225 to 1455. Is this a problem
test2 <- finallong %>% group_by(D_level,sample_num, Mean.time.of.deposition, Depth..m.,Duration,Elements,) %>% summarize(Count = sum(Count))

unique <- unique(test2$Elements)

taxon <- taxon %>% filter(species %in% unique)

combined_data <- test2 %>%
  left_join(taxon, by = c("Elements" = "species"))


d_order <- paste0("D", 1:21)

combined_data <- combined_data %>%
  mutate(D_level = factor(D_level, levels = d_order))

plot_data <- combined_data %>%
  group_by(major_group, Mean.time.of.deposition) %>%
  summarise(total_val = sum(Count, na.rm = T), .groups = "drop") 
#facet wrap with free scales
ggplot(plot_data %>% filter(!is.na(Mean.time.of.deposition)), aes(x = Mean.time.of.deposition, y = total_val, color = major_group, group = major_group)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(
    title = "Total Counts by Major Group across time points",
    x = "Mean time of deposition",
    y = "Sum of Counts",
    color = "Major Group"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~major_group, scales="free") +
  geom_smooth()