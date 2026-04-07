


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


combined_data <- test %>%
  left_join(taxon, by = c("Elements" = "species"))


d_order <- paste0("D", 1:21)

combined_data <- combined_data %>%
  mutate(D_level = factor(D_level, levels = d_order))

plot_data <- combined_data %>%
  group_by(major_group, D_level) %>%
  summarise(total_val = sum(Count, na.rm = TRUE), .groups = "drop") 

ggplot(plot_data, aes(x = D_level, y = total_val, color = major_group, group = major_group)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(
    title = "Total Counts by Major Group across D-Levels",
    x = "D-Level",
    y = "Sum of Counts",
    color = "Major Group"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))