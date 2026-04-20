library(dplyr)
library(tidyverse)
library(ggplot2)

summary_data <- finallong %>%
  group_by(Mean.time.of.deposition, D_level) %>%
  summarise(count = sum(Count), .groups = "drop")




test2 <- finallong %>% group_by(D_level,sample_num, Mean.time.of.deposition, Depth..m.,Duration,Elements,) %>% summarize(Count = sum(Count))

unique <- unique(test2$Elements)

taxon <- taxon %>% filter(species %in% unique)

combined_data <- test2 %>%
  left_join(taxon, by = c("Elements" = "species"))


d_order <- paste0("D", 1:21)

combined_data <- combined_data %>%
  mutate(D_level = factor(D_level, levels = d_order))

plot_data <- combined_data %>%
  filter(major_group != "Unidentified") %>%
  group_by(major_group, Mean.time.of.deposition) %>%
  summarise(total_val = sum(Count, na.rm = T), .groups = "drop") 


ggplot(plot_data %>% filter(!is.na(Mean.time.of.deposition)), aes(x = Mean.time.of.deposition, y = total_val,  group = major_group)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(
    title = "Total Counts by Major Group across time points",
    x = "Mean time of deposition",
    y = "Sum of Counts",
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~major_group, scales="free") +
  geom_smooth()

#The graph above shows the counts for the different major groups through time.
#It is important to note that the scales on the Y-axis are different for the different groups.
#The groups that seem to be the most prevelant are Centric, NDO, and Pennate.


#hypothesis test for trend between time and count
#time point pairs
#deviation form constant mean
