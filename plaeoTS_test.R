library(paleoTS)
library(dplyr)
library(tidyverse)
library(ggplot2)
taxon <- read.csv("./data/taxonomy_species_table.csv")
final <- read.csv("./data/Final_diatom_phytolith_by_transect.csv")

final <- final %>% 
  dplyr::select(-any_of(c(
    paste0("Diatom_Unidentified_", LETTERS[1:5]),
    "Diatom_Unknown_Diatom",
    "Diatom_Unknown_Diatom.fragment",
    "Others_Granules",
    paste0("Others_Undetermined_", LETTERS[7:12])
  )))

finallong <- final %>% pivot_longer(cols = Diatom_Symmetric_Biraphid_spp.:Others_Charred, names_to = "Elements", values_to = "Count") 
finallong$Count[is.na(finallong$Count)] <- 0
test <- finallong %>% group_by(D_level,sample_num,  Depth..m.,Duration,Elements,) %>% summarize(Count = sum(Count))

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

plot_data %>% filter(major_group == "Biogenic") %>% ggplot(aes(x = Mean.time.of.deposition, y = total_val)) + geom_point() + geom_line()

x <- plot_data %>% filter(major_group == "Biogenic") %>% pull(total_val)
x <- as.paleoTS(x)

w.sta <- fitSimple(x, model = "Stasis")
w.ss <- fitSimple(x, model = "StrictStasis")
compareModels(w.sta, w.ss)