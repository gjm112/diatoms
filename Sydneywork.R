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
  mutate(D_level = factor(D_level, levels = d_order)) %>%
  filter(!is.na(D_level)) %>%
  droplevels()


#Calculate total specimens counted (N_total) per sample layer
sample_totals <- combined_data %>%
  group_by(D_level, Mean.time.of.deposition) %>%
  summarise(N_total = sum(Count), .groups = "drop")


occurrence_check <- combined_data %>%
  filter(!is.na(morphotype), morphotype != "") %>%
  group_by(morphotype, D_level) %>%
  summarise(present = sum(Count) > 0, .groups = "drop") %>%
  group_by(morphotype) %>%
  summarise(n_present = sum(present), n_levels = n(), .groups = "drop") %>%
  mutate(prop_present = n_present / n_levels)

keep_morphotypes <- occurrence_check %>%
  filter(prop_present >= 0.5) %>%
  pull(morphotype)

excluded_morphotypes <- occurrence_check %>%
  filter(prop_present < 0.5)

cat("Excluded morphotypes (too sparse):\n")
print(excluded_morphotypes)

combined_data_morphotype <- combined_data %>%
  filter(morphotype %in% keep_morphotypes)

#Helper function to compute y (relative abundance) & vv (binomial variance)
get_paleots_list <- function(data, group_var) {
  
  summary_df <- data %>%
    filter(!is.na(.data[[group_var]]), .data[[group_var]] != "") %>%
    group_by(D_level, .data[[group_var]]) %>%
    summarise(
      mm = mean(Count),
      vv = var(Count),
      nn = n(),
      tt = head(Mean.time.of.deposition, 1),
      .groups = "drop"
    ) %>%
    arrange(tt)
  
  split_data <- split(summary_df, summary_df[[group_var]])
  
  lapply(split_data, function(sub_df) {
    # Drop rows with NA variance (e.g. nn = 1) before pooling
    valid <- !is.na(sub_df$vv) & sub_df$vv > 0
    pooled_vv <- pool.var(sub_df$vv[valid], sub_df$nn[valid])
    
    as.paleoTS(
      mm = sub_df$mm,
      vv = rep(pooled_vv, nrow(sub_df)),
      nn = sub_df$nn,
      tt = sub_df$tt
    )
  })
}


ts_major_group <- get_paleots_list(combined_data, "major_group")
ts_morphotype  <- get_paleots_list(combined_data_morphotype, "morphotype")
ts_eco_group   <- get_paleots_list(combined_data, "ecological_group")


fit_5_models <- function(ts_obj) {
  m1_ss  <- fitSimple(ts_obj, model = "StrictStasis")
  m2_sta <- fitSimple(ts_obj, model = "Stasis")
  m3_urw <- fitSimple(ts_obj, model = "URW")          
  m4_brw <- fitSimple(ts_obj, model = "GRW")          
  m5_ou  <- fitSimple(ts_obj, model = "OU")           
  
  compareModels(m1_ss, m2_sta, m3_urw, m4_brw, m5_ou)
}



print_labeled_results <- function(results_list, category_name) {
  cat("\n==================================================\n")
  cat(" CATEGORY:", toupper(category_name), "\n")
  cat("==================================================\n")
  
  for (group_name in names(results_list)) {
    cat("\n--------------------------------------------------\n")
    cat(" GROUP:", group_name, "\n")
    cat("--------------------------------------------------\n")
    print(results_list[[group_name]])
  }
}


results_major_group <- lapply(ts_major_group, fit_5_models)
results_morphotype  <- lapply(ts_morphotype, fit_5_models)
results_eco_group   <- lapply(ts_eco_group, fit_5_models)


print_labeled_results(results_major_group, "Major Group")
print_labeled_results(results_morphotype, "Morphotype")
print_labeled_results(results_eco_group, "Ecological Group")

for (nm in names(ts_morphotype)) {
  obj <- ts_morphotype[[nm]]
  cat(nm, "\n")
  print(obj$vv)
  print(obj$mm)
  print(obj$tt)
  cat("\n")
}
