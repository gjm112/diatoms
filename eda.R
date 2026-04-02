library(tidyverse)
taxon <- read.csv("./data/taxonomy_species_table.csv")
final <- read.csv("./data/Final_diatom_phytolith_by_transect.csv")
#remove Some morphotypes are labeled "Others_Undetermined", for both diatoms and phytoliths (columns BV-CC, DG-DM). These appear to be novel species/types that we couldn't assign to known species/taxa. Remove these.
final <- final %>% 
  select(-any_of(c(
    paste0("Diatom_Unidentified_", LETTERS[1:5]),
    "Diatom_Unknown_Diatom",
    "Diatom_Unknown_Diatom.fragment",
    "Others_Granules",
    paste0("Others_Undetermined_", LETTERS[7:12])
  )))


finallong <- final %>% pivot_longer(cols = Diatom_Symmetric_Biraphid_spp.:Others_Charred, names_to = "Elements", values_to = "Count") 
finallong$Count[is.na(finallong$Count)] <- 0
test <- finallong %>% group_by(D_level,sample_num,  Depth..m.,Duration,Elements,) %>% summarize(Count = sum(Count))

#group_by transect and sum the counts.  
#Then we left join the 
#time series stuff.  

finallong %>% view()
taxon %>% view()


