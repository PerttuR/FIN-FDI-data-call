IC_DB_SSF <- IC_DB %>% filter(
  Year ==2024 & 
    SeasonType=="Quarter" & 
    Species=="HER" & 
    NumAgeMeas!=0 & 
    NumAgeMeas!=-9
)

IC_DB_SSF2 <- IC_DB_SSF %>% mutate(
  Fleet = case_when(
    Fleet == "Active" ~ ">12",
    Fleet == "Pelagic trawl" ~ ">12",
    Fleet == "Trapnet" ~ "<12",
    Fleet == "Gillnet" ~ "<12",
    TRUE ~ "Tuntematon"  ))

write.csv(IC_DB_SSF2, file = "intercatch_her.27.3d_landings_age.csv", row.names = FALSE, fileEncoding = "UTF-8")
