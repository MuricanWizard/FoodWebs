library(R6)
source('Migration_Structured_Support_Functions.R')
MainlandTest <- Structured_Ecosystem$new()
MainlandTest$generate_interactions_newWeb(1)
MainlandTest$record_parameters()
for(i in 1:50){
  MainlandTest$species_list[[i]]$activate_species()
}
MainlandTest$simulate_secondary_extinctions()
MainlandTest$get_population()
print(MainlandTest$calculate_robustness())

IslandTest <- Structured_Ecosystem$new()
data <- migrate_structured_getpop(MainlandTest, IslandTest, 100)
write.csv(data, "p1.csv")
