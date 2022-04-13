library(R6)
source('Migration_simulation_Structured.R')
source('Migration_Structured_Support_Functions.R')

pop <- list()
pVal <- c(0, 0.01, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
#store population dataframe with variable p's in a list pop, c = 0.05(default)
for(i in 1:300){
  Mainland = Structured_Ecosystem$new()
  Island = Structured_Ecosystem$new()
  p = pVal[(i%%length(pVal)) + 1]
  Mainland$generate_interactions(pVal)
  Mainland$record_species()
  Mainland$get_population()
  tempVector <- migrate_structured(Mainland, Island, 100)
  tempVector[length(tempVector) + 1] = p
  pop[[length(pop) + 1]] <- tempVector
}
write.table(pop, file = "popC0.05.csv",row.names=FALSE, na="",col.names=FALSE, sep=",")

#store population dataframe with variable p's in a list pop, c = 0.1 
pop2 <- list()
for(i in 1:300){
  Mainland = Structured_Ecosystem$new()
  Island = Structured_Ecosystem$new()
  p = pVal[(i%%length(pVal)) + 1]
  Mainland$generate_interactions_customFWstats(pVal, 50, 0.1, 1)
  Mainland$record_species()
  Mainland$get_population()
  tempVector <- migrate_structured(Mainland, Island, 100)
  tempVector[length(tempVector) + 1] = p
  pop2[[length(pop2) + 1]] <- tempVector
}
write.table(pop2, file = "popC0.1.csv",row.names=FALSE, na="",col.names=FALSE, sep=",")

#store population dataframe with variable p's in a list pop, c = 0.3
pop3 <- list()
for(i in 1:300){
  Mainland = Structured_Ecosystem$new()
  Island = Structured_Ecosystem$new()
  p = pVal[(i%%length(pVal)) + 1]
  Mainland$generate_interactions_customFWstats(pVal, 50, 0.3, 1)
  Mainland$record_species()
  Mainland$get_population()
  tempVector <- migrate_structured(Mainland, Island, 100)
  tempVector[length(tempVector) + 1] = p
  pop3[[length(pop3) + 1]] <- tempVector
}
write.table(pop3, file = "popC0.3.csv",row.names=FALSE, na="",col.names=FALSE, sep=",")

pop4 <- list()
for(i in 1:300){
  Mainland = Structured_Ecosystem$new()
  Island = Structured_Ecosystem$new()
  p = pVal[(i%%length(pVal)) + 1]
  Mainland$generate_interactions_customFWstats(pVal, 50, 0.4, 1)
  Mainland$record_species()
  Mainland$get_population()
  tempVector <- migrate_structured(Mainland, Island, 100)
  tempVector[length(tempVector) + 1] = p
  pop4[[length(pop4) + 1]] <- tempVector
}
write.table(pop4, file = "popC0.4.csv",row.names=FALSE, na="",col.names=FALSE, sep=",")

pop5 <- list()
for(i in 1:300){
  Mainland = Structured_Ecosystem$new()
  Island = Structured_Ecosystem$new()
  p = pVal[(i%%length(pVal)) + 1]
  Mainland$generate_interactions_customFWstats(pVal, 50, 0.2, 1)
  Mainland$record_species()
  Mainland$get_population()
  tempVector <- migrate_structured(Mainland, Island, 100)
  tempVector[length(tempVector) + 1] = p
  pop5[[length(pop5) + 1]] <- tempVector
}
write.table(pop5, file = "popC0.2.csv",row.names=FALSE, na="",col.names=FALSE, sep=",")

Mainland = Structured_Ecosystem$new()
Island = Structured_Ecosystem$new()
Mainland$generate_interactions(0.5)
Mainland$record_species()
Mainland$get_population()
migrate_structured(Mainland, Island, 2)
Island$simulate_secondary_extinctions()
Island$get_population()
