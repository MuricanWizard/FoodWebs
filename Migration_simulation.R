source('FoodWebFunctions.R')
source('Robustness.R')
source('Migration_Support_Functions.R')

S = 50    ## set species richness
C = 0.05   ## set connectance
N = 1     ## set the number of replicate webs to make
L = S^2*C  ## calculate number of links from S and C

#Mainland <- Generate_Mainland() #Cascade foodweb model of the mainland

Island <- Generate_Empty_Island(5)
Plot.matrix(Mainland)
Robustness_timeline <- c()
Plot.matrix(Mainland)
box()
Mainland_colSums <- colSums (Mainland, na.rm = FALSE, dims = 1)
print(Mainland)
print(Mainland_colSums)
Island <- matrix(0, 50, 50) #empty island's foodweb
print(Island)
migrated_species_list <- list()


Plot.matrix(Island)
box()


self_Sustaining <- list() #Self sustaining species (Which don't eat anything)

j <- 1
for(i in 1:50){
  if(Mainland_colSums[i] == 0){
    self_Sustaining[j] <- i
    j <- j + 1
  }
}


potential_migrants <- list()
migrated_species <- list()
count <- 0
abundancy <- list()
#Actual migration
migrations <- sample(1:50, 5, replace = F)
migrating_self_sustaining <- list()
actual_migrating_species <- list()
  
  #Loop which stores self sustaining migrating species in a list - migrating_self_sustaining
  for(i in 1:5){
    for(j in 1:length(self_Sustaining)){
      if(migrations[i] == (as.integer(self_Sustaining))[j]){
        migrating_self_sustaining[length(migrating_self_sustaining) + 1] <- self_Sustaining[j]
      }
    }
  }
  if(length(migrating_self_sustaining) > 0){
  for(i in 1:length(migrating_self_sustaining)){
    actual_migrating_species[length(actual_migrating_species) + 1] <- migrating_self_sustaining[i]
  }
  }
  #Comparisons which determine the survival of the species
  for(i in 1:5){
    species_dependency <- list()
    available_dependencies <- list()
    for(j in 1:50){
      if(Mainland[migrations[i], j] == 1){
        species_dependency[length(species_dependency) + 1] <- j
      }
    }
    if(length(species_dependency) > 0){
      if(length(migrated_species) > 0){
        for(k in 1:length(species_dependency)){
          for(l in 1:length(migrated_species)){
            if(!(is.null(migrated_species)) && as.integer(species_dependency)[k] == as.integer(migrated_species)[l]){
              available_dependencies[length(available_dependencies) + 1] <- species_dependency[k]
            }
          }
        }
      }
      for(m in 1:length(species_dependency)){
        for(n in 1:5){
          if(species_dependency[m] == migrations[n]){
            available_dependencies[length(available_dependencies) + 1] <- species_dependency[m]
          }
        }
      }
    }
    #Migration into island foodweb
    if(length(available_dependencies) > 0){
      actual_migrating_species[length(actual_migrating_species) + 1] <- migrations[i]
      for(o in 1:length(available_dependencies)){
        Island[migrations[i], (as.integer(available_dependencies))[o]] <- 1
      }
    }
    if(length(actual_migrating_species)!=0){
    for(p in 1:length(actual_migrating_species)){
      migrated_species[length(migrated_species) + 1] <- actual_migrating_species[p]
    }
    }
  }
#Count
tempCount <- length(migrated_species)
remove <- 0
for(i in 1:length(migrated_species)){
  ref <- migrated_species[i]
  for(j in (i+1):length(migrated_species)){
     if(as.integer(ref) == as.integer(migrated_species[j])){
      remove <- remove + 1
    }
  }
}
tempCount <- tempCount - remove
abundancy[length(abundancy) + 1] <- tempCount
Plot.matrix(Island)

#Robustness
SpeciesCount <- tempCount
halfPopulation <- tempCount/2
tempRobustness <- 0
attempts <- 0
for(r in 1: tempCount){
  attempts <- attempts + 1
  tempCount <- tempCount - 1
  if(tempCount <= halfPopulation){
    Robustness_timeline[length(Robustness_timeline) + 1] <- attempts/SpeciesCount
    return()
  }
}
#Graphing Robustness
x <- Robustness_timeline; y <- x # create some data
par(pch=100, col="black") # plotting symbol and color 
par(mfrow=c(1,1)) # all plots on one page 
  heading = paste("Robustness") 
  plot(x, y, main=heading) 
  lines(x, y, type = "l")

#Annual migration
#migrations <- sample(1:50, 5, replace = F)
#for(i in 1:5){
#  migrated_species_list[length(migrated_species_list) + 1] <- migrations[i]  
#}
# print(migrated_species_list)
#for(i in 1:5){
  Island[migrations[i],] <- Mainland[migrations[i],]
  Island[,migrations[i]] <- Mainland[,migrations[i]]
#}

 

      
 