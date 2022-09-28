library('tidyverse')
source('FoodWebFunctions.R')

## make some random, cascade, and niche food webs
S = 20## set species richness
C = 0.3   ## set connectance
N = 1     ## set the number of replicate webs to make
L = S^2*C  ## calculate number of links from S and C


xxx <- Niche.model(S, L, N)
Plot.matrix(xxx)
box()
#create grid
abline(v = 1:S, lwd = 0.15) #create vertical lines
abline(h = 1:S, lwd = 0.15) #create horizontal lines

num_resources <- colSums(xxx) #number of species in each column (prey)

num_stages <- rep(NA,S)
for (i in 1:S){
  if (num_resources[i] != 0){
    num_stages[i] <- sample(1:num_resources[i], 1) #set a number of random stages for each species
  }
}

# plot(sort(num_stages))
max_stages <- max(num_stages, na.rm = TRUE) #record the max number of stages


p <- 0.2
stage_list <- list()
for (j in 1:S){
  if (num_resources[j] > 0){
    tmp_stage_mat <- rep(NA, num_stages[j] * num_resources[j]) #creates an empty vector with size = num_stages * num_resources
    for (i in 1:length(tmp_stage_mat)){
      tmp_stage_mat[i] <- rbinom(1, 1, p) #binomial distribution of 1's according to probability 'p'
    }
    
    stage_mat <- array(tmp_stage_mat, dim = c(num_resources[j], num_stages[j])) #row = num_resources, column = num_stages
    
    stage_list[[j]] <- stage_mat #store matrix in a list
  } else {
    stage_list[[j]] <- NA #else set to NA
  }
}

### "we assured that each stage consumed at least one resource"
### need to implement this to make sure all stages have at least on resource. this checks it...
check <- rep(NA, S)
for (i in 1:length(stage_list)){
  if (!is.na(stage_list[[i]][1])){ #if stage_list is not NA
    check[i] <- ncol(stage_list[[i]]) - sum(colSums(stage_list[[i]]) > 0) #not sure what the conditional in the sum part does 
    if (check[i] > 0){
      empty_stages <- which(colSums(stage_list[[i]]) == 0)
      fillin_rows <- sample(1:nrow(stage_list[[i]]), length(empty_stages), replace = TRUE)
      for (j in 1:length(empty_stages)){
        stage_list[[i]][fillin_rows[j], empty_stages[j]] <- 1
      }
    }
  }
}

#Same as above, but for rows instead of columns
for (i in 1:length(stage_list)) {
  if (!is.na(stage_list[[i]][1])) {#if stage_list is not NA
    check <- rowSums(stage_list[[i]])
    for (j in 1:nrow(stage_list[[i]])[[1]]) {
      if (check[j] == 0) {
        column <- sample.int(ncol(stage_list[[i]]), 1)[[1]]
        stage_list[[i]][j, column] <- 1
      }
    }
  }
}


resource_list <- list()
for (i in 1:S){
  resource_list[[i]] <- which(xxx[,i] == 1)
}


stage_matrix <- array(0, dim = c(S,S,max_stages))
for (i in which(!is.na(num_stages))){
  stage_matrix[resource_list[[i]], i, 1:ncol(stage_list[[i]])] <- stage_list[[i]]
}







#############
random_removal <- sample(S,1)

tmp_rm_stage_matrix <- stage_matrix
tmp_rm_stage_matrix[random_removal,,] <- 0  
tmp_rm_stage_matrix[,random_removal,] <- 0  
rm_stage_matrix <- tmp_rm_stage_matrix



rowSums(stage_matrix[,,1])
rowSums(stage_matrix[,,1])

eating_by_stage <- array(dim = c(max_stages, S))
rm_eating_by_stage <- array(dim = c(max_stages, S))

for (i in 1:max_stages){
  eating_by_stage[i,] <- colSums(stage_matrix[,,i])
  rm_eating_by_stage[i,] <- colSums(rm_stage_matrix[,,i])
}


eating_by_stage
rm_eating_by_stage
secondary <- (eating_by_stage > 0) - (rm_eating_by_stage > 0)

tmp_secondary_id <- which(colSums(secondary) > 0)
secondary_id <- tmp_secondary_id[tmp_secondary_id != random_removal]
number_secondary_extinctions <- length(secondary_id)


secondary_id
number_secondary_extinctions

#Blank island
S = 20
yyy <- matrix(rep(0, S), nrow = S, ncol = S)
Plot.matrix(yyy)
box()
abline(v = 1:S, lwd = 0.15) #create vertical lines
abline(h = 1:S, lwd = 0.15) #create horizontal lines

#Fill in with blueprint
for(i in 1:length(xxx)){
  yyy[[i]] <- xxx[i]
}
Plot.matrix(yyy)
box()
abline(v = 1:S, lwd = 0.15)
abline(h = 1:S, lwd = 0.15)



#Reproductive ages
reproductive_ages <- c()
for(i in 1:length(stage_list)){
  if(!is.na(stage_list[[i]])){
    n_stages <- dim(stage_list[[i]])[[2]]
    start_threshold <- ceiling(n_stages/2)
    reproductive_ages[length(reproductive_ages) + 1] <- sample(ceiling(n_stages/2) : n_stages, 1)
    #reproductive_ages[length(reproductive_ages) + 1] <- start_threshold
  }
  else{
    reproductive_ages[length(reproductive_ages) + 1] <- 0
  }
}



#Store Mainland eating habits (with stages)
mainSpecList <- list()
feeding <- colSums(xxx)
for(i in 1:length(feeding)){
  if(feeding[[i]] > 0){
    for(j in 1:dim(stage_list[[i]])[2]){
      for(k in 1:dim(stage_list[[i]])[1]){
        if(stage_list[[i]][k, j] == 1){
          if(is.null(mainSpecList[i][[1]])){
            tempVec <- c(k)
            mainSpecList[[i]] <-tempVec
          }
          else{
            mainSpecList[[i]][length(mainSpecList[[i]]) + 1] = k
          }
        }
      }  
    }
  }
}

mainSpecList <- list()
feeding <- colSums(xxx)
for(i in 1:S){
  mainSpecList[[i]] <- list()
}




#Set self sustaining to 0
for(i in 1:S){
  if(is.null(mainSpecList[i][[1]])){
    mainSpecList[[i]] <- 0
  }
}

presenceMainland <- list()
for(i in 1:length(stage_list)){
  if(is.null(dim(stage_list[[i]]))){
    presenceMainland[[i]] <- c(1)
  }
  else{
    tempVec <- c()
    for(j in 1:dim(stage_list[[i]])[2]){
      tempVec <- append(tempVec, 1)
    }
    presenceMainland[[i]] <- tempVec
    }
}

presenceIsland <- list()
for(i in 1:length(stage_list)){
  if(is.null(dim(stage_list[[i]]))){
    presenceIsland[[i]] <- c(0)
  }
  else{
    tempVec <- c()
    for(j in 1:dim(stage_list[[i]])[2]){
      tempVec <- append(tempVec, 0)
    }
    presenceIsland[[i]] <- tempVec
  }
}

#migrate
#Inputs: (presenceMainland, presenceIsland)
#Returns: presenceIsland with migrations
migrate <- function(Mainland, Island){
  migrating <- sample(c(1:S), size = 5, replace = FALSE)
  for(i in 1:length(migrating)){
    index <- migrating[i]
    Island[[index]] <- Mainland[[index]]
  }
  return(Island)
}

#Converts stage_list prey indices to actual indices mapped on the matrix
#Inputs: (presenceMainland, predatorIndex)
#Returns: vector of all actual prey indices 
stageIndicesToMatrixIndices <- function(MainlandMatrix, predatorIndex){
  ret = c()
  for(i in 1:length(MainlandMatrix[,predatorIndex])){
    if(MainlandMatrix[,predatorIndex][i] == 1){
      ret[length(ret) + 1] <- i
    }
  }
  return(ret)
}


#simulateExtinctions
#Inputs: (presenceMainland, presenceIsland)
#Returns: presenceIsland with extinctions

simulateExtinctions <- function(Mainland, Island){
  for(i in 1:length(Island)){ #iterate through Island (presenceIsland)
    if(sum(Island[[i]]) > 0){ #if there's at least one stage of the species present
      for(j in 1:length(Island[[i]])){
        if(Island[[i]][j] == 1){ #check if the j'th stage of i'th species is present
          if(!(is.na(stage_list[[i]]))){ #stage_list is blueprint of food habits
            resources <- stage_list[[i]][,j] #store all the resources that this stage depends on 
            actualIndices <- stageIndicesToMatrixIndices(xxx, i)
            if(sum(resources) > 0){
              count = 0
              for(k in 1:length(resources)){
                    if(resources[k] == 1){
                      if(sum(Island[[actualIndices[k]]]) > 0){
                        count = count + 1
                      }
                    }
              }
              if(count == 0){
                Island[[i]][j] <- 0
              }
            }
          }
        }
      }
      for(j in 1:length(Island[[i]])){
        if(Island[[i]][j] == 0){
          for(k in j:length(Island[[i]])){
            Island[[i]][k] <- 0
          }
        } 
      }
    }
    for(j in length(Island[[i]]):1){
      if(Island[[i]][j] == 1){
        if(j >= reproductive_ages[i]){
          break
        }
        else{
          for(k in 1:length(Island[[i]])){
            Island[[i]][k] <- 0
          }
        }
      }
    }
  }
  return(Island)
}

simulateExtinctions_multipleIterations <- function(Mainland, Island, iterations){
  temp_Island <- Island
  for(i in 1:iterations){
    temp_Island <- simulateExtinctions(Mainland, temp_Island)
  }
  return(temp_Island)
}

#getPopulation
#Input: Island
#Output: population of species (only counts presence of species, not stages)
getPopulation <- function(Island){
  count <- 0
  for(i in Island){
    if(sum(i) > 0){
      count = count + 1
    }
  }
  return(count)
}

#getActiveSpeciesIDs
#Input: Island
#Output: vector of active species IDs
getActiveSpeciesIDs <- function(Island){
  activeSpecies <- c()
  for(i in 1:length(Island)){
    if(sum(Island[[i]]) > 0){
      activeSpecies[length(activeSpecies) + 1] <- i
    }
  }
  return(activeSpecies)
}

presenceIsland <- migrate(presenceMainland, presenceIsland)
presenceIsland <- presenceMainland
getPopulation(presenceIsland)
getPopulation(presenceMainland)
presenceIsland <- simulateExtinctions_multipleIterations(presenceMainland, presenceIsland, 10)
presenceIsland <- simulateExtinctions(presenceMainland, presenceIsland)
presenceIsland
#Random reproductive ages with at least (stages/2) 
#No Time variable (single iteration in secondary extictions)

#Generate Data
connectanceControlValues <- c(0.01, 0.05, 0.1, 0.2, 0.3, 0.4, 0.45)
pControlValues <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
#connectanceControlValues <- c(0.01)
#pControlValues <- c(1.0)
iterationValues <- c(1,2,3,4,5,6,7,8,9,10)
for(connectance in connectanceControlValues){
for(prob in pControlValues){
for(iterations in iterationValues){

pop_data <- list()
pop_data[[1]] <- c(1:500)
for(superIndex in 1:30){
  S = 100     ## set species richness
  C = connectance   ## set connectance
  N = 1     ## set the number of replicate webs to make
  L = S^2*C  ## calculate number of links from S and C
  
  
  xxx <- Niche.model(S, L, N)
  Plot.matrix(xxx)
  box()
  #create grid
  abline(v = 1:S, lwd = 0.15) #create vertical lines
  abline(h = 1:S, lwd = 0.15) #create horizontal lines
  
  num_resources <- colSums(xxx) #number of species in each column (prey)
  
  num_stages <- rep(NA,S)
  for (i in 1:S){
    if (num_resources[i] != 0){
      num_stages[i] <- sample(1:num_resources[i], 1) #set a number of random stages for each species
    }
  }
  
  # plot(sort(num_stages))
  max_stages <- max(num_stages, na.rm = TRUE) #record the max number of stages
  
  
  p <- prob
  stage_list <- list()
  for (j in 1:S){
    if (num_resources[j] > 0){
      tmp_stage_mat <- rep(NA, num_stages[j] * num_resources[j]) #creates an empty vector with size = num_stages * num_resources
      for (i in 1:length(tmp_stage_mat)){
        tmp_stage_mat[i] <- rbinom(1, 1, p) #binomial distribution of 1's according to probability 'p'
      }
      
      stage_mat <- array(tmp_stage_mat, dim = c(num_resources[j], num_stages[j])) #row = num_resources, column = num_stages
      
      stage_list[[j]] <- stage_mat #store matrix in a list
    } else {
      stage_list[[j]] <- NA #else set to NA
    }
  }
  
  ### "we assured that each stage consumed at least one resource"
  ### need to implement this to make sure all stages have at least on resource. this checks it...
  check <- rep(NA, S)
  for (i in 1:length(stage_list)){
    if (!is.na(stage_list[[i]][1])){ #if stage_list is not NA
      check[i] <- ncol(stage_list[[i]]) - sum(colSums(stage_list[[i]]) > 0) #not sure what the conditional in the sum part does 
      if (check[i] > 0){
        empty_stages <- which(colSums(stage_list[[i]]) == 0)
        fillin_rows <- sample(1:nrow(stage_list[[i]]), length(empty_stages), replace = TRUE)
        for (j in 1:length(empty_stages)){
          stage_list[[i]][fillin_rows[j], empty_stages[j]] <- 1
        }
      }
    }
  }
  
  #Same as above, but for rows instead of columns
  for (i in 1:length(stage_list)) {
    if (!is.na(stage_list[[i]][1])) {#if stage_list is not NA
      check <- rowSums(stage_list[[i]])
      for (j in 1:nrow(stage_list[[i]])[[1]]) {
        if (check[j] == 0) {
          column <- sample.int(ncol(stage_list[[i]]), 1)[[1]]
          stage_list[[i]][j, column] <- 1
        }
      }
    }
  }
  
  
  resource_list <- list()
  for (i in 1:S){
    resource_list[[i]] <- which(xxx[,i] == 1)
  }
  
  
  stage_matrix <- array(0, dim = c(S,S,max_stages))
  for (i in which(!is.na(num_stages))){
    stage_matrix[resource_list[[i]], i, 1:ncol(stage_list[[i]])] <- stage_list[[i]]
  }
  
  #Reproductive ages
  reproductive_ages <- c()
  for(i in 1:length(stage_list)){
    if(!is.na(stage_list[[i]])){
      n_stages <- dim(stage_list[[i]])[[2]]
      start_threshold <- ceiling(n_stages/2)
      reproductive_ages[length(reproductive_ages) + 1] <- sample(ceiling(n_stages/2) : n_stages, 1)
      #reproductive_ages[length(reproductive_ages) + 1] <- start_threshold
    }
    else{
      reproductive_ages[length(reproductive_ages) + 1] <- 0
    }
  }
  
  presenceMainland <- list()
  for(i in 1:length(stage_list)){
    if(is.null(dim(stage_list[[i]]))){
      presenceMainland[[i]] <- c(1)
    }
    else{
      tempVec <- c()
      for(j in 1:dim(stage_list[[i]])[2]){
        tempVec <- append(tempVec, 1)
      }
      presenceMainland[[i]] <- tempVec
    }
  }
  
  presenceIsland <- list()
  for(i in 1:length(stage_list)){
    if(is.null(dim(stage_list[[i]]))){
      presenceIsland[[i]] <- c(0)
    }
    else{
      tempVec <- c()
      for(j in 1:dim(stage_list[[i]])[2]){
        tempVec <- append(tempVec, 0)
      }
      presenceIsland[[i]] <- tempVec
    }
  }
  population <- c()
  for(superIndex2 in 1:500){
    presenceIsland <- migrate(presenceMainland, presenceIsland)
    presenceIsland <- simulateExtinctions_multipleIterations(presenceMainland, presenceIsland, iterations)
    population <- append(population, getPopulation(presenceIsland))
  }
  pop_data[[length(pop_data) + 1]] <- population
}
fileName <- paste0("allData/C", connectance, "/p", prob, "/iteration",iterations , "/pop_data_C", connectance, "_p", prob, "_iteration", iterations, ".csv")
print(fileName)
write.table(pop_data, file = fileName,row.names=FALSE, na="",col.names=FALSE, sep=",")
}
}
}


#Graph for time against population/2 for each p
#Graph for max population reached for each p
#Add n number of iterations while checking for survival - make a function