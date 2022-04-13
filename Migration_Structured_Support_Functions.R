source('FoodWebFunctions.R')
library(R6)

generate_reproductive_stage = function(){
  choices <- c(1,1,2,2,2,2,3,3,3,3,3,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,6,6,6,6,6,6,6,6,6,7,7,7,8,8,9,10)
  i = sample(1:length(choices), 1, replace = FALSE)
  rs <- choices[i[1]]
  return(rs)
}

Interactions <- R6Class("Interactions", private = list(), public = list(S = 10, C = 0.05, N = 1, L = S^2*C,
                                                                        Interaction_matrix = Random.model(S, L, N),
                                                                        interaction_x = 0,
                                                                        interaction_y = 0,
  generate_interaction = function(p){
    I_matrix <- matrix(0, 10, 10)                                                                      
    for(i in 1:10){                                                                      
      for(j in 1:10){                                                                    
          if(i == j){
            I_matrix[i,j] = 1
          }
        }
      }
      if(p < 0){
        print("Error. p must be greater than 0")
      }
      else if(p == 0){
        self$Interaction_matrix = I_matrix
        self$plot_interaction()
        return(self$Interaction_matrix)
      }
      else if(p < 1){
        p = p * 100
        i = sample(1:100, p, replace = FALSE)
        for(l in i:length(i)){
          I_matrix[i[l]] = 1
        }
      }
      else{
          I_matrix <- matrix(1,10,10)
      }
      self$Interaction_matrix = I_matrix
      self$plot_interaction()
      return(self$Interaction_matrix)
    },
    plot_interaction = function(){
      Plot.matrix(self$Interaction_matrix)
    },
    set_coordinates = function(x,y){
      self$interaction_x = x
      self$interaction_y = y
    },
    get_coordinates = function(a){
      if(a == 'x'){
        return(self$interaction_x)
      }
      else{
        return(self$interaction_y)
      }
    }
))

Stage <- R6Class("Stage", private = list(), public = list(species_ID = 0,
                                                          ID = 0,
                                                          reproduce = 0,
                                                          dependencies = rep(list(c(0)), 50),
                                                          active = 0,
  set_ID = function(ID){
    self$ID <- ID
  },
  
  get_ID = function(ID){
    return(self$ID)
  },
  
  set_species_ID = function(ID){
    self$species_ID <- ID
  },
  
  activate_stage = function(){
    self$active <- 1
  },
  
  deactivate_stage = function(){
    self$active <- 0
  },
  
  is_active = function(){
    if(self$active == 1){
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  },
  
  activate_reproduction = function(){
    self$reproduce <- 1
  },
  
  deactivate_reproduction = function(){
    self$reproduce <- 0
  },
  
  can_reproduce = function(){
    if(self$reproduce == 1){
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  },
  #test code
  add_dependency = function(prey_ID, prey_stage){
    if(self$dependencies[[prey_ID]] == 0){
      self$dependencies[[prey_ID]] <- c(prey_stage)
    }
    else{
      self$dependencies[[prey_ID]][length(self$dependencies[[prey_ID]]) + 1] <- prey_stage
    }
  }
))


Structured_Species <- R6Class("Structured_Species", lock_objects = FALSE,lock_class = FALSE, private = list(), public = list(ID = 0,
                                                                                    stage_list = rep(list(0), 10),
                                                                                    reproductive_stage = generate_reproductive_stage(),
                                                                                    active = 0,
  set_ID = function(ID){
    self$ID <- ID
  },

  get_ID = function(){
    return(self$ID)
  },

  empty_stages = function(){
    for(i in self$stage_list){
      i$deactivate$stage()
    }
  },

  fill_stages = function(){
    for(i in self$stage_list){
      i$activate_stage()
    }
  },
  
  insert_stage = function(stage){
    id <- stage$get_ID()
    self$stage_list[[id]] <- stage$clone(deep = TRUE)
  },
  
  remove_stage = function(stage_ID){
    self$stage_list[[stage_ID]] <- 0
  },
  
  activate_stage = function(stage_ID){
    self$stage_list[[stage_ID]]$activate_stage()
  },
  
  deactivate_stage = function(stage_ID){
    self$stage_list[[stage_ID]]$deactivate_stage()
  },
  
  stage_present = function(stage_ID){
    if(class(self$stage_list[[stage_ID]]) == 'numeric'){
      return(FALSE)
    }
    else{
      if(self$stage_list[[stage_ID]]$is_active()){
        return(TRUE)
      }
      else{
        return(FALSE)
      }
    }
  },
  add_dependency = function(prey_ID, predator_stage, prey_stage){
    self$stage_list[[predator_stage]]$add_dependency(prey_ID, prey_stage)
  },
  
  check_active = function(){
    count = 0
    for(i in self$stage_list){
      if(i$is_active()){
        count = count + 1
      }
    }
    if(count >= 1){
      self$active <- 1
      return(TRUE)
    }
    else{
      self$active <- 0
      return(FALSE)
    }
  },
  
  activate_species = function(){
    for(i in 1:10){
      self$stage_list[[i]]$activate_stage()
    }
  },
  
  deactivate_species = function(){
    for(i in 1:10){
      self$stage_list[[i]]$deactivate_stage()
    }
  }
))

#test
Structured_Ecosystem <- R6Class("Structured_Ecosystem", lock_objects = FALSE,lock_class = FALSE, private = list(), public = list(ID = 0,
                                                                                        S = 50,
                                                                                        C = 0.05,
                                                                                        N = 1,
                                                                                        L = S^2*C,
                                                                                        Species_web = matrix(0, 50, 50),
                                                                                        species_list = rep(list(0), 50),
                                                                                        interactions_list = list(),
  set_ID = function(ID){
    self$ID <- ID
  },

  get_ID = function(){
    return(self$ID)
  },
                                                                    
  custom_web = function(custS, custC, custN){
    custL = custS^2*custC
    web = Cascade.model(custS,custL,custN)
    self$Species_web <- web
  },

  get_web = function(){
    return(self$Species_web)
  },

  import_web = function(web){
    self$Species_web <- web
  },
  
  import_species = function(species){
    self$species_list <- species
  },
  
  import_interactions = function(interactions){
    self$interactions_list <- interactions
  },

  generate_interactions_newWeb = function(p){
    self$Species_web <- Cascade.model(self$S, self$L, self$N)
    temp_list = list()
    for(i in 1:50){
      for(j in 1:50){
        if(self$Species_web[i,j] == 1){
          temp_interaction = Interactions$new()
          temp_interaction$generate_interaction(p)
          temp_interaction$set_coordinates(i,j)
          temp_list[[length(temp_list) + 1]] <- temp_interaction
        }
      }
    }
    self$interactions_list <- temp_list
  },
  generate_interactions = function(p){
    temp_list = list()
    for(i in 1:50){
      for(j in 1:50){
        if(self$Species_web[i,j] == 1){
          temp_interaction = Interactions$new()
          temp_interaction$generate_interaction(p)
          temp_interaction$set_coordinates(i,j)
          temp_list[[length(temp_list) + 1]] <- temp_interaction
        }
      }
    }
    self$interactions_list <- temp_list
  },
  
  record_parameters = function(){
    for(i in 1:50){
      tempSpecies <- Structured_Species$new()
      tempSpecies$set_ID(i)
      repr_stage <- tempSpecies$reproductive_stage
      for(j in 1:10){
        tempStage <- Stage$new()
        tempStage$set_ID(j)
        if(j >= repr_stage){
          tempStage$activate_reproduction()
        }
        tempSpecies$insert_stage(tempStage)
      }
      self$add_species(tempSpecies)
    }
    for(i in 1:50){
      for(j in 1:50){
        if(self$Species_web[j,i] == 1){
          predator <- i
          prey <- j
          for(k in self$interactions_list){
            if(k$get_coordinates('x') == j){
              if(k$get_coordinates('y') == i){
                tempInteractionMatrix <- k$Interaction_matrix
                for(p in 1:10){
                  for(q in 1:10){
                    if(tempInteractionMatrix[q,p] == 1){
                      self$species_list[[predator]]$add_dependency(prey, p, q)
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  },
  
  get_population = function(){
    pop = 0
    for(i in 1:50){
      if(self$species_list[[i]]$check_active()){
        pop = pop + 1
      }
    }
    return(pop)
  },
  
  get_population_IDs = function(){
    ret = c()
    for(i in 1:50){
      if(self$species_list[[i]]$check_active()){
        ret[length(ret) + 1] <- i
      }
    }
    return(ret)
  },
  determine_stage_survival = function(species_ID, stage_ID){
    dependency_count = 0
    dependencies = list()
    temp_stage <- self$species_list[[species_ID]]$stage_list[[stage_ID]]
    temp_list <- temp_stage$dependencies
    for(i in 1:length(temp_list)){
      if(temp_list[[i]][1] == 0){
        next
      }
      else{
        if(length(temp_list[[i]] > 1)){
          prey <- c(i)
          for(j in 1:length(temp_list[[i]])){
            prey[length(prey) + 1] <- temp_list[[i]][j] 
          }
          dependencies[[length(dependencies) + 1]] <- prey
        }
        else{
          prey <- c(i, temp_list[[i]][1])
        }
      }
    }
    if(length(dependencies) == 0){
      dependency_count = 0
    }
    else{
      for(i in 1:length(dependencies)){
        dependency_count = dependency_count + length(dependencies[[i]])
      }
      if(dependency_count == 0){
        return(1)
      }
      else{
        count = 0
        for(i in 1:length(dependencies)){
          if(count > 0){
            return(1)
          }
          else{
            if(length(dependencies[[i]] > 2)){
              spID <- dependencies[[i]][1]
              for(j in 2:length(dependencies[[i]])){
                stID <- dependencies[[i]][j]
                if(self$species_list[[spID]]$stage_list[[stID]]$is_active()){
                  count = count + 1
                }
              }
            }
            if(count > 0){
              return(1)
            }
            else{
              return(0)
            }
          }
        }
      }
    }
  },
  determine_species_survival = function(species_ID){
    temp_spec <- self$species_list[[species_ID]]
    stats <- c()
    for(i in 1:10){
      stats[length(stats) + 1] <- self$determine_stage_survival(species_ID, i)
    }
    reproductive_age <- temp_spec$reproductive_stage
    survive = 1
    if(reproductive_age > 1){
      for(i in 1:reproductive_age){
        if(stats[i] == 0){
          survive = 0
          break
        }
      }
    }
    else{
      if(stats[1] == 0){
        survive = 0
      }
      else{
        survive = 1
      }
     }
    if(survive == 0){
      return(c(species_ID, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
    }
    else{
      ret = c(species_ID)
      for(i in 1:length(stats)){
        if(stats[i] == 0){
          ret[length(ret) + 1] <- i
        }
        if(length(stats) == 1){
          stats[1] <- 0
        }
      }
      return(ret)
    }
    ret <- c(species_ID)
    for(i in 1:10){
      if(stats[i] == 0){
        ret[length(ret) + 1] <- i
      }
    }
    return(ret)
  },
  
  deactivate_stages_list = function(stages){
    if(length(stages) == 1){
        #do nothing
        #self$species_list[[stages[1]]]$deactivate_species()
      }
    else{
      for(i in 2:length(stages)){
        self$species_list[[stages[1]]]$stage_list[[stages[i]]]$deactivate_stage()
      }
    }
  },
  get_population_full = function(){
    count = 0
    for(i in 1:50){
      for(j in 1:10){
        if(self$species_list[[i]]$stage_list[[j]]$is_active()){
          count = count + 1
        }
      }
    }
    return(count)
  },
  simulate_secondary_extinctions = function(){
    if(self$get_population_full() == 500){
      #do nothing
    }
    else{
      deactivation_list = list()
      for(i in 1:length(self$species_list)){
        deactivation_list[[length(deactivation_list) + 1]] <- self$determine_species_survival(i)
      }
      if(length(deactivation_list) >= 1){
        for(i in 1:length(deactivation_list)){
          self$deactivate_stages_list(deactivation_list[[i]])
        }
      }
    }
  },
  
  calculate_robustness = function(){
    if(self$get_population() < 2){
      return(0)
    }
    tempEco <- self$clone(deep = TRUE)
    attempts = 0
    initial_pop = self$get_population()
    target_pop = initial_pop/2
    calculate_target = function(pop, target){
      if((target %% 2) == 0){
        if(pop > target){
          return(TRUE)
        }
        else return(FALSE)
      }
      else{
        target = target + 0.5
        if(pop > target){
          return(TRUE)
        }
        else return(FALSE)
      }
    }
    count = 0
    while(calculate_target(tempEco$get_population(), target_pop)){
      ListOfSpecies <- sample(tempEco$get_population_IDs())
      tempEco$deactivate_species(ListOfSpecies[1])
      count = count + 1
      tempEco$simulate_secondary_extinctions()
    }
    ret = count/initial_pop
    return(ret)
  },
  
  transition_stages = function(){
    present <- self$get_population_IDs()
    for(i in 1:length(present)){
      self$species_list[[present[i]]]$fill_stages()
    }
    self$simulate_secondary_extinctions()
    for(i in 1:50){
      cutoff = 0
      for(j in 1:10){
        if(self$species_list[[i]]$stage_list[[j]]$is_active()){
          next
        }
        else{
          cutoff = j
          break
        }
      }
      if(cutoff >0){
        for(k in cutoff:10){
          self$species_list[[i]]$stage_list[[k]]$deactivate_stage()
        }
      }
    }
    self$simulate_secondary_extinctions
  },
  
  add_species = function(species){
    ID = species$get_ID()
    self$species_list[[ID]] <- species$clone(deep = TRUE)
  },

  remove_species = function(species_ID){
    self$species_list[[species_ID]] <- 0
  },

  insert_stage = function(species_ID, stage){
    self$species_list[[species_ID]]$insert_stage(stage)
  },

  remove_stage = function(species_ID, stage_ID){
    self$species_list[[species_ID]]$remove_stage(stage_ID)
  },

  activate_species = function(species_ID){
    self$species_list[[species_ID]]$activate_species()
  },

  deactivate_species = function(species_ID){
    self$species_list[[species_ID]]$deactivate_species()
  }
  ))

test <- function(){
  x <- Structured_Ecosystem$new()
  x$custom_web(50, 0.05, 1)
  x$generate_interactions(0.5)
  x$record_parameters()
  return(x)
}

migrate_step_structured = function(M, I){
  species = sample(1:50, 15, replace = FALSE)
  stages = sample(1:10, 15, replace = TRUE)
  for(i in 1:15){
    I$activate_species(species[i])
    I$species_list[[species[i]]]$activate_stage(stages[i])
  }
  I$transition_stages()
}

migrate_structured_getpop = function(M, I, time){
  pop_record = c()
  I$import_web(M$Species_web)
  I$import_interactions(M$interactions_list)
  I$record_parameters()
  for(i in 1:50){
    I$deactivate_species(i)
  }
  for(i in 1:time){
    migrate_step_structured(M, I)
    pop_record[length(pop_record) + 1] <- I$get_population()
  }
  return(pop_record)
}

Mainland <- Structured_Ecosystem$new()
Mainland$set_ID(1)
Mainland$custom_web(50, 0.1, 1)
Mainland$generate_interactions(0.5)
Mainland$record_parameters()
for(i in 1:50){
  Mainland$activate_species(i)
}
Mainland$get_population()
Mainland$deactivate_species(50)
Mainland$simulate_secondary_extinctions()
Mainland$get_population_full()
