source('FoodWebFunctions.R')
source('Robustness.R')
source('generateStageMatrix.R')
source('Migration_Support_Functions.R')
library(R6)


# DON'T FORGET TO USE PARENTHESES WHEN INSTANTIATING
generate_reproductive_stage = function(){
  choices <- c(1,1,2,2,2,2,3,3,3,3,3,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,6,6,6,6,6,6,6,6,6,7,7,7,8,8,9,10)
  i = sample(1:length(choices), 1, replace = FALSE)
  rs <- choices[i[1]]
  return(rs)
}

Structured_Species <- R6Class("Structured_Species", private = list(), public = list(ID = 0, stage_list = c(1,1,1,1,1,1,1,1,1,1), stage_dependencies = list(),
                                                                                    reproductive_stage = generate_reproductive_stage(),
                                                                                    
  set_ID = function(ID){
    self$ID = ID
  },
                                                                                    
  get_ID = function(){
    return(self$ID) 
  },                                                                                 
  
  add_stage = function(stage_ID){
    self$stage_list[stage_ID] = 1
  },
  
  remove_stage = function(stage_ID){
    self$stage_list[stage_ID] = NULL
  },
  
  set_reproductive_stage = function(stage){
    self$reproductive_stage = stage
  },
  
  get_stage_population = function(){
    count = 0
    for(i in 1:length(self$stage_list)){
      if(self$stage_list[i] == 1){
        count = count + 1
      }
    }
    return(count)
  },
  
  add_dependency = function(ID, predator_stage, prey_stage){
    self$stage_dependencies[[length(self$stage_dependencies) + 1]] = c(ID, predator_stage, prey_stage)
  },
  
  remove_dependency = function(ID, predator_stage, prey_stage){
    for(i in 1:length(self$stage_dependencies)){
      if(self$stage_dependencies[[i]][1] == ID){
        if(self$stage_dependencies[[i]][2] == predator_stage){
          if(self$stage_dependencies[[i]][3] == prey_stage){
            self$stage_dependencies[[i]] = NULL
            i = length(self$stage_dependencies)
          }
        }
      }
    }
  },
  
  fill_stages = function(){
    self$stage_list <- c(1,1,1,1,1,1,1,1,1,1)
  }
  ))

Interactions <- R6Class("Interactions", private = list(), public = list(S = 5, C = 0.05, N = 1, L = S^2*C, Interaction_matrix = Random.model(S, L, N),
                                                                        interaction_x = 0, interaction_y = 0,
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
    else if(p <= 0.1){
      p = p * 100
      i = sample(1:100, p, replace = FALSE)
      for(l in i:length(i)){
        I_matrix[i[l]] = 1
      }
    }
    else if(p > 1){
      print("Error. p must be lesser than 1")
    }
    else{
      p = p * 100
      i = sample(1:100, p, replace = FALSE)
      for(k in 1:length(i)){
        I_matrix[i[k]] = 1
      }
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

Structured_Ecosystem <- R6Class("Structured_Ecosystem",lock_objects = FALSE,lock_class = FALSE,  private = list(), public = list(S = 50, C = 0.05, N = 1, L = S^2*C,
  Species_web = Cascade.model(S, L, N), species_list = as.list(rep(0, 50)), interactions_list = list(),
  
  generate_interactions = function(p){
    self_Species_web = Cascade.model(self$S, self$L, self$N)
    temp_list = list()
    for(i in 1:50){
      for(j in 1:50){
        if(self$Species_web[i,j] == 1){
          temp_interaction = Interactions$new()
          temp_interaction$generate_interaction(p)
          temp_interaction$set_coordinates(i,j)
          temp_list[[length(temp_list) + 1]] = temp_interaction
        }
      }
    }
    self$interactions_list = temp_list
  },
  
  generate_interactions_customFWstats = function(p, S, C, N){
    L = S^2*C
    self_Species_web = Cascade.model(S, L, N)
    temp_list = list()
    for(i in 1:50){
      for(j in 1:50){
        if(self$Species_web[i,j] == 1){
          temp_interaction = Interactions$new()
          temp_interaction$generate_interaction(p)
          temp_interaction$set_coordinates(i,j)
          temp_list[[length(temp_list) + 1]] = temp_interaction
        }
      }
    }
    self$interactions_list = temp_list
  },
  
  plot_Ecosystem = function(){
    Plot.matrix(self$Species_web)
  },
  
  get_interaction = function(x,y){
    for(i in 1:length(self$interactions_list)){
      if(x == self$interactions_list[[i]]$get_coordinates('x')){
        if(y == self$interactions_list[[i]]$get_coordinates('y')){
          self$interactions_list[[i]]$plot_interaction()
          return(self$interactions_list[[i]]$Interaction_matrix)
        }
      }
    }
  },
  
  import_species_interactions = function(food_matrix){
    self$Species_web = food_matrix
  },
  
  import_stage_interactions = function(interaction_matrix){
    self$interactions_list = interaction_matrix
  },
  
  export_species_interactions = function(){
    return(self$Species_web)
  },
  
  export_stage_interactions = function(){
    return(self$interaction_matrix)
  },
  
  add_species = function(species){
    ID = species$get_ID()
    self$species_list[[ID]] = species
  },
  
  add_stage = function(species_ID, stage_ID){
    self$species_list[[species_ID]]$add_stage(stage_ID)
  },
  
  remove_species = function(species_ID){
    self$species_list[[species_ID]] = 0
  },
  
  remove_stage = function(species_ID, stage_ID){
    self$species_list[[species_ID]]$remove_stage(stage_ID)
  },
  
  record_species = function(){
    interaction_count = 0
    eco = self$Species_web
    for(i in 1:50){
      temp_spec = Structured_Species$new()
      temp_spec$set_ID(i)
      for(m in 1:10){
        temp_spec$add_stage(m)
      }
      for(j in 1:50){
        if(eco[j,i] == 1){
          interaction_count = interaction_count + 1
          int_matrix = (self$interactions_list[[interaction_count]])$Interaction_matrix
          temp_stage_depen = c()
          for(k in 1:10){
            temp_stage_depen[1] = j
            for(l in 1:10){
              if(int_matrix[l,k] == 1){
                temp_stage_depen[2] = k
                temp_stage_depen[3] = l
                temp_spec$add_dependency(j, k, l)
              }
            }
          }
        }
      }
      self$species_list[[i]] = temp_spec
    }
  },
    
  get_population = function(){
    count = 50
    for(i in 1:length(self$species_list)){
      if(class(self$species_list[[i]]) == "numeric"){
        count = count - 1
      }
    }
    return(count)
  },
  
  exists = function(species_ID, stage_ID){
    ret = FALSE
    if(class(self$species_list[[species_ID]]) != "numeric"){
      if(self$species_list[[species_ID]]$stage_list[stage_ID] == 1){
        ret = TRUE
      }
    }
    return(ret)
  },
  
  determine_survival = function(species_ID, stage_ID){
    ret = 0
    temp_spec = self$species_list[[species_ID]]$clone()
    dependencies = list()
   if(length(temp_spec$stage_dependencies) > 0){
     for(i in 1:length(temp_spec$stage_dependencies)){
      if(temp_spec$stage_dependencies[[i]][2] == stage_ID){
        dependencies[[length(dependencies) + 1]] = c(temp_spec$stage_dependencies[[i]][1], temp_spec$stage_dependencies[[i]][3])
      }
    }
  }
    if(length(dependencies) == 0){
      ret = 1
    }
    else{
      count = 0
      for(i in 1:length(dependencies)){
        if(self$exists(dependencies[[i]][1], dependencies[[i]][2]) == 1){
          count = count + 1
        }
      }
      if(count > 0){
        ret = 1
      }
    }
    return(ret)
  },
  
  simulate_secondary_extinctions = function(){
    removals = list()
    eco = self$Species_web
    predator_species_ID = 0
    predator_stage_ID = 0
    prey_species_ID = 0
    prey_stage_ID = 0
    for(i in 1:50){
      if(class(self$species_list[[i]])[1] == "Structured_Species"){
        for(j in 1:10){
          if(self$species_list[[i]]$stage_list[j] == 1){
            if(self$determine_survival(i, j) == 0){
              self$species_list[[i]]$stage_list[j] = 0
            }
          }
        }
        count = 0
        for(k in 1:10){
          if(self$species_list[[i]]$stage_list[k] == 1){
            count = count + 1
          }
        }
        if(count == 0){
          self$species_list[[i]] = 0
        }
      }
    }
  },
  get_population_IDs = function(){
    count = c()
    for(i in 1:length(self$species_list)){
      if(class(self$species_list[[i]]) == "Structured_Species"){
        count[length(count) + 1] = self$species_list[[i]]$get_ID()
      }
    }
    return(count)
  },
  
  calculate_robustness = function(){
    if(self$get_population() < 2){
      return(0)
    }
    else{
      attempts = 0
      tempEco = self$clone()
      initial = self$get_population()
      target = initial/2
      existing_IDs = tempEco$get_population_IDs()
      ListOfSpecies <- sample(existing_IDs)
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
      index = 1
      count = 0
      while(calculate_target(tempEco$get_population(), target)){
        tempEco$remove_species(ListOfSpecies[index])
        tempEco$simulate_secondary_extinctions()
        count = count + 1
        index = index + 1
       }
      ret = count/initial
      return(ret)
      }
  },
  
  transition_stages = function(){
    for(i in 1:length(self$species_list)){
      if(class(self$species_list[[i]]) == "numeric"){
        next
      }
      self$species_list[[i]]$fill_stages()
    }
    self$simulate_secondary_extinctions()
    for(j in 1:length(self$species_list)){
      if(class(self$species_list[[j]]) == "numeric"){
        next
      }
      cutoff = 0
      for(k in 1:10){
        if(self$species_list[[j]]$stage_list[k] == 0){
          cutoff = k
          break
        }
      }
      if(cutoff > 0){
        if(cutoff <= self$species_list[[j]]$reproductive_stage){
          self$species_list[[j]] <- 0
          self$simulate_secondary_extinctions()
        }
        else{
          for(p in cutoff:10){
            self$species_list[[j]]$stage_list[p] <- 0
          }
          self$simulate_secondary_extinctions()
        }
      }
    }
  }
))

migrate_step_structured = function(M, I){
  species = sample(1:50, 5, replace = FALSE)
  stages = sample(1:10, 5, replace = TRUE)
  for(i in 1:5){
    I$species_list[[species[i]]] <- M$species_list[[species[i]]]
    I$species_list[[species[i]]]$stage_list = c(0,0,0,0,0,0,0,0,0,0)
    for(j in 1:stages[i]){
      I$add_stage(species[i], j)
    }
  }
  I$simulate_secondary_extinctions()
  I$transition_stages()
}

migrate_structured = function(M, I, time){
  I$import_species_interactions(M$export_species_interactions())
  I$import_stage_interactions(M$export_stage_interactions())
  pop = c()
  for(i in 1:time){
    migrate_step_structured(M, I)
    pop[length(pop) + 1] = I$get_population()
  }
  return(pop)
}

calculate_average_pop = function(pop_list){
  temp <- list()
  for(i in 1:30){
    t <- c()
    for(j in 1:30){
      t[length(t) + 1] = pop_list[[j]][i]
    }
    temp[[length(temp) + 1]] <- t
  }
  ret = c()
  for(k in 1:30){
    ret[length(ret) + 1] <- mean(temp[[k]])
  }
  return(ret)
}

plot_migration_pop = function(c, p1, p2, p3, p4, p5){
  pop1 = list()
  for(i in 1:30){
    M1 = Structured_Ecosystem$new()
    M1$generate_interactions_customFWstats(p1, 50, c, 1)
    M1$record_species()
    I1 = Structured_Ecosystem$new()
    pop1[[i]] <- migrate_structured(M1, I1, 30) 
  }
  Population <- calculate_average_pop(pop1)
  
  pop2 = list()
  for(i in 1:30){
    M2 = Structured_Ecosystem$new()
    M2$generate_interactions_customFWstats(p2, 50, c, 1)
    M2$record_species()
    I2 = Structured_Ecosystem$new()
    pop2[[i]] <- migrate_structured(M2, I2, 30)
  }
  p2 <- calculate_average_pop(pop2)
      
  pop3 = list()
  for(i in 1:30){
    M3 = Structured_Ecosystem$new()
    M3$generate_interactions_customFWstats(p3, 50, c, 1)
    M3$record_species()
    I3 = Structured_Ecosystem$new()
    pop3[[i]] <- migrate_structured(M3, I3, 30)
  }
  p3 <- calculate_average_pop(pop3)
  
  pop4 = list()
  for(i in 1:30){
    M4 = Structured_Ecosystem$new()
    M4$generate_interactions_customFWstats(p4, 50, c, 1)
    M4$record_species()
    I4 = Structured_Ecosystem$new()
    pop4[[i]] <- migrate_structured(M4, I4, 30)
  }
  p4 <- calculate_average_pop(pop4)
  
  pop5 = list()
  for(i in 1:30){
    M5 = Structured_Ecosystem$new()
    M5$generate_interactions_customFWstats(p5, 50, c, 1)
    M5$record_species()
    I5 = Structured_Ecosystem$new()
    pop5[[i]] <- migrate_structured(M5, I5, 30)
  }
  p5 <- calculate_average_pop(pop5)
  
  Time <- c(1:30)
  # Graph cars using blue points overlayed by a line 
  plot(Time, Population, type="o", col="blue")
  t <- c(0.1,0.4,0.2,0.1,0.2)
  lines(p2, type = "o", col = "yellow")
  lines(p3, type = "o", col = "green")
  lines(p4, type = "o", col = "black")
  lines(p5, type = "o", col = "red")
  # Create a title with a red, bold/italic font
  title(main="Population/Time", col.main="red", font.main=4)
}

plot_migration_rob = function(c, p1, p2, p3, p4, p5){
  rob1 = list()
  for(i in 1:30){
    M1 = Structured_Ecosystem$new()
    M1$generate_interactions_customFWstats(p1, 50, c, 1)
    M1$record_species()
    I1 = Structured_Ecosystem$new()
    period_rob = c()
    for(j in 1:30){
      migrate_step_structured(M1, I1) 
      period_rob[j] <- I1$calculate_robustness()
    }
    rob1[[i]] <- period_rob
  }
  Robustness <- calculate_average_pop(rob1)
  
  rob2 = list()
  for(i in 1:30){
    M2 = Structured_Ecosystem$new()
    M2$generate_interactions_customFWstats(p2, 50, c, 1)
    M2$record_species()
    I2 = Structured_Ecosystem$new()
    period_rob2 = c()
    for(j in 1:30){
      migrate_step_structured(M2, I2) 
      period_rob2[j] <- I2$calculate_robustness()
    }
    rob2[[i]] <- period_rob2
  }
  p2 <- calculate_average_pop(rob2)
  
  rob3 = list()
  for(i in 1:30){
    M3 = Structured_Ecosystem$new()
    M3$generate_interactions_customFWstats(p3, 50, c, 1)
    M3$record_species()
    I3 = Structured_Ecosystem$new()
    period_rob3 <- c()
    for(j in 1:30){
      migrate_step_structured(M3, I3) 
      period_rob3[j] <- I3$calculate_robustness()
    }
    rob3[[i]] <- period_rob3
  }
  p3 <- calculate_average_pop(rob3)
  
  rob4 = list()
  for(i in 1:30){
    M4 = Structured_Ecosystem$new()
    M4$generate_interactions_customFWstats(p4, 50, c, 1)
    M4$record_species()
    I4 = Structured_Ecosystem$new()
    period_rob4 <- c()
    for(j in 1:30){
      migrate_step_structured(M4, I4) 
      period_rob4[j] <- I4$calculate_robustness()
    }
    rob4[[i]] <- period_rob4
  }
  p4 <- calculate_average_pop(rob4)
  
  rob5 = list()
  for(i in 1:30){
    M5 = Structured_Ecosystem$new()
    M5$generate_interactions_customFWstats(p5, 50, c, 1)
    M5$record_species()
    I5 = Structured_Ecosystem$new()
    period_rob5 <- c()
    for(j in 1:30){
      migrate_step_structured(M5, I5) 
      period_rob5[j] <- I5$calculate_robustness()
    }
    rob5[[i]] <- period_rob5
  }
  p5 <- calculate_average_pop(rob5)
  
  Time <- c(1:30)
  # Graph cars using blue points overlayed by a line 
  plot(Time, Robustness, type="o", col="blue")
  t <- c(0.1,0.4,0.2,0.1,0.2)
  lines(p2, type = "o", col = "yellow")
  lines(p3, type = "o", col = "green")
  lines(p4, type = "o", col = "black")
  lines(p5, type = "o", col = "red")
  # Create a title with a red, bold/italic font
  title(main="Robustness/Time", col.main="red", font.main=4)
}

POP1 = list()
POP2 = list()
POP3 = list()
POP4 = list()
POP5 = list()

ROB1 = list()
ROB2 = list()
ROB3 = list()
ROB4 = list()
ROB5 = list()

calculate_migration_pop = function(c, p1, p2, p3, p4, p5){
  pop1 = list()
  for(i in 1:30){
    M1 = Structured_Ecosystem$new()
    M1$generate_interactions_customFWstats(p1, 50, c, 1)
    M1$record_species()
    I1 = Structured_Ecosystem$new()
    pop1[[i]] <- migrate_structured(M1, I1, 30) 
  }
  POP1[[length(POP1) + 1]] <<- calculate_average_pop(pop1)
  
  pop2 = list()
  for(i in 1:30){
    M2 = Structured_Ecosystem$new()
    M2$generate_interactions_customFWstats(p2, 50, c, 1)
    M2$record_species()
    I2 = Structured_Ecosystem$new()
    pop2[[i]] <- migrate_structured(M2, I2, 30)
  }
  POP2[[length(POP2) + 1]] <<- calculate_average_pop(pop2)
  
  pop3 = list()
  for(i in 1:30){
    M3 = Structured_Ecosystem$new()
    M3$generate_interactions_customFWstats(p3, 50, c, 1)
    M3$record_species()
    I3 = Structured_Ecosystem$new()
    pop3[[i]] <- migrate_structured(M3, I3, 30)
  }
  POP3[[length(POP3) + 1]] <<- calculate_average_pop(pop3)
  
  pop4 = list()
  for(i in 1:30){
    M4 = Structured_Ecosystem$new()
    M4$generate_interactions_customFWstats(p4, 50, c, 1)
    M4$record_species()
    I4 = Structured_Ecosystem$new()
    pop4[[i]] <- migrate_structured(M4, I4, 30)
  }
  POP4[[length(POP4) + 1]] <<- calculate_average_pop(pop4)
  
  pop5 = list()
  for(i in 1:30){
    M5 = Structured_Ecosystem$new()
    M5$generate_interactions_customFWstats(p5, 50, c, 1)
    M5$record_species()
    I5 = Structured_Ecosystem$new()
    pop5[[i]] <- migrate_structured(M5, I5, 30)
  }
  POP5[[length(POP5) + 1]] <<- calculate_average_pop(pop5)
}

calculate_migration_rob = function(c, p1, p2, p3, p4, p5){
  rob1 = list()
  for(i in 1:30){
    M1 = Structured_Ecosystem$new()
    M1$generate_interactions_customFWstats(p1, 50, c, 1)
    M1$record_species()
    I1 = Structured_Ecosystem$new()
    period_rob = c()
    for(j in 1:30){
      migrate_step_structured(M1, I1) 
      period_rob[j] <- I1$calculate_robustness()
    }
    rob1[[i]] <- period_rob
  }
  ROB1[[length(ROB1) + 1]] <<- calculate_average_pop(rob1)
  
  rob2 = list()
  for(i in 1:30){
    M2 = Structured_Ecosystem$new()
    M2$generate_interactions_customFWstats(p2, 50, c, 1)
    M2$record_species()
    I2 = Structured_Ecosystem$new()
    period_rob2 = c()
    for(j in 1:30){
      migrate_step_structured(M2, I2) 
      period_rob2[j] <- I2$calculate_robustness()
    }
    rob2[[i]] <- period_rob2
  }
  ROB2[[length(ROB2) + 1]] <<- calculate_average_pop(rob2)
  
  rob3 = list()
  for(i in 1:30){
    M3 = Structured_Ecosystem$new()
    M3$generate_interactions_customFWstats(p3, 50, c, 1)
    M3$record_species()
    I3 = Structured_Ecosystem$new()
    period_rob3 <- c()
    for(j in 1:30){
      migrate_step_structured(M3, I3) 
      period_rob3[j] <- I3$calculate_robustness()
    }
    rob3[[i]] <- period_rob3
  }
  ROB3[[length(ROB3) + 1]] <<- calculate_average_pop(rob3)
  
  rob4 = list()
  for(i in 1:30){
    M4 = Structured_Ecosystem$new()
    M4$generate_interactions_customFWstats(p4, 50, c, 1)
    M4$record_species()
    I4 = Structured_Ecosystem$new()
    period_rob4 <- c()
    for(j in 1:30){
      migrate_step_structured(M4, I4) 
      period_rob4[j] <- I4$calculate_robustness()
    }
    rob4[[i]] <- period_rob4
  }
  ROB4[[length(ROB4) + 1]] <<- calculate_average_pop(rob4)
  
  rob5 = list()
  for(i in 1:30){
    M5 = Structured_Ecosystem$new()
    M5$generate_interactions_customFWstats(p5, 50, c, 1)
    M5$record_species()
    I5 = Structured_Ecosystem$new()
    period_rob5 <- c()
    for(j in 1:30){
      migrate_step_structured(M5, I5) 
      period_rob5[j] <- I5$calculate_robustness()
    }
    rob5[[i]] <- period_rob5
  }
  ROB5[[length(ROB5) + 1]] <<- calculate_average_pop(rob5)
}

calculate_migration_pop_variable_connectance = function(c1, c2, c3, c4, c5, p1, p2, p3, p4, p5){
  calculate_migration_pop(c1, p1, p2, p3, p4, p5)
  calculate_migration_pop(c2, p1, p2, p3, p4, p5)
  calculate_migration_pop(c3, p1, p2, p3, p4, p5)
  calculate_migration_pop(c4, p1, p2, p3, p4, p5)
  calculate_migration_pop(c5, p1, p2, p3, p4, p5)
}

calculate_migration_rob_variable_connectance = function(c1, c2, c3, c4, c5, p1, p2, p3, p4, p5){
  calculate_migration_rob(c1, p1, p2, p3, p4, p5)
  calculate_migration_rob(c2, p1, p2, p3, p4, p5)
  calculate_migration_rob(c3, p1, p2, p3, p4, p5)
  calculate_migration_rob(c4, p1, p2, p3, p4, p5)
  calculate_migration_rob(c5, p1, p2, p3, p4, p5)
}

plot_pop_connectance = function(connectance_index){
  Time <- c(1:30)
  Population = POP1[[connectance_index]]
  # Graph cars using blue points overlayed by a line 
  plot(Time, Population, type="o", col="blue")
  t <- c(0.1,0.4,0.2,0.1,0.2)
  lines(POP2[[connectance_index]], type = "o", col = "yellow")
  lines(POP3[[connectance_index]], type = "o", col = "green")
  lines(POP4[[connectance_index]], type = "o", col = "black")
  lines(POP5[[connectance_index]], type = "o", col = "red")
  # Create a title with a red, bold/italic font
  title(main="Population/Time", col.main="red", font.main=4)
}

plot_rob_connectance = function(connectance_index){
  Time <- c(1:30)
  Robustness = ROB1[[connectance_index]]
  # Graph cars using blue points overlayed by a line 
  plot(Time, Robustness, type="o", col="blue")
  t <- c(0.1,0.4,0.2,0.1,0.2)
  lines(ROB2[[connectance_index]], type = "o", col = "yellow")
  lines(ROB3[[connectance_index]], type = "o", col = "green")
  lines(ROB4[[connectance_index]], type = "o", col = "black")
  lines(ROB5[[connectance_index]], type = "o", col = "red")
  # Create a title with a red, bold/italic font
  title(main="Robustness/Time", col.main="red", font.main=4)
}

calculate_comparative_connectance_pop = function(c, p){
  
}



inter = Interactions$new()
inter$generate_interaction(0.4)
Eco = Structured_Ecosystem$new()

#Eco$generate_interactions(0)
#Eco$plot_Ecosystem()
#Eco$interactions_list[[2]]$get_coordinates('x')
#Eco$interactions_list[[2]]$get_coordinates('y')
#Eco$get_interaction(1,32)
#Eco$record_species()
#Eco$get_population()
#Eco$species_list
#Eco$species_list[[1]]$get_stage_population()


#Eco$simulate_secondary_extinctions()
#Eco$get_population()

#Eco2 = Structured_Ecosystem$new()
#Eco2$add_species(Eco$species_list[[1]])
#pop_list <- migrate_structured(Eco, Eco2, 30)
#Eco2$species_list
#plot_migration_pop(0.05, 0, 0.2, 0.4, 0.7, 1)
#plot_migration_rob(0.05, 0, 0.2, 0.4, 0.7, 1)
#calculate_migration_pop_variable_connectance(0.01, 0.05, 0.1, 0.3, 0.4, 0, 0.2, 0.4, 0.7, 1)
#calculate_migration_rob_variable_connectance(0.01, 0.05, 0.1, 0.3, 0.4, 0, 0.2, 0.4, 0.7, 1)
