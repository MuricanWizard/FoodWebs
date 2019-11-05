source('FoodWebFunctions.R')
source('Robustness.R')
library(R6)

S = 50    ## set species richness
C = 0.05   ## set connectance
N = 1     ## set the number of replicate webs to make
L = S^2*C  ## calculate number of links from S and C

M<- Cascade.model(S, L, N) #Cascade foodweb model of the mainland
I <- matrix(0, 50, 50) #Empty Island
push <- function(l, x) {
       assign(l, append(eval(as.name(l)), x), envir=parent.frame())
}

expandingList <- function(capacity = 50) {
  buffer <- vector('list', capacity)
  length <- 0
  
  methods <- list()
  
  methods$double.size <- function() {
    buffer <<- c(buffer, vector('list', capacity))
    capacity <<- capacity * 2
  }
  
  methods$add <- function(val) {
    if(length == capacity) {
      methods$double.size()
    }
    
    length <<- length + 1
    buffer[[length]] <<- val
  }
  
  methods$as.list <- function() {
    b <- buffer[0:length]
    return(b)
  }
  
  methods
}
# DON'T FORGET TO PUT PARENTHESES WHEN INSTANTIATING

Species <- R6Class("Species", private = list(), public = list(ID = 0, stage = 0, set_stage = function(s){
  self$stage = s
}, depends_on = c(), set_ID = function(n){
  self$ID = n
}, get_ID = function(){
  return(self$ID)
}, add_dependencies = function(ID){
  self$depends_on[length(self$depends_on) + 1] <- ID
}, get_dependencies = function(){
  return(self$depends_on)
}))

Ecosystem2 <- R6Class("Ecosystem2", private = list(), public = list(poplulation = 0, Species_list = as.list(rep(0, 50)),
  
  add_species = function(s){
    self$Species_list[[s$get_ID()]] <- s
    self$get_population()
}, get_population = function(){
  count = 0
    for(i in 1:length(self$Species_list)){
      if(class(self$Species_list[[i]])[1] == "Species"){
        count = count + 1
      }
    }
    return(count) 
}, 
  get_population_IDs = function(){
    count = c()
    for(i in 1:length(self$Species_list)){
      if(class(self$Species_list[[i]])[1] == "Species"){
        count[length(count) + 1] = self$Species_list[[i]]$get_ID()
      }
    }
    return(count)
    
  }, remove_byID = function(ID){
    if(class(self$Species_list[[ID]]) == "numeric"){
      print("No such Species exists")
    }
    else{
      if(ID == (self$Species_list[[ID]])$get_ID()){
        self$Species_list[[ID]] = 0
        self$get_population()
      }
      else{
        print("Error")
      }
    }
}, simulate_secondary_extinctions = function(){
    removals = c()
    dependencies = c()
    for(i in 1:length(self$Species_list)){
      if(class(self$Species_list[[i]]) != "numeric"){
        dependencies = self$Species_list[[i]]$get_dependencies()
        count = length(dependencies)
        for(j in dependencies){
          if(class(self$Species_list[[j]]) == "numeric"){
            count = count - 1
          }
        }
      }
      if(count < 1 && length(dependencies != 0)){
        removals[length(removals) + 1] = i
      }
    }
    for(k in removals){
      self$remove_byID(k)
    }
},calculate_robustness = function(){
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
      tempEco$remove_byID(ListOfSpecies[index])
      tempEco$simulate_secondary_extinctions()
      count = count + 1
      index = index + 1
    }
    ret = count/initial
    return(ret)
  }
}))

migrate_get_robustnessandpop_over_time <- function(M, I){
  robustness_over_time <- c()
  population_over_time <- c()
  attempts = 0
  while(attempts < 30){
    robustness_over_time[length(robustness_over_time) + 1] = migrate_step_get_Robustness(M, I)
    population_over_time[length(population_over_time) + 1] = I$get_population()
    attempts = attempts + 1
  }
  ret = list(population_over_time, robustness_over_time)
  return(ret)
}

plot_robustness_over_time <- function(x){

}

average_stats_at_C <- function(C){
  pop_list <- list()
  rob_list <- list()
  #loop
  for(i in 1:30){
    pop <- c()
    rob <- c()
    S = 50    ## set species richness
    N = 1     ## set the number of replicate webs to make
    L = S^2*C
    tempMatrix <- Cascade.model(S, L, N)
    tempMatrix2 <- matrix(0, 50, 50)
    Plot.matrix(tempMatrix)
    tempMainland <- Ecosystem2$new()
    tempIsland <- Ecosystem2$new()
    add_species_to_Ecosystem(tempMatrix, tempMainland)
    temp_ret = migrate_get_robustnessandpop_over_time(tempMainland, tempIsland)
    pop <- temp_ret[[1]]
    rob <- temp_ret[[2]]
    pop_list[[length(pop_list) + 1]] = pop
    rob_list[[length(rob_list) + 1]] = rob
  }
    #end loop
  average_pop <- c()
  average_rob <- c()
  #average_calculation
  for(i in 1:30){
    sum = 0
    sum2 = 0
    for(j in 1:30){
      sum = sum + pop_list[[j]][[i]]
      sum2 = sum2 + rob_list[[j]][i]
    }
    average_pop[length(average_pop) + 1] = sum/30
    average_rob[length(average_rob) + 1] = sum2/30
  }
  ret <- list(average_pop, average_rob)
  return(ret)
}

generate_average_connectance_graph_pop <- function(c1, c2, c3, c4, c5){
  connec1 <- average_stats_at_C(c1)
  connec2 <- average_stats_at_C(c2)
  connec3 <- average_stats_at_C(c3)
  connec4 <- average_stats_at_C(c4)
  connec5 <- average_stats_at_C(c5)
  Time <- c(1:30)
  plot(Time, connec1[[1]], type="o", col="blue")
  t <- c(0.1,0.4,0.2,0.1,0.2)
  lines(connec2[[1]], type = "o", col = "red")
  lines(connec3[[1]], type = "o", col = "green")
  lines(connec4[[1]], type = "o", col = "purple")
  lines(connec5[[1]], type = "o", col = "yellow")
  # Create a title with a red, bold/italic font
  title(main="Average population/Time", col.main="red", font.main=4)
}

generate_average_connectance_graph_rob <- function(c1, c2, c3, c4, c5){
  connec1 <- average_stats_at_C(c1)
  connec2 <- average_stats_at_C(c2)
  connec3 <- average_stats_at_C(c3)
  connec4 <- average_stats_at_C(c4)
  connec5 <- average_stats_at_C(c5)
  Time <- c(1:30)
  Robustness <- connec1[[2]]
  plot(Time, Robustness, type="o", col="blue", ylim = c(-1, 0.6))
  t <- c(0.1,0.4,0.2,0.1,0.2)
  lines(connec2[[2]], type = "o", col = "red")
  lines(connec3[[2]], type = "o", col = "green")
  lines(connec4[[2]], type = "o", col = "purple")
  lines(connec5[[2]], type = "o", col = "yellow")
  # Create a title with a red, bold/italic font
  title(main="Average robustness/Time", col.main="red", font.main=4)
}

generate_timeToFifty_graph_connectance <- function(){
  
}

#Mainland = Ecosystem2$new()
#add_species_to_Ecosystem(M, Mainland)
#Mainland$get_population()
#Mainland$get_population_IDs()
#Mainland$calculate_robustness()
#Island = Ecosystem2$new()
#migrate_step_get_Robustness(Mainland, Island)
#X <- migrate_get_robustnessandpop_over_time(Mainland, Island)
#print(x)
#Plot.matrix(M)
#box()
#Population <- x[[1]]

#C = 0.1
S2 = 50    ## set species richness
C2 = 0.1   ## set connectance
N2 = 1     ## set the number of replicate webs to make
L2 = S2^2*C2  ## calculate number of links from S and C

#M2<- Cascade.model(S2, L2, N2) #Cascade foodweb model of the mainland

#Mainland2 = Ecosystem2$new()
#add_species_to_Ecosystem(M2, Mainland2)
#Mainland2$get_population()
#Mainland2$get_population_IDs()
#Mainland2$calculate_robustness()
#Island2 = Ecosystem2$new()
#migrate_step_get_Robustness(Mainland2, Island2)
#y <- migrate_get_robustnessandpop_over_time(Mainland2, Island2)
#print(y)
#Population2 <- y[[1]]

#C = 0.2
S3 = 50    ## set species richness
C3 = 0.2   ## set connectance
N3 = 1     ## set the number of replicate webs to make
L3 = S3^2*C3  ## calculate number of links from S and C

#M3<- Cascade.model(S3, L3, N3) #Cascade foodweb model of the mainland

#Mainland3 = Ecosystem2$new()
#add_species_to_Ecosystem(M3, Mainland3)
#Mainland3$get_population()
#Mainland3$get_population_IDs()
#Mainland3$calculate_robustness()
#Island3 = Ecosystem2$new()
#migrate_step_get_Robustness(Mainland3, Island3)
#z <- migrate_get_robustnessandpop_over_time(Mainland3, Island3)
#print(z)
#Population3 <- z[[1]]


#C = 0.3
S4 = 50    ## set species richness
C4 = 0.3   ## set connectance
N4 = 1     ## set the number of replicate webs to make
L4 = S4^2*C4  ## calculate number of links from S and C

#M4<- Cascade.model(S4, L4, N4) #Cascade foodweb model of the mainland

#Mainland4 = Ecosystem2$new()
#add_species_to_Ecosystem(M4, Mainland4)
#Mainland4$get_population()
#Mainland4$get_population_IDs()
#Mainland4$calculate_robustness()
#Island4 = Ecosystem2$new()
#migrate_step_get_Robustness(Mainland4, Island4)
#a <- migrate_get_robustnessandpop_over_time(Mainland4, Island4)
#print(a)
#Population4 <- a[[1]]

# Define the cars vector with 5 values
#Time <- c(1:30)
# Graph cars using blue points overlayed by a line 
#plot(Time, Population, type="o", col="blue")
#t <- c(0.1,0.4,0.2,0.1,0.2)
#lines(Population2, type = "o", col = "red")
#lines(Population3, type = "o", col = "green")
#lines(Population4, type = "o", col = "purple")
# Create a title with a red, bold/italic font
#title(main="Population/Time", col.main="red", font.main=4)
#legend(1, 95, legend=c("C = 0.05", "C = 0.1", "C = 0.2"),
#       col=c("blue", "red", "green"), lty=1:2, cex=0.8)
