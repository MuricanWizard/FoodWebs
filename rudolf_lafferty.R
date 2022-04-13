# For 09/15/20
# Perform rowsum checks to make sure every row has at least 1 1 in the stage_list
# Build migration model


## maje some random, cascade, and niche food webs
S = 10     ## set species richness
C = 0.3   ## set connectance
N = 1     ## set the number of replicate webs to make
L = S^2*C  ## calculate number of links from S and C


xxx <- Niche.model(S, L, N)
Plot.matrix(xxx)
box()
#create grid
abline(v = 1:S, lwd = 0.15) #create vertical lines
abline(h = 1:S, lwd = 0.15) #create horizontal lines

num_resources <- colSums(xxx) #number of species in each column

num_stages <- rep(NA,S)
for (i in 1:S){
  if (num_resources[i] != 0){
    num_stages[i] <- sample(1:num_resources[i], 1) #set a number of random stages for each species (1 to S)
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

check <- rep(NA, S)
### "we assured that each stage consumed at least one resource"
### need to implement this to make sure all stages have at least on resource. this checks it...
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
#redundant?
check <- rep(NA, S)
for (i in 1:S){
  if (!is.na(stage_list[[i]][1])){
    check[i] <- ncol(stage_list[[i]]) - sum(colSums(stage_list[[i]]) > 0)   
    }
}
check

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
