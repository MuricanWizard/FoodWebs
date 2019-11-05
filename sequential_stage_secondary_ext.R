

## make some random, cascade, and niche food webs
S = 50    ## set species richness
C = 0.3   ## set connectance
N = 1     ## set the number of replicate webs to make
L = S^2*C  ## calculate number of links from S and C


xxx <- Cascade.model(S, L, N)
p_nicheoverlap <- 0.5

  num_resources <- colSums(xxx)
  
  num_stages <- rep(NA,S)
  for (i in 1:S){
    if (num_resources[i] != 0){
      num_stages[i] <- sample(1:num_resources[i], 1)
    }
  }
  
  # plot(sort(num_stages))
  max_stages <- max(num_stages, na.rm = TRUE)
  
  
  p <- p_nicheoverlap
  stage_list <- list()
  for (j in 1:S){
    if (num_resources[j] > 0){
      tmp_stage_mat <- rep(NA, num_stages[j] * num_resources[j])
      for (i in 1:length(tmp_stage_mat)){
        tmp_stage_mat[i] <- rbinom(1, 1, p)
      }
      
      stage_mat <- array(tmp_stage_mat, dim = c(num_resources[j], num_stages[j]))
      
      stage_list[[j]] <- stage_mat
    } else {
      stage_list[[j]] <- NA
    }
  }
  
  ### "we assured that each stage consumed at least one resource"
  ### need to implement this to make sure all stages have at least on resource. this checks it...
  check <- rep(NA, S)
  for (i in 1:length(stage_list)){
    if (!is.na(stage_list[[i]][1])){
      check[i] <- ncol(stage_list[[i]]) - sum(colSums(stage_list[[i]]) > 0)   
      if (check[i] > 0){
        empty_stages <- which(colSums(stage_list[[i]]) == 0)
        fillin_rows <- sample(1:nrow(stage_list[[i]]), length(empty_stages), replace = TRUE)
        for (j in 1:length(empty_stages)){
          stage_list[[i]][fillin_rows[j], empty_stages[j]] <- 1
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
secondary_extinction <- function(stage_web){
    
  S <- dim(stage_web)[1]  
  random_removal <- sample(S,1)
  
  tmp_rm_stage_matrix <- stage_web
  tmp_rm_stage_matrix[random_removal,,] <- 0  
  tmp_rm_stage_matrix[,random_removal,] <- 0  
  rm_stage_matrix <- tmp_rm_stage_matrix
  
  
  eating_by_stage <- array(dim = c(max_stages, S))
  rm_eating_by_stage <- array(dim = c(max_stages, S))
  
  for (i in 1:max_stages){
    eating_by_stage[i,] <- colSums(stage_web[,,i])
    rm_eating_by_stage[i,] <- colSums(rm_stage_matrix[,,i])
  }
  
  secondary <- (eating_by_stage > 0) - (rm_eating_by_stage > 0)
  
  tmp_secondary_id <- which(colSums(secondary) > 0)
  secondary_id <- tmp_secondary_id[tmp_secondary_id != random_removal]
  number_secondary_extinctions <- length(secondary_id)
  
  
  tmp_row_rm <- stage_web[-c(random_removal, secondary_id),,]
  tmp_col_rm <- tmp_row_rm[,-c(random_removal, secondary_id),]  
  resulting_stage_web <- tmp_col_rm
  
  ### function outputs
  output <- list(random_removal = random_removal,
                 secondary_id = secondary_id,
                 number_secondary_extinctions = number_secondary_extinctions,
                 resulting_stage_web = resulting_stage_web)
  
  # return(output)
  
}
  
  
  number_removals <- 25
  sequential <- rep(NAgm
                    , number_removals)
  
  a <- stage_matrix
  
  for (i in 1:length(sequential)){


    print(dim(a))    
    x <- secondary_extinction(a)

    a <- x$resulting_stage_web
    
    sequential[i] <- x$number_secondary_extinctions
    
  }
  

  plot(1:number_removals, cumsum(sequential), 
       xlab = 'species removed', ylab = 'cumulative secondary extinctions', 
       pch = 19, 
       xlim = c(0,number_removals), ylim=c(0,number_removals))
  
  abline(0,1, lty=2)
  
  