generateStageMatrix <- function(web, p_nicheoverlap){

S <- dim(web)[1]
num_resources <- colSums(web)

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

return(stage_matrix)
}
