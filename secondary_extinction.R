secondary_extinction <- function(stage_web){
  
  S <- dim(stage_web)[1]
  max_stages <- dim(stage_web)[3]
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

