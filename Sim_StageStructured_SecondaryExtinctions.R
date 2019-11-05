source('FoodWebFunctions.R')
source('generateStageMatrix.R')
source('secondary_extinction.R')


S = 50    ## set species richness
C = 0.1   ## set connectance
N = 1     ## set the number of replicate webs to make
L = S^2*C  ## calculate number of links from S and C

xxx <- Cascade.model(S, L, N)


p_range <- seq(0.1, 0.9, by = 0.1)

number_removals <- 25
sequential <- array(NA, dim = c(length(p_range), number_removals))

for (j in 1:length(p_range)){


stage_matrix <- generateStageMatrix(xxx, p_range[j])

a <- stage_matrix

for (i in 1:number_removals){
  
  print(i)
  print(dim(a))
  
  x <- secondary_extinction(a)
  
  a <- x$resulting_stage_web
  
  sequential[j,i] <- x$number_secondary_extinctions
  
}

}

rainbow_vec <- rainbow(length(p_range))
plot(1:number_removals, cumsum(sequential[1,]), 
     xlab = 'species removed', ylab = 'cumulative secondary extinctions', 
     type = 'l', lwd = 2,
     xlim = c(0,number_removals), ylim=c(0,max(apply(sequential, 1 ,cumsum))))
for (i in 2:length(p_range)){
  points(1:number_removals, cumsum(sequential[i,]), col = rainbow_vec[i-1], type = 'l', lwd = 2)
}
  abline(0,1, lty=2)

