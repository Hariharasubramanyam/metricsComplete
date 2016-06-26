csir <- function(alpha_pr, alpha_wt, u){
  N         <- length(alpha_pr)
  alpha_up  <- rep(0, N)
  
  alpha_wt  <- alpha_wt/sum(alpha_wt)
  sorted    <- sort(alpha_pr, index.return = T)
  alpha_pr  <- sorted$x
  alpha_wt  <- alpha_wt[sorted$ix] 
  
  alpha_cwt <- c(0 , cumsum(alpha_wt))
  alpha_pr  <- c(alpha_pr[1] , alpha_pr)
  
  j <- 1
  for (i in 1:N){
    while (alpha_cwt[i] < u[j] & u[j] <= alpha_cwt[i+1]){
      alpha_up[j] <- alpha_pr[i] + ( (alpha_pr[i+1] - alpha_pr[i])/(alpha_cwt[i+1] - alpha_cwt[i]) ) * (u[j] - alpha_cwt[i])
      if (j < N){
        j <- j + 1
      }
      else {break}
    }
  }
  return(alpha_up)
}