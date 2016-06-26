garchvoltrack <- function(series, garchmodel){
  N <- length(series)
  coefficients <- as.numeric(garchmodel@fit$coef)
  z <- rnorm(N, 0, 1)
  track   <- rep(0, N)
  predict <- rep(0, N)
  
  track[1]   <- series[1]
  predict[1] <- series[1]
  
  for(i in 2:N){
    track[i] <- coefficients[2] + (coefficients[3] * (series[i-1]^2)) + (coefficients[4] * track[i - 1])
    predict[i] <- z[i]*sqrt(track[i])
  }
  return(list(track = track, predict = fitted))
}


svvoltrack <- function(svmodel, series, P){
  
  N <- length(series)
  
  constant <- svmodel$par[1]
  phi      <- svmodel$par[2]
  tausq    <- svmodel$par[3]    
  
  alpha_up <- rnorm(P,0,0.1)
  alpha_pr <- rep(0,P)
  alpha_wt <- (rep(1,P))/P
  
  alpha_up_mat <- matrix(0, N, 3)
  alpha_pr_mat <- matrix(0, N, 3)
  alpha_pr_are <- matrix(0, N, 20)
  
  for ( i in 1:N){
    alpha_pr <- constant + (phi * alpha_up) + rnorm(P, 0, sqrt(tausq))
    alpha_wt <- dnorm( (returns$insamp[i] * rep(1,P)), rep(0, P), exp(alpha_pr/2))
    alpha_up <- csir(alpha_pr, alpha_wt, sort(runif(P,0,1)))
    
    alpha_up_mat[i,2] <- mean(alpha_up)
    alpha_up_mat[i,1] <- quantile(alpha_up, 0.05)
    alpha_up_mat[i,3] <- quantile(alpha_up, 0.95)
    alpha_pr_mat[i,2] <- mean(alpha_pr)
    alpha_pr_mat[i,1] <- quantile(alpha_pr, 0.05)
    alpha_pr_mat[i,3] <- quantile(alpha_pr, 0.95)
    
    alpha_pr_are[i,] = quantile(alpha_pr , seq(0, 1, by = ncol(alpha_pr_are)) )   
  }
  
  return(list(alpha_up_mat = alpha_up_mat, alpha_pr_mat = alpha_pr_mat))
 
}
