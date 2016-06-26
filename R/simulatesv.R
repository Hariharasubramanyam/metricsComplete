simulatesv <- function(omega, N){
  
  constant <- omega[1]
  phi      <- omega[2]
  tausq    <- omega[3]
  
  alpha     <- rep(0, N)
  svseries  <- rep(0, N)
  
  eta      <- rnorm(N, 0, sqrt(tausq))
  z        <- rnorm(N, 0, 1)
  nu       <- rnorm(N, 0, 1)
  
  alpha[1]    <- constant
  svseries[1] <- z[1] * exp(alpha[1]/2)
  
  for (i in 2:N){
    alpha[i] <- constant + (phi * alpha[i - 1]) + eta[i]
    svseries[i] <- z[i] * exp(alpha[i]/ 2)
  }
  
  return(list(alpha = alpha, svseries = svseries))
}