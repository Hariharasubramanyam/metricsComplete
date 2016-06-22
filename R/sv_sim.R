sv_sim <- function(theta, L){

   constant <- theta[1]
   phi      <- theta[2]
   tausq    <- theta[3]

   alpha    <- rep(0, L)
   y        <- rep(0, L)

   eta      <- rnorm(L, 0, sqrt(tausq))
   z        <- rnorm(L, 0, 1)
   nu       <- rnorm(L, 0, 1)

   alpha[1] <- constant
   y[1]     <- z[1] * exp(alpha[1]/2)

   for (i in 2:L){
      alpha[i] <- constant + (phi * alpha[i - 1]) + eta[i]
      y[i] <- z[i] * exp(alpha[i]/ 2)
   }

   return(list(alpha = alpha, y = y))
}