sv_loglik <- function(theta, y, eta_sim, u_sim, alpha_up, alpha_wt){

   N           <- length(y)
   P           <- length(alpha_up)

   constant    <- theta[1]
   phi         <- theta[2]
   tausq       <- theta[3]

   alpha_up_pr <- as.data.frame(matrix(0,N,4))
   loglik      <- 0
   
   for (i in 1:N){
      alpha_pr  <- constant + (phi * alpha_up) + (sqrt(tausq) * eta_sim[,i])

      lik       <- dnorm( y[i]*rep(1, P), rep(0, P), exp(alpha_pr/2) ) 
         loglik <- loglik - log( mean( lik))
   
      alpha_wt  <- lik
      alpha_up  <- csir(alpha_pr,alpha_wt,u_sim[,i])

      alpha_up_pr[i,1]   <- mean( alpha_up )
      alpha_up_pr[i,2]   <- mean( alpha_pr )
      alpha_up_pr[i,3:4] <- quantile( alpha_pr , c(0.05, 0.95))
   }
   
   loglik <- loglik/T;
   return(loglik)
}