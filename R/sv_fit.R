sv_fit <- function(y, theta, P, estimate){
	
	N <- length(y)

	alpha_up_0 <- rnorm(P, 0, 1)
	alpha_wt_0 <- rep(0, P)/P
	eta_sim    <- matrix(rnorm(P*N, 0, 1), P, N)
	u_sim      <- matrix(runif(P*N, 0, 1), P, N)

	for (i in 1:N){
  		u_sim[,i] <- sort(u_sim[,i])
	}

	lb <- rep(0, length(theta)) + 0.001
	ub <- rep(1, length(theta)) - (2*(10^-10))

	theta = c(var(y)*(1-0.95), 0.95, 0.1)

	sv_proxy <- function(par){
  		output <- sv_loglik(par, y, eta_sim, u_sim, alpha_up_0, alpha_wt_0)
  		return(output)
	}

	trial <- nlminb(start = theta, control = list(trace = 1), objective = sv_proxy, lower = lb, upper = ub)

	return(output$par)
}