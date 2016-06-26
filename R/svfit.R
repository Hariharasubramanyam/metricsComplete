svfit <- function (y, omega, P) {
  N <- length(y)
  alpha_up_init <- rnorm(P, 0, 1)
  alpha_wt_init <- rep(0, P)/P
  eta_sim <- matrix(rnorm(P * N, 0, 1), P, N)
  u_sim <- matrix(runif(P * N, 0, 1), P, N)
  
  for (i in 1:N) {
    u_sim[, i] <- sort(u_sim[, i])
  }
  lb <- rep(0, length(omega)) + 0.01
  ub <- rep(1, length(omega)) - (2 * (10^-10))
  omega = c(var(y) * (1 - 0.95), 0.95, 0.1)
  sv_proxy <- function(par) {
    output <- svll(par, y, eta_sim, u_sim, alpha_up_init, 
                        alpha_wt_init)
    return(output)
  }
  trial <- optim(par = omega, fn = sv_proxy, upper = ub, lower = lb, control = list(trace = 1), method = "L-BFGS-B", hessian = T)
  return(trial)
}