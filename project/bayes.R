library(R2jags)
bt.mod <- function(){
  # Priors
  for(m in 1:(M-1)){
    theta_[m] ~ dnorm(0, 1)
    delta_[m] ~ dnorm(0, 1)
  }
  theta <- c(theta_, -sum(theta_))
  delta <- c(delta_, -sum(delta_))
  mu_ ~ dnorm(0, 1)
  mu <- c(mu_, -mu_)
  tau_ ~ dgamma(0.01, 0.01)
  for(k in 1:(N-1)){
    gamma_[k] ~ dnorm(0, tau_)
  }
  gamma <- c(gamma_, -sum(gamma_))
  # Probabilities
  for(r in 1:2){
    for(k in 1:N){
      for(l in 1:L){
        i[r, k, l] <- ifelse(r == 1, pw[L*(k-1)+l, 1], pb[L*(k-1)+l, 1])
        j[r, k, l] <- ifelse(r == 1, pw[L*(k-1)+l, 2], pb[L*(k-1)+l, 2])
        probit(p[r, k, l]) <- theta[i[r, k, l]] - theta[j[r, k, l]] + 
          ifelse(r == 1, 1, -1) * (delta[i[r, k, l]] - delta[j[r, k, l]] + 
                   mu[r] + gamma[k])
        y[r, k, l] ~ dbinom(p[r, k, l], 1)
      }
    }
  }
}

bt.dat <- list(y = y, M = M, N = N, L = L,
               pw = pairs_white, pb = pairs_black)
bt.param <- c('theta_', 'delta_', 'mu_', 'gamma_', 'tau_')
bt.mod.inits <- function(){
  list('delta_' = rnorm(M-1),
       'theta_' = rnorm(M-1),
       'mu_' = rnorm(1),
       'gamma_' = rnorm(N-1),
       'tau_' = rnorm(1))
}
bt.mod.fit <- jags(data = bt.dat, parameters.to.save = bt.param,
                   model.file = bt.mod,
                   n.chains = 3, n.iter = 9000, n.burnin = 2000)
bt.mcmc <- as.mcmc(bt.mod.fit)
print(bt.mod.fit)
pdf("Research/RA/colon-cancer/project/toy_mcmc.pdf")
plot(bt.mcmc)
dev.off()
