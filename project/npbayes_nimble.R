#############################################################
#################### TIME INTENSIVE COMPUTATION #############
#############################################################
setwd("~/Research/RA/colon-cancer/project/") # change work dir accordingly

library(nimble, warn.conflicts = FALSE)
set.seed(100)
source("generate_data.R")
source("model_matrix.R")

mod_bnp <- nimbleCode({
  for(t in 1:total){
    y[t] ~ dbern(p[t])
    probit(p[t]) <- inprod(theta[1:(M-1)], Theta[t, 1:(M-1)]) + inprod(delta[1:(M-1)], Delta[t, 1:(M-1)]) + mu * Mu[t] + inprod(gamma[1:(2*N)], Gamma[t, 1:(2*N)])
  }
  for(m in 1:(M-1)){
    theta[m] ~ dnorm(th_mu[m], th_var[m])
    th_mu[m] <- th_tilde_mu[xi_th[m]]
    th_var[m] <- th_tilde_var[xi_th[m]]
    delta[m] ~ dt(0, 1, 1)
  }
  for(m in 1:(M-1)){
    th_tilde_mu[m] ~ dnorm(mu0, var = var0)
    th_tilde_var[m] ~ dinvgamma(a0, b0)
  }
  xi_th[1:(M-1)] ~ dCRP(alpha, size = (M-1))
  mu ~ dt(0, 1, 1)
  for(n in 1:(2*N)){
    gamma[n] ~ dnorm(0, var = tau2)
  }
  tau2 ~ dinvgamma(2, 1)
  alpha ~ dgamma(1, 1)
  mu0 ~ dnorm(0, 10)
  var0 ~ dinvgamma(2, 1)
  a0 ~ dinvgamma(2, 1)
  b0 ~ dinvgamma(2, 1)
})

data <- list(y = res, Theta = Theta, Delta = Delta,
             Gamma = Gamma, Mu = Mu)
constants <- list(M = M, N = N, total = 2*N*L)
inits <- list(theta = rnorm(M-1), delta = rnorm(M-1), 
              mu = rnorm(1), gamma = rnorm(2*N), tau2 = 1,
              xi_th = sample(1:2, (M-1), replace = T),
              th_tilde_mu = rnorm(M-1), th_tilde_var = rep(1, M-1),
              alpha = 1, mu0 = 0, var0 = 1, a0 = 1, b0 = 1)

mcmc_bnp <- nimbleMCMC(code = mod_bnp, inits = inits,
                      constants = constants, data = data,
                      monitors = c('theta', 'delta', 'mu', 'gamma', 'tau2',
                                   'alpha', 'xi_th', "mu0", "var0", 
                                   "a0", "b0"),
                      thin = 10, niter = 35000, nburnin = 5000, nchains = 3, 
                      setSeed = TRUE)

saveRDS(mcmc_bnp, "output/mcmc_bnp.RData")