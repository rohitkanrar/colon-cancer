#############################################################
#################### TIME INTENSIVE COMPUTATION #############
#############################################################

library(nimble, warn.conflicts = FALSE)
set.seed(100)
source("~/Research/RA/colon-cancer/project/generate_data.R")
source("~/Research/RA/colon-cancer/project/model_matrix.R")

mod <- nimbleCode({
  for(t in 1:total){
    y[t] ~ dbern(p[t])
    probit(p[t]) <- inprod(theta[1:(M-1)], Theta[t, 1:(M-1)]) + inprod(delta[1:(M-1)], Delta[t, 1:(M-1)]) + mu * Mu[t] + inprod(gamma[1:(2*N)], Gamma[t, 1:(2*N)])
  }
  for(m in 1:(M-1)){
    theta[m] ~ dnorm(0, var=100)
    delta[m] ~ dt(0, 1, 1)
  }
  mu ~ dt(0, 1, 1)
  for(n in 1:(2*N)){
    gamma[n] ~ dnorm(0, var = tau2)
  }
  tau2 ~ dinvgamma(2, 1)
})

data <- list(y = res, Theta = Theta, Delta = Delta,
             Gamma = Gamma, Mu = Mu)
constants <- list(M = M, N = N, total = 2*N*L)
inits <- list(theta = rnorm(M-1), delta = rnorm(M-1), 
              mu = rnorm(1), gamma = rnorm(2*N), tau2 = 1)

mcmc_bayes <- nimbleMCMC(code = mod, inits = inits,
                      constants = constants, data = data,
                      monitors = c('theta', 'delta', 'mu', 'gamma', 'tau2'),
                      thin = 10, niter = 35000, nburnin = 5000, nchains = 3, 
                      setSeed = TRUE)

saveRDS(mcmc_bayes, "~/Research/RA/colon-cancer/project/mcmc_bayes.RData")