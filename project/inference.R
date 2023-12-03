#############################################################
#################### TIME INTENSIVE COMPUTATION #############
#############################################################

mcmc_bayes <- readRDS("~/Research/RA/colon-cancer/project/mcmc_bayes.RData")
mcmc_bnp <- readRDS("~/Research/RA/colon-cancer/project/mcmc_bnp.RData")

library(MCMCvis)

# Code to generate Table 1 in the report.

summary_bayes <- MCMCsummary(object = mcmc_bayes, round = 2)
summary_bnp <- MCMCsummary(object = mcmc_bnp, round = 2)

xtable::xtable(
cbind(theta[1:11], summary_bayes[114:124, c(1, 2, 3, 5, 6)],
      summary_bnp[118:128, c(1, 2, 3, 5, 6)])
)

# Code to generate Figure 1.

par(mfrow=c(1, 2))
MCMCplot(object = mcmc_bayes,
         params = 'theta', main = "Parametric Priors")
MCMCplot(object = mcmc_bnp,
         params = 'theta', main = "Non-parametric Priors")


hist(c(as.numeric(mcmc_bayes$chain1[, 114:124]),
       as.numeric(mcmc_bayes$chain2[, 114:124]),
       as.numeric(mcmc_bayes$chain3[, 114:124])), breaks = 70,
     xlab = expression(theta), probability = T,
     main = "Parametric Priors", xlim = c(-10, 20),
     ylim = c(0, 0.18))


hist(c(as.numeric(mcmc_bnp$chain1[, 118:128]),
       as.numeric(mcmc_bnp$chain2[, 118:128]),
       as.numeric(mcmc_bnp$chain3[, 118:128])), breaks = 50,
     xlab = expression(theta), probability = T,
     main = "Non-parametric Priors", xlim = c(-10, 20),
     ylim = c(0, 0.18))
par(mfrow=c(1,1))



# Traceplots
MCMCtrace(object = mcmc_bayes,
          pdf = TRUE, # no export to PDF
          ind = TRUE, # separate density lines per chain
          params = 'theta',
          filename = "/Research/RA/colon-cancer/project/bayes_trace.pdf",
          Rhat = T, )

MCMCtrace(object = mcmc_bnp,
          pdf = FALSE, # no export to PDF
          ind = TRUE, # separate density lines per chain
          params = "theta")

# order(theta[1:11])
# rownames(summary_bayes)[114:124][order(summary_bayes[114:124, 1])]
# rownames(summary_bnp)[118:128][order(summary_bnp[118:128, 1])]
