## ----loadnimble, include=FALSE-------------------------------------------
library(nimble)

## ----chunksetup, include=FALSE-------------------------------------------
# Following code is only needed for slide generation, not for using R code separately.
library(methods)

## ---- ssm-reparam--------------------------------------------------------
ssmCode <- nimbleCode({
    # Priors and constraints
    logN.est[1] ~ dnorm(5.6, 0.01)       # Prior for initial population size
    mean.r ~ dnorm(1, 0.001)             # Prior for mean growth rate
    sigma.proc ~ dunif(0, 1)             # Prior for sd of state process
    sigma.obs ~ dunif(0, 1)              # Prior for sd of observation process

   # State process
    for (t in 1:(T-1)){
#        r[t] ~ dnorm(mean.r, sd = sigma.proc)
        logN.est[t+1] ~ dnorm(logN.est[t] + mean.r, sd = sigma.proc)
    }
    
    # Observation process
    for (t in 1:T) {
        y[t] ~ dnorm(logN.est[t], sd = sigma.obs)
    }
})

## @knitr ssm-model
hm <- c(271, 261, 309, 318, 231, 216, 208, 226, 195, 226,
        233, 209, 226, 192, 191, 225,
        245, 205, 191, 174)
year <- 1990:2009

# Bundle data
bugs.data <- list(y = log(hm), T = length(year))
## NIMBLE will handle y as data, T as a constant

# Initial values
inits <- function(){list(sigma.proc = runif(1, 0, 1), mean.r = rnorm(1),
                         sigma.obs = runif(1, 0, 1),
                         logN.est = c(rnorm(1, 5.6, 0.1),
                                      rep(NA, (length(year)-1))))}
set.seed(1)
ssm <- nimbleModel(ssmCode, constants = bugs.data, inits = inits()) 

## ---- mcem, eval=FALSE---------------------------------------------------
## mcem = buildMCEM(ssm, ssm$getNodeNames(latentOnly = TRUE, stochOnly = TRUE),
##                  boxConstraints = list(list('mean.r', c(-1,1)),
##                                        list('sigma.proc', c(0, 1)),
##                                        list('sigma.obs', c(0, 1))))
## output <- mcem()

