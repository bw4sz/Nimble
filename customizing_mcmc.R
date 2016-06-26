## ----chunksetup, include=FALSE-------------------------------------------
# Following code is only needed for slide generation, not for using R code separately.
library(methods)
read_chunk('chunks_ssm.R')

## ----loadnimble, include=FALSE-------------------------------------------
library(nimble)

## ---- ssm-code-----------------------------------------------------------

## ---- ssm-model----------------------------------------------------------

## ---- ssm-compile--------------------------------------------------------

## ---- ssm-mcmc, fig.width=12, fig.height=5-------------------------------
mcmcConf <- buildMCMC(ssm) # can skip mcmcConf
CssmMCMC <- compileNimble(mcmcConf, project = ssm)
CssmMCMC$run(10000)
samples <- as.matrix(CssmMCMC$mvSamples)
tsplot <- function(x, ...) plot(seq_along(x), x, type = 'l', ...)

par(mfrow = c(1, 3), mai = c(.6, .5, .1, .2))
tsplot(samples[ , 'mean.r'], xlab = 'iteration',
     ylab = 'mean.r', main = 'mean.r')
tsplot(samples[ , 'sigma.proc'], xlab = 'iteration',
     ylab = 'sigma.proc', main = 'sigma.proc')
tsplot(samples[ , 'sigma.obs'], xlab = 'iteration',
     ylab = 'sigma.obs', main = 'sigma.obs')

## ---- ssm-mcmc2----------------------------------------------------------
mcmcConf2 <- configureMCMC(ssm) 
mcmcConf2$removeSamplers('sigma.obs')
mcmcConf2$removeSamplers('sigma.proc')
mcmcConf2$removeSamplers('r')
## sample variance terms on log scale
mcmcConf2$addSampler(target = 'sigma.obs', type = 'RW', control = list(log=TRUE))
mcmcConf2$addSampler(target = 'sigma.proc', type = 'RW', control = list(log=TRUE))
## illustrate how we can access all r[t] nodes:
ssm$expandNodeNames('r')
mcmcConf2$addSampler(target = ssm$expandNodeNames('r'), type = 'RW_block')
mcmcConf2$printSamplers()

## ---- run-ssm-mcmc2------------------------------------------------------
ssmMCMC2 <- buildMCMC(mcmcConf2)
Cssm <- compileNimble(ssm)
CssmMCMC2 <- compileNimble(ssmMCMC2, project = ssm)
CssmMCMC2$run(10000)

## ---- fig.width=12, fig.height=5-----------------------------------------
samples2 <- as.matrix(CssmMCMC2$mvSamples)

par(mfrow = c(1, 3), mai = c(.6, .5, .1, .2))
tsplot(samples2[ , 'mean.r'], xlab = 'iteration',
     ylab = 'mean.r', main = 'mean.r')
tsplot(samples2[ , 'sigma.proc'], xlab = 'iteration',
     ylab = 'sigma.proc', main = 'sigma.proc')
tsplot(samples2[ , 'sigma.obs'], xlab = 'iteration',
     ylab = 'sigma.obs', main = 'sigma.obs')


## ---- compare-mcmcs------------------------------------------------------
set.seed(1)
modelInfo <- list(ssm = list(code = ssmCode, data = bugs.data['y'], 
          constants = bugs.data['T'], inits = inits()))
MCMCdefs <- list(custom = quote({
    mcmcConf2 <- configureMCMC(Rmodel) 
    mcmcConf2$addSampler(target = 'sigma.obs', type = 'RW', control = list(log=TRUE))
    mcmcConf2$addSampler(target = 'sigma.proc', type = 'RW', control = list(log=TRUE))
    mcmcConf2
}))
comparisonResults <- compareMCMCs(modelInfo = modelInfo,
                                  MCMCs = c('nimble', 'custom'),
                                  MCMCdefs = MCMCdefs,
                                  niter = 50000,
                                  burnin = 5000, summary = FALSE)

## ---- make-comparison-figures--------------------------------------------
make_MCMC_comparison_pages(comparisonResults, dir = 'ssm_custom_MCMC_results')
## open ssm.html in a browser to see results
## The name ssm is taken from the name in the modelInfo list

