---
title: "Customizing and comparing MCMCs (state-space example)"
subtitle: "NIMBLE training materials module"
author: "NIMBLE Development Team"
output:
  html_document:
    code_folding: show
---





# Outline

In this module we will:

   - Use the house martin state-space model from K&eacute;ry and Schaub (2012).
   - Try block samplers for the `r[t]`s.  Often a key factor that reduces MCMC performance is dependence between parameters that limits the ability of univariate samplers to move very far. A standard strategy is to sample correlated parameters in blocks. 

       + This actually doesn't help much in this model (and will
       be more efficient in a future NIMBLE release) but we'll
       illustrate it anyway.

   - Try Metropolis-Hastings samplers on a log scale for `sigma.obs` and `sigma.proc`
   - Automatically compare the customized MCMC to the default MCMC using `compareMCMCs`.

# Build the model and default MCMC

We first need to build the model.


```r
library(nimble)
ssmCode <- nimbleCode({
    # Priors and constraints
    logN.est[1] ~ dnorm(5.6, 0.01)       # Prior for initial population size
    mean.r ~ dnorm(1, 0.001)             # Prior for mean growth rate
    sigma.proc ~ dunif(0, 1)             # Prior for sd of state process
    sigma.obs ~ dunif(0, 1)              # Prior for sd of observation process

   # State process
    for (t in 1:(T-1)){
        r[t] ~ dnorm(mean.r, sd = sigma.proc)
        logN.est[t+1] <- logN.est[t] + r[t]
    }
    
    # Observation process
    for (t in 1:T) {
        y[t] ~ dnorm(logN.est[t], sd = sigma.obs)
    }

    # Population sizes on real scale
    for (t in 1:T) {
        N.est[t] <- exp(logN.est[t])
    }
})
```


```r
pyears <- 6 # Number of future years with predictions
hm <- c(271, 261, 309, 318, 231, 216, 208, 226, 195, 226,
        233, 209, 226, 192, 191, 225,
        245, 205, 191, 174, rep(NA, pyears))
year <- 1990:(2009 + pyears)

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
```

```
## defining model...
```

```
## Detected y as data within 'constants'.
## Adding y as data for building model.
```

```
## building model...
```

```
## setting data and initial values...
```

```
## checking model...   (use nimbleModel(..., check = FALSE) to skip model check)
```

```
## NAs were detected in model variables: r, logProb_r, logN.est, logProb_y, N.est, y.
```

```
## model building finished
```


```r
Cssm <- compileNimble(ssm)
```


```r
mcmcConf <- buildMCMC(ssm) # can skip mcmcConf
CssmMCMC <- compileNimble(mcmcConf, project = ssm)
CssmMCMC$run(10000)
```

```
## NULL
```

```r
samples <- as.matrix(CssmMCMC$mvSamples)
tsplot <- function(x, ...) plot(seq_along(x), x, type = 'l', ...)

par(mfrow = c(1, 3), mai = c(.6, .5, .1, .2))
tsplot(samples[ , 'mean.r'], xlab = 'iteration',
     ylab = 'mean.r', main = 'mean.r')
tsplot(samples[ , 'sigma.proc'], xlab = 'iteration',
     ylab = 'sigma.proc', main = 'sigma.proc')
tsplot(samples[ , 'sigma.obs'], xlab = 'iteration',
     ylab = 'sigma.obs', main = 'sigma.obs')
```

![plot of chunk ssm-mcmc](figure/ssm-mcmc-1.png)

# Customizing and MCMC configuration

MCMC is a family of algorithms and there are many ways to create an MCMC for any given model by choosing different samplers (or "updaters").  Different choices can have vastly different efficiencies for different models and data sets.

There are several ways we can customize an MCMC configuration in NIMBLE.

Here we will start from a default configuration and then remove and add samplers.


```r
mcmcConf2 <- configureMCMC(ssm) 
mcmcConf2$removeSamplers('sigma.obs')
mcmcConf2$removeSamplers('sigma.proc')
mcmcConf2$removeSamplers('r')
## sample variance terms on log scale
mcmcConf2$addSampler(target = 'sigma.obs', type = 'RW', control = list(log=TRUE))
mcmcConf2$addSampler(target = 'sigma.proc', type = 'RW', control = list(log=TRUE))
## illustrate how we can access all r[t] nodes:
ssm$expandNodeNames('r')
```

```
##  [1] "r[1]"  "r[2]"  "r[3]"  "r[4]"  "r[5]"  "r[6]"  "r[7]"  "r[8]" 
##  [9] "r[9]"  "r[10]" "r[11]" "r[12]" "r[13]" "r[14]" "r[15]" "r[16]"
## [17] "r[17]" "r[18]" "r[19]" "r[20]" "r[21]" "r[22]" "r[23]" "r[24]"
## [25] "r[25]"
```

```r
mcmcConf2$addSampler(target = ssm$expandNodeNames('r'), type = 'RW_block')
mcmcConf2$printSamplers()
```

```
## [1]  conjugate_dnorm_dnorm sampler: logN.est[1],  dep_dnorm: y[26], y[25], y[24], y[23], y[22], y[21], y[20], y[19], y[18], y[17], y[16], y[15], y[14], y[13], y[12], y[11], y[10], y[9], y[8], y[7], y[6], y[5], y[4], y[3], y[2], y[1]
## [2]  conjugate_dnorm_dnorm sampler: mean.r,  dep_dnorm: r[1], r[2], r[3], r[4], r[5], r[6], r[7], r[8], r[9], r[10], r[11], r[12], r[13], r[14], r[15], r[16], r[17], r[18], r[19], r[20], r[21], r[22], r[23], r[24], r[25]
## [3]  posterior_predictive sampler: y[21]
## [4]  posterior_predictive sampler: y[22]
## [5]  posterior_predictive sampler: y[23]
## [6]  posterior_predictive sampler: y[24]
## [7]  posterior_predictive sampler: y[25]
## [8]  posterior_predictive sampler: y[26]
## [9]  RW sampler: sigma.obs,  log: TRUE
## [10] RW sampler: sigma.proc,  log: TRUE
## [11] RW_block sampler: r[1], r[2], r[3], r[4], r[5], r[6], r[7], r[8], r[9], r[10], r[11], r[12], r[13], r[14], r[15], r[16], r[17], r[18], r[19], r[20], r[21], r[22], r[23], r[24], r[25]
```

# Build, compile and run the customized MCMC


```r
ssmMCMC2 <- buildMCMC(mcmcConf2)
Cssm <- compileNimble(ssm)
CssmMCMC2 <- compileNimble(ssmMCMC2, project = ssm)
CssmMCMC2$run(10000)
```

```
## NULL
```


```r
samples2 <- as.matrix(CssmMCMC2$mvSamples)

par(mfrow = c(1, 3), mai = c(.6, .5, .1, .2))
tsplot(samples2[ , 'mean.r'], xlab = 'iteration',
     ylab = 'mean.r', main = 'mean.r')
tsplot(samples2[ , 'sigma.proc'], xlab = 'iteration',
     ylab = 'sigma.proc', main = 'sigma.proc')
tsplot(samples2[ , 'sigma.obs'], xlab = 'iteration',
     ylab = 'sigma.obs', main = 'sigma.obs')
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png)

# Comparing MCMC configurations

We've made two kinds of MCMCs for the same model.  How can we compare
them?

We must consider two factors:

   - How well the MCMC mixes.  This is measured by the *effective
      sample size* (ESS) for each parameter.

   - How quickly the MCMC runs.  This is measured by its computation
   time.

We define *MCMC efficiency* for one parameter as

   - MCMC efficiency (of a parameter) = ESS / time (in seconds)

That gives a measure of performance for each parameter, but how we do
summarize those across parameters to get a single number?

Often the trustworthiness of results is limited by the worst-mixing
parameter.  Therefore we define

   - MCMC efficiency (of an algorithm) = minimum MCMC efficiency of
     all 

# Comparing MCMC configurations using `compareMCMCs`

After some preliminary exploration, one sees that the slowest-mixing
parameters are the two standard deviations. A good strategy for
them to mix is to include two samplers for each: one on a regular scale
and one on a log scale. So we'll include just that modification for
the comparison.


```r
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
```

```
## Working on ssm
```

```
## defining model...
```

```
## building model...
```

```
## setting data and initial values...
```

```
## checking model...   (use nimbleModel(..., check = FALSE) to skip model check)
```

```
## NAs were detected in model variables: r, logProb_r, logN.est, logProb_y, N.est, y.
```

```
## model building finished
```

Now we'll make the comparison figures:


```r
make_MCMC_comparison_pages(comparisonResults, dir = 'ssm_custom_MCMC_results')
## open ssm.html in a browser to see results
## The name ssm is taken from the name in the modelInfo list
```

The results are [here](ssm_custom_MCMC_results/ssm.html)


