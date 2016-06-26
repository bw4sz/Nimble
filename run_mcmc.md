---
title: "Running an MCMC (state-space model example)"
subtitle: "NIMBLE training materials module"
author: "NIMBLE Development Team"
output:
  html_document:
    code_folding: show
---




# A basic MCMC

The steps of running an MCMC are as follows:

 - configure the MCMC (can be combined with next step)
 - build the MCMC 
 - compile the MCMC
 - run the MCMC
 - extract the MCMC samples

# Build the model

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

# Configuring a basic MCMC

*Configuring* the MCMC means setting up the samplers to be used for
 each node or group of nodes, which nodes will be recorded and any
 thinning.

You can modify the configuration before you build the actual MCMC algorithm for compiling and running.


NIMBLE provides a default configuration, but we'll see shortly how you can modify that. 

The entire MCMC system is written using nimbleFunctions, so you can read and modify the source code in R or write your own MCMC system if you want.

It is easy to write new samplers as nimbleFunctions and include them in an MCMC configuration.


```r
mcmcConf <- configureMCMC(ssm)
```

You can see what kind of sampler has been assigned to each node:

```r
mcmcConf$printSamplers()
```

```
## [1]  conjugate_dnorm_dnorm sampler: logN.est[1],  dep_dnorm: y[26], y[25], y[24], y[23], y[22], y[21], y[20], y[19], y[18], y[17], y[16], y[15], y[14], y[13], y[12], y[11], y[10], y[9], y[8], y[7], y[6], y[5], y[4], y[3], y[2], y[1]
## [2]  conjugate_dnorm_dnorm sampler: mean.r,  dep_dnorm: r[1], r[2], r[3], r[4], r[5], r[6], r[7], r[8], r[9], r[10], r[11], r[12], r[13], r[14], r[15], r[16], r[17], r[18], r[19], r[20], r[21], r[22], r[23], r[24], r[25]
## [3]  RW sampler: sigma.proc
## [4]  RW sampler: sigma.obs
## [5]  conjugate_dnorm_dnorm sampler: r[1],  dep_dnorm: y[26], y[25], y[24], y[23], y[22], y[21], y[20], y[19], y[18], y[17], y[16], y[15], y[14], y[13], y[12], y[11], y[10], y[9], y[8], y[7], y[6], y[5], y[4], y[3], y[2]
## [6]  conjugate_dnorm_dnorm sampler: r[2],  dep_dnorm: y[26], y[25], y[24], y[23], y[22], y[21], y[20], y[19], y[18], y[17], y[16], y[15], y[14], y[13], y[12], y[11], y[10], y[9], y[8], y[7], y[6], y[5], y[4], y[3]
## [7]  conjugate_dnorm_dnorm sampler: r[3],  dep_dnorm: y[26], y[25], y[24], y[23], y[22], y[21], y[20], y[19], y[18], y[17], y[16], y[15], y[14], y[13], y[12], y[11], y[10], y[9], y[8], y[7], y[6], y[5], y[4]
## [8]  conjugate_dnorm_dnorm sampler: r[4],  dep_dnorm: y[26], y[25], y[24], y[23], y[22], y[21], y[20], y[19], y[18], y[17], y[16], y[15], y[14], y[13], y[12], y[11], y[10], y[9], y[8], y[7], y[6], y[5]
## [9]  conjugate_dnorm_dnorm sampler: r[5],  dep_dnorm: y[26], y[25], y[24], y[23], y[22], y[21], y[20], y[19], y[18], y[17], y[16], y[15], y[14], y[13], y[12], y[11], y[10], y[9], y[8], y[7], y[6]
## [10] conjugate_dnorm_dnorm sampler: r[6],  dep_dnorm: y[26], y[25], y[24], y[23], y[22], y[21], y[20], y[19], y[18], y[17], y[16], y[15], y[14], y[13], y[12], y[11], y[10], y[9], y[8], y[7]
## [11] conjugate_dnorm_dnorm sampler: r[7],  dep_dnorm: y[26], y[25], y[24], y[23], y[22], y[21], y[20], y[19], y[18], y[17], y[16], y[15], y[14], y[13], y[12], y[11], y[10], y[9], y[8]
## [12] conjugate_dnorm_dnorm sampler: r[8],  dep_dnorm: y[26], y[25], y[24], y[23], y[22], y[21], y[20], y[19], y[18], y[17], y[16], y[15], y[14], y[13], y[12], y[11], y[10], y[9]
## [13] conjugate_dnorm_dnorm sampler: r[9],  dep_dnorm: y[26], y[25], y[24], y[23], y[22], y[21], y[20], y[19], y[18], y[17], y[16], y[15], y[14], y[13], y[12], y[11], y[10]
## [14] conjugate_dnorm_dnorm sampler: r[10],  dep_dnorm: y[26], y[25], y[24], y[23], y[22], y[21], y[20], y[19], y[18], y[17], y[16], y[15], y[14], y[13], y[12], y[11]
## [15] conjugate_dnorm_dnorm sampler: r[11],  dep_dnorm: y[26], y[25], y[24], y[23], y[22], y[21], y[20], y[19], y[18], y[17], y[16], y[15], y[14], y[13], y[12]
## [16] conjugate_dnorm_dnorm sampler: r[12],  dep_dnorm: y[26], y[25], y[24], y[23], y[22], y[21], y[20], y[19], y[18], y[17], y[16], y[15], y[14], y[13]
## [17] conjugate_dnorm_dnorm sampler: r[13],  dep_dnorm: y[26], y[25], y[24], y[23], y[22], y[21], y[20], y[19], y[18], y[17], y[16], y[15], y[14]
## [18] conjugate_dnorm_dnorm sampler: r[14],  dep_dnorm: y[26], y[25], y[24], y[23], y[22], y[21], y[20], y[19], y[18], y[17], y[16], y[15]
## [19] conjugate_dnorm_dnorm sampler: r[15],  dep_dnorm: y[26], y[25], y[24], y[23], y[22], y[21], y[20], y[19], y[18], y[17], y[16]
## [20] conjugate_dnorm_dnorm sampler: r[16],  dep_dnorm: y[26], y[25], y[24], y[23], y[22], y[21], y[20], y[19], y[18], y[17]
## [21] conjugate_dnorm_dnorm sampler: r[17],  dep_dnorm: y[26], y[25], y[24], y[23], y[22], y[21], y[20], y[19], y[18]
## [22] conjugate_dnorm_dnorm sampler: r[18],  dep_dnorm: y[26], y[25], y[24], y[23], y[22], y[21], y[20], y[19]
## [23] conjugate_dnorm_dnorm sampler: r[19],  dep_dnorm: y[26], y[25], y[24], y[23], y[22], y[21], y[20]
## [24] conjugate_dnorm_dnorm sampler: r[20],  dep_dnorm: y[26], y[25], y[24], y[23], y[22], y[21]
## [25] conjugate_dnorm_dnorm sampler: r[21],  dep_dnorm: y[26], y[25], y[24], y[23], y[22]
## [26] conjugate_dnorm_dnorm sampler: r[22],  dep_dnorm: y[26], y[25], y[24], y[23]
## [27] conjugate_dnorm_dnorm sampler: r[23],  dep_dnorm: y[26], y[25], y[24]
## [28] conjugate_dnorm_dnorm sampler: r[24],  dep_dnorm: y[26], y[25]
## [29] conjugate_dnorm_dnorm sampler: r[25],  dep_dnorm: y[26]
## [30] posterior_predictive sampler: y[21]
## [31] posterior_predictive sampler: y[22]
## [32] posterior_predictive sampler: y[23]
## [33] posterior_predictive sampler: y[24]
## [34] posterior_predictive sampler: y[25]
## [35] posterior_predictive sampler: y[26]
```

This list reveals that this model is not written well for efficient
computation.  Every time time the MCMC considers a new value for
`r[t]`, the value of `logN.est` and the probability density of
`y` must be calculated for every value of `t` until the end of the
data.  We'll reconsider that later, but it's tangential to our current
purpose of illustrating MCMC.

We'll see how to modify sampler assignments later.  

You can also inspect the monitors, which is the vector of nodes that will be
recorded in output.


```r
mcmcConf$getMonitors()
```

```
## thin = 1: logN.est, mean.r, sigma.proc, sigma.obs
```

If we wanted to add or remove variables from monitors, we could use
`mcmcConf$addMonitors()`.

# Building, compiling and running the MCMC



```r
ssmMCMC <- buildMCMC(mcmcConf)
CssmMCMC <- compileNimble(ssmMCMC, project = ssm)
```

Now let's run the MCMC.  *Both the uncompiled and compiled versions
can be run*, but the uncompiled version will be really slow.  Its
purpose is to allow debugging of algorithms from R.  (They will usually
create exactly the same samples if you set the random number seed to
the same value before each.)

Let's run each for 10 iterations and time them:

```r
set.seed(0)
print(system.time(ssmMCMC$run(10)))  # uncompiled version
```

```
##    user  system elapsed 
##   8.862   0.008   8.866
```

```r
set.seed(0)
print(system.time(CssmMCMC$run(10)))   # compiled version
```

```
##    user  system elapsed 
##   0.002   0.000   0.001
```

In this case the uncompiled MCMC generates a set of warnings
(suppressed in these slides).   These arise from the
Metropolis-Hastings algorithm proposing negative values for standard
deviations, which create NaNs in `dnorm` calculations.  These are not
a problem and could be removed by using a reflected sampler, a sampler
on a log scale, or other customization.

But now let's run the compiled version for much longer:

```r
print(system.time(CssmMCMC$run(10000)))
```

```
##    user  system elapsed 
##   1.277   0.000   1.276
```

# Extracting MCMC output

The samples are saved inside of the `CssmMCMC` object and must be
extracted and converted to a matrix.


```r
samples <- as.matrix(CssmMCMC$mvSamples)
```

Now let's look at a few parameters


```r
tsplot <- function(x, ...) plot(seq_along(x), x, type = 'l', ...)

par(mfrow = c(1, 3), mai = c(.6, .5, .1, .2))
tsplot(samples[ , 'mean.r'], xlab = 'iteration',
     ylab = 'mean.r', main = 'mean.r')
tsplot(samples[ , 'sigma.proc'], xlab = 'iteration',
     ylab = 'sigma.proc', main = 'sigma.proc')
tsplot(samples[ , 'sigma.obs'], xlab = 'iteration',
     ylab = 'sigma.obs', main = 'sigma.obs')
```

![](figure/output-mcmc-1.png)

### Using CODA

NIMBLE does not provide any MCMC diagnostics, but one can easily use CODA or other R packages with the MCMC output from a NIMBLE MCMC.


```r
library(coda)
```

```
## 
## Attaching package: 'coda'
```

```
## The following object is masked _by_ '.GlobalEnv':
## 
##     densplot
```

```r
burnin <- 100
mcmc <- as.mcmc(samples[(burnin+1):nrow(samples), ])
crosscorr(mcmc[ , c('mean.r', 'sigma.proc', 'sigma.obs')])
```

```
##                mean.r sigma.proc  sigma.obs
## mean.r      1.0000000 -0.1412297  0.1973835
## sigma.proc -0.1412297  1.0000000 -0.6360679
## sigma.obs   0.1973835 -0.6360679  1.0000000
```

```r
effectiveSize(mcmc)
```

```
##  logN.est[1]  logN.est[2]  logN.est[3]  logN.est[4]  logN.est[5] 
##    390.53122    318.47653    122.84224     49.01156    213.22428 
##  logN.est[6]  logN.est[7]  logN.est[8]  logN.est[9] logN.est[10] 
##    175.78757    234.58526    659.83796     93.88538    558.21367 
## logN.est[11] logN.est[12] logN.est[13] logN.est[14] logN.est[15] 
##    289.54724    699.10900    265.70831    242.92228    194.98405 
## logN.est[16] logN.est[17] logN.est[18] logN.est[19] logN.est[20] 
##    275.06139     49.31659   1082.94881   1418.32526    262.10533 
## logN.est[21] logN.est[22] logN.est[23] logN.est[24] logN.est[25] 
##    145.04119    108.89966     75.13492     68.84929     58.14737 
## logN.est[26]       mean.r    sigma.obs   sigma.proc 
##     55.84732    303.05165     22.48641     40.72500
```

One could apply the commonly used Gelman-Rubin potential scale
reduction factor diagnostic (which requires multiple chains).

NIMBLE does not automate running multiple chains, but one can easily
write a for-loop to do so.





