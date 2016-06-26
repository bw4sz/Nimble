---
title: "Building a model (state-space model example)"
subtitle: "NIMBLE training materials module"
author: "NIMBLE Development Team"
output:
  html_document:
    code_folding: show
---




# A basic example: population state-space model

We'll use an example from Chapter 5 of K&eacute;ry and Schaub
(2012)\footnote{Marc K&eacute;ry  and Michael Schaub. 2012. Bayesian Population Analysis Using WinBUGS: A hierarchical perspective.  Elsevier / Academic Press.}

Code and data are available from the [BPA web site](http://www.vogelwarte.ch/de/projekte/publikationen/bpa/complete-code-and-data-files-of-the-book.html), in bpa-code.txt.

### Summary

   - House martin counts (1990 - 2009) by Reto Freuler
   - Exponential population model with population growth rate varying each year
   - Log-normal observations of population size.
   - Parameters to estimate: mean and variance of population growth rate; observation variance.
   - Latent states: True population size in each year.
   - The example also generates predictions through 2015.

### BUGS \& NIMBLE code for the model

This has been simplified (without changing the model) from the book's code files.


```r
library(nimble)
ssmCode <- nimbleCode({
        # Priors and constraints
        logN.est[1] ~ dnorm(5.6, 0.01)  # Prior for initial population size
        mean.r ~ dnorm(1, 0.001)        # Prior for mean growth rate
        sigma.proc ~ dunif(0, 1)        # Prior for sd of state process
        sigma.obs ~ dunif(0, 1)         # Prior for sd of observation process

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

# Building a model in NIMBLE

   - WinBUGS/OpenBUGS/JAGS: provide code, data and (optional) initial values at one time, and the software runs an MCMC.
   - NIMBLE gives more control: data is different from constants and can be set and modified later.  Model is separate from algorithms.

Building the state-space model in one step:

```r
# Code from BPA book:
pyears <- 6 # Number of future years with predictions
hm <- c(271, 261, 309, 318, 231, 216, 208, 226, 195, 226, 233, 209, 
   226, 192, 191, 225, 245, 205, 191, 174, rep(NA, pyears))
year <- 1990:(2009 + pyears)

# Bundle data
bugs.data <- list(y = log(hm), T = length(year))
## NIMBLE will handle y as data, T as a constant

# Initial values
inits <- function(){
      list(sigma.proc = runif(1, 0, 1), mean.r = rnorm(1),
                         sigma.obs = runif(1, 0, 1), 
                         logN.est = c(rnorm(1, 5.6, 0.1), 
                                  rep(NA, (length(year)-1))))
}

ssm <- nimbleModel(ssmCode, constants = bugs.data, 
    inits = inits()) # inits handled as function call here (can be fixed)
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

Note that `nimbleModel` returns an object with which you can program. This will be covered later.

# Compiling a model

In general, you'll want a version of the model that allows for fast computation (this can then be used by any algorithms you use on the model).

To create a fast compiled version of the model, you simply do this.


```r
Cssm <- compileNimble(ssm)
```

# Looking at the model's graph

A central concept for hierarchical statistical modeling is:

   - Think of the model as a graph, typically a directed acyclic graph
   (DAG).
   - Every line of BUGS code declares a node on the graph.

We can harness NIMBLE's use of the `igraph` library to plot the graph.


```r
## Remove N.est[t] to reduce clutter 
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
          for (t in 1:T) y[t] ~ dnorm(logN.est[t], sd = sigma.obs)
})

# Make a smaller version so the graph wil be readable
ssmSmall <- nimbleModel(ssmCode, constants = list(T = 5),
                        data = list(y = bugs.data$y[1:5]))
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
## NAs were detected in model variables: logN.est, logProb_logN.est, mean.r, logProb_mean.r, sigma.proc, logProb_sigma.proc, sigma.obs, logProb_sigma.obs, r, logProb_r, logProb_y.
```

```
## model building finished
```

```r
library(igraph)
```

```
## 
## Attaching package: 'igraph'
```

```
## The following object is masked _by_ '.GlobalEnv':
## 
##     sizes
```

```
## The following objects are masked from 'package:stats':
## 
##     decompose, spectrum
```

```
## The following object is masked from 'package:base':
## 
##     union
```

```r
graph <- ssmSmall$getGraph()
plot(graph, layout = layout_(graph, with_kk())) ## uses plot.igraph
```

![](figure/plot-graph-1.png)

`igraph`'s plot function doesn't know how to lay out a state-space model
but this looks somewhat ok.
