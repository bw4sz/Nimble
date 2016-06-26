## ---- markov-exer, eval=FALSE--------------------------------------------
## set.seed(0)
## n <- 1e6
## path <- rep(0, n)
## rho1 <- .8
## rho2 <- .1
## path[1:2] <- rnorm(2)
## print(system.time(
## for(i in 3:n)
##       path[i] <- rho1*path[i-1] + rho2*path[i-2] + rnorm(1)
## ))
## nplot <- 5000
## plot(seq_len(nplot), path[seq_len(nplot)], type = 'l', xlab = 'time')

## ---- markov-exer-scaffold, eval=FALSE-----------------------------------
## mc <- nimbleFunction(
##    run = function( ... ) ) {
##        returnType( ... )
##        ...
##        return(...)
## })
## cmc <- compileNimble(mc)
## set.seed(0)
## system.time(path <- cmc(n, rho1, rho2))

## ---- solution, eval=FALSE-----------------------------------------------
## set.seed(0)
## n <- 1e6
## path <- rep(0, n)
## rho1 <- .8
## rho2 <- .1
## path[1:2] <- rnorm(2)
## print(system.time(
## for(i in 3:n)
##       path[i] <- rho1*path[i-1] + rho2*path[i-2] + rnorm(1)
## ))
## nplot <- 5000
## plot(seq_len(nplot), path[seq_len(nplot)], type = 'l', xlab = 'time')
## 
## 
## library(nimble)
## mc <- nimbleFunction(
##    run = function(n = double(0), rho1 = double(0), rho2 = double(0)) {
##        returnType(double(1))
##        path <- numeric(n, init = FALSE)
##        path[1] <- rnorm(1)
##        path[2] <- rnorm(1)
##        for(i in 3:n)
##              path[i] <- rho1*path[i-1] + rho2*path[i-2] + rnorm(1)
##        return(path)
## })
## cmc <- compileNimble(mc)
## set.seed(0)
## system.time(path <- cmc(n, rho1, rho2))

