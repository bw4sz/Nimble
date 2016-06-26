## ----loadnimble, include=FALSE-------------------------------------------
library(nimble)
library(igraph)

## ----chunksetup, include=FALSE-------------------------------------------
# Following code is only needed for slide generation, not for using R code separately.
library(methods)
read_chunk('chunks_ssm.R')

## ---- ssmSmall-code------------------------------------------------------

## ---- ssmSmall-model-----------------------------------------------------

## ---- echo=FALSE---------------------------------------------------------
drawGraph <- function(model, colorBy = "none", colors = c('salmon','cyan','plum')) {
    graph <- model$getGraph()
    numNodes <- length(ssmSmall$getNodeNames())
    vertex.color <- rep('grey', numNodes)
    if(identical(colorBy, "none")) {}
    else if(length(colorBy) > 1) {
        if(is.character(colorBy))
            colorBy <- model$expandNodeNames(colorBy, returnType = "ids")
        vertex.color[colorBy] <- colors[1]
    } else if(colorBy == "stochDetermData") {
        stochIDs <- ssmSmall$getNodeNames(stochOnly = TRUE, returnType = "ids")
        determIDs <- ssmSmall$getNodeNames(determOnly = TRUE, returnType = "ids")
        dataIDs <- ssmSmall$getNodeNames(dataOnly = TRUE, returnType = "ids")
        vertex.color[stochIDs] <- colors[1]
        vertex.color[determIDs] <- colors[2]
        vertex.color[dataIDs] <- colors[3]
    } else if(colorBy == "topLatentEnd") {
        topIDs <- ssmSmall$getNodeNames(topOnly=TRUE, returnType = "ids")
        latentIDs <- ssmSmall$getNodeNames(latentOnly=TRUE, returnType = "ids")
        endIDs <- ssmSmall$getNodeNames(endOnly=TRUE, returnType = "ids")
        vertex.color[topIDs] <- colors[1]
        vertex.color[latentIDs] <- colors[2]
        vertex.color[endIDs] <- colors[3]
    }
    plot(graph, layout = layout_(graph, with_kk()),
         vertex.color = vertex.color ) ## uses plot.igraph
}

## ---- deps-graph, fig.cap=""---------------------------------------------
## function drawGraph is defined in the R file for this module
drawGraph(ssmSmall, colorBy = ssmSmall$getDependencies('r[3]'))

## ------------------------------------------------------------------------
ssmSmall$getDependencies('r[3]')

## ---- echo=FALSE, fig.cap=""---------------------------------------------
drawGraph(ssmSmall, ssmSmall$getDependencies('r[3]'))

## ------------------------------------------------------------------------
ssmSmall$getDependencies('r[3]', determOnly = TRUE)

## ---- echo=FALSE, fig.cap=""---------------------------------------------
drawGraph(ssmSmall, ssmSmall$getDependencies('r[3]', determOnly = TRUE))

## ------------------------------------------------------------------------
args(ssmSmall$getDependencies)
## or help(modelBaseClass)

## ------------------------------------------------------------------------
ssmSmall$getNodeNames()

## ------------------------------------------------------------------------
ssmSmall$getNodeNames(stochOnly = TRUE)   #salmon
ssmSmall$getNodeNames(determOnly = TRUE)  #cyan
ssmSmall$getNodeNames(dataOnly = TRUE)    #plum

## ---- fig.cap=""---------------------------------------------------------
drawGraph(ssmSmall, 'stochDetermData')

## ------------------------------------------------------------------------
ssmSmall$getNodeNames(topOnly = TRUE)   #salmon
ssmSmall$getNodeNames(latentOnly = TRUE)  #cyan
ssmSmall$getNodeNames(endOnly = TRUE)    #plum

## ---- fig.cap=""---------------------------------------------------------
drawGraph(ssmSmall, 'topLatentEnd')

## ------------------------------------------------------------------------
ssmSmall$expandNodeNames('logN.est[2:5]')

## ------------------------------------------------------------------------
ssmSmall$topologicallySortNodes(c('logN.est[5]', 'r[4]', 'logN.est[3]', 'y[2]'))

## ---- fig.cap=""---------------------------------------------------------
m1 <- nimbleModel(
    nimbleCode({
        tau ~ dunif(0, 100)
        x ~ dnorm(0, tau) #by default, tau is a precision
    }))
plot(m1$getGraph())
m1$getNodeNames()

## ---- eval = FALSE-------------------------------------------------------
## nimbleCode({
##     tau ~ dunif(0, 100)
##     lifted_d1_over_sqrt_oPtau_cP <- 1/sqrt(tau)
##     x ~ dnorm(0, sd = lifted_d1_over_sqrt_oPtau_cP) #by default, tau is a precision
## }))

## ---- eval = FALSE-------------------------------------------------------
## m1$calculate(c('tau','x')) ## Wrong: the lifted node is being neglected

## ---- eval = FALSE-------------------------------------------------------
## m1$calculate( m1$getDependencies('tau') )

## ---- fig.cap=""---------------------------------------------------------
m2 <- nimbleModel(
    nimbleCode({
        a ~ dnorm(0, 1)
        b ~ dnorm(a + 1, 1)
    }))
plot(m2$getGraph())
m2$getNodeNames()

## ------------------------------------------------------------------------
ssmSmall$getVarNames()
ssmSmall$getVarInfo('r')

