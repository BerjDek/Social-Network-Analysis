library(ergm)


set.seed(0)

data(package = "ergm") 

## ------------------------------------------------------------------------
data(florentine) # loads flomarriage and flobusiness data
flomarriage # Let's look at the flomarriage network properties

par(mfrow=c(1,2)) # Setup a 2 panel plot (for later)
plot(flomarriage, main="Florentine Marriage", cex.main=0.8) # Plot the flomarriage network
summary(flomarriage~edges) # Look at the $g(y)$ statistic for this model

flomodel.01 <- ergm(flomarriage~edges) # Estimate the model 
summary(flomodel.01) # The fitted model object

## ------------------------------------------------------------------------
summary(flomarriage~edges+triangle) # Look at the g(y) stats for this model
flomodel.02 <- ergm(flomarriage~edges+triangle) 
summary(flomodel.02)

## ------------------------------------------------------------------------
class(flomodel.02) # this has the class ergm

names(flomodel.02) # the ERGM object contains lots of components.

## ------------------------------------------------------------------------
flomodel.02$coef # you can extract/inspect individual components

## ------------------------------------------------------------------------
wealth <- flomarriage %v% 'wealth' # %v% references vertex attributes
wealth
summary(wealth) # summarize the distribution of wealth
plot(flomarriage, vertex.cex=wealth/25, main="Florentine marriage by wealth", cex.main=0.8) # network plot with vertex size proportional to wealth
summary(flomarriage~edges+nodecov('wealth')) 
# observed statistics for the model
flomodel.03 <- ergm(flomarriage~edges+nodecov('wealth'))
summary(flomodel.03)

## ------------------------------------------------------------------------
data(faux.mesa.high) 
mesa <- faux.mesa.high

## ------------------------------------------------------------------------
mesa
par(mfrow=c(1,1)) # Back to 1-panel plots
plot(mesa, vertex.col='Grade')
legend('bottomleft',fill=7:12,legend=paste('Grade',7:12),cex=0.75)

## ------------------------------------------------------------------------
fauxmodel.01 <- ergm(mesa ~ edges + nodematch('Grade',diff=T) + nodematch('Race',diff=T))
summary(fauxmodel.01)

## ------------------------------------------------------------------------
table(mesa %v% 'Race') # Frequencies of race
mixingmatrix(mesa, "Race")

## ------------------------------------------------------------------------
summary(mesa ~ edges + nodematch('Grade',diff=T) + nodematch('Race',diff=T))


## ------------------------------------------------------------------------
flo.03.gof.model <- gof(flomodel.03 ~ model)
flo.03.gof.model
plot(flo.03.gof.model)

## ------------------------------------------------------------------------
flo.03.gof.global <- gof(flomodel.03 ~ degree + esp + distance)
flo.03.gof.global
plot(flo.03.gof.global)

## ------------------------------------------------------------------------
mesamodel.b <- ergm(mesa~edges)
plot(gof(mesamodel.b ~ model))
plot(gof(mesamodel.b ~ degree + esp + distance))

## ------------------------------------------------------------------------
summary(flobusiness ~ edges+degree(1))
fit <- ergm(flobusiness ~ edges+degree(1))
mcmc.diagnostics(fit)


## ------------------------------------------------------------------------
data('faux.magnolia.high')
magnolia <- faux.magnolia.high
plot(magnolia, vertex.cex=.5)
summary(magnolia ~ edges+triangle)


fit.mag.01 <- ergm(magnolia ~ edges+triangle, control=control.ergm(MCMLE.maxit=2))

mcmc.diagnostics(fit.mag.01)

## ------------------------------------------------------------------------
fit.mag.02 <- ergm(magnolia ~ edges+gwesp(0.25,fixed=T))
mcmc.diagnostics(fit.mag.02)

## ------------------------------------------------------------------------
gof.mag.02.model <- gof(fit.mag.02, GOF = ~model)
gof.mag.02.model
plot(gof.mag.02.model)


rm(list = ls())


###########################################################
# Multiple network cross-sections
###########################################################

library(tergm)

data(samplk)
ls()

samplk1 %v% "cloisterville"

samp.ls <- list()
samp.ls[[1]] <- samplk1
samp.ls[[2]] <- samplk2
samp.ls[[3]] <- samplk3

s1 <- stergm(samp.ls,
             formation=~ edges + mutual + cyclicalties + transitiveties,
             dissolution=~ edges + mutual + cyclicalties + transitiveties,
             estimate="CMLE")
summary(s1)

s2 <- stergm(samp.ls,
             formation=~ edges + mutual + nodecov("cloisterville"),
             dissolution=~ edges,
             estimate="CMLE", times=1:3)
summary(s2)


###########################################################
# Post-Brexit Coreper I
###########################################################

library(igraph)

load("COREPER18.rda")

# Create network without uk

# Plot the two networks

# load network statistics

# check if names match

# calculate expected changes based on 2015 data

# convert data into ergem object

# run ergm model
