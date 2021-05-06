library(ergm)
#ninety percent of social scientists are using this model (exponential random graph model) ERGM
#gravity model can be used as well, it can be used to explain trade flows between countries
#latent space model is more about to deduce the relative distance, it is not suggested to start from this model

#ERGM also known as p* model, assumes that networks are created in a random way (which naturally it is not) so it studies how any
#variables can effect this randomness, some of the problems with the model is that it can be difficult to calculate



set.seed(0)

data(package = "ergm") 

## ------------------------------------------------------------------------
data(florentine) # loads flomarriage and flobusiness data
flomarriage # Let's look at the flomarriage network properties

par(mfrow=c(1,2)) # Setup a 2 panel plot (for later)
plot(flomarriage, main="Florentine Marriage", cex.main=0.8) # Plot the flomarriage network
summary(flomarriage~edges) # Look at the $g(y)$ statistic for this model
summary(flomarriage~edges + triangle) #gives us further information on transitivity
summary(flomarriage~edges + triangle + degree(1)) # you can add more terms for more details


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
#nodecav is the to measure the "node covariate" basically 
#saying if one node has more of an attribute it is more likely to have more connections
summary(flomodel.03)

flomodel.04 <- ergm(flomarriage~edges+degreepopularity)  
summary(flomodel.04)


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


par(mfrow = c(1,3))
plot(samplk1)
plot(samplk1)
plot(samplk1)

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

cp18 
V(cp18)

cp18_bx <- delete_vertices(cp18, v= c("UK"))  
V(cp18); V(cp18_bx)


# Plot the two networks
par(mfrow = c(1,2))
plot(cp18); plot(cp18_bx)

# load network statistics

node.df <- read.csv('node_statistics.csv')

# check if names match

V(cp18_bx)$name
node.df$cty

V(cp18_bx)$name == node.df$cty  #checking if the names match

# calculate expected changes based on 2015 data

names(node.df)
V(cp18_bx)$d_degree <- node.df$dgr_post-node.df$dgr_pre
V(cp18_bx)$d_degree

V(cp18_bx)$d_close <- node.df$cls_post-node.df$cls_pre
V(cp18_bx)$d_close

V(cp18_bx)$d_between <- node.df$btw_post-node.df$btw_pre
V(cp18_bx)$d_between

cp18_bx

# convert data into ergem object
install.packages("intergraph")
library(intergraph)
cp18.eg <- asNetwork(cp18_bx)
class(cp18.eg)
cp18.eg


# run ergm model

library(ergm)

plot(cp18.eg)

fit0 <- ergm(cp18.eg ~ edges)
summary(fit0)

fit1 <- ergm(cp18.eg ~ edges + triangle)
summary(fit1)
