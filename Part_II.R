
library(igraph)

###################################################################################################
#                                                                                                 #
# Visualizing Network Data                                                                        #
#                                                                                                 #
###################################################################################################

# Example 1: Karate Club #--------------------------------#

data(karate, package = "igraphdata")
help(karate, package = "igraphdata")

karate
class(karate)
V(karate)
E(karate)

karate$Citation
V(karate)$Faction
E(karate)$weight

set.seed(1019)                                             # Reproducible layout
l <- layout.kamada.kawai(karate)
l

par(mfrow=c(1,1))
plot(karate, layout = layout_nicely(karate), vertex.label = NA)                # Plot undecorated first.

V(karate)$label <- sub("Actor ", "", V(karate)$name)       # Now decorate, starting with labels.
V(karate)$shape <- "circle"                                # Two leaders get shapes different from 
V(karate)[c("Mr Hi", "John A")]$shape <- "rectangle"       # club members.

V(karate)[Faction == 1]$color <- "red"                     # Differentiate two factions by color.
V(karate)[Faction == 2]$color <- "dodgerblue"

V(karate)$size <- 4*sqrt(graph.strength(karate))           # Vertex area proportional to vertex,
V(karate)$size2 <- V(karate)$size * .5                     # i.e., total weight of incident edges.

E(karate)$width <- E(karate)$weight                        # Weight edges by number of common 
                                                           # activities
F1 <- V(karate)[Faction==1]                                # Color edges by within/between faction.
F2 <- V(karate)[Faction==2]
E(karate)[ F1 %--% F1 ]$color <- "pink"
E(karate)[ F2 %--% F2 ]$color <- "lightblue"
E(karate)[ F1 %--% F2 ]$color <- "yellow"

V(karate)$label.dist <-                                    # Offset vertex labels for smaller
  ifelse(V(karate)$size >= 10, 0, 0.75)                    # points (default=0).

plot(karate, layout = l)                                   # Plot decorated graph.

# k.nbhds <- graph.neighborhood(karate, order = 1)
# sapply(k.nbhds, vcount)
# 
# k.1 <- k.nbhds[[1]]
# k.34 <- k.nbhds[[34]]
# par(mfrow=c(1,2))
# plot(k.1, vertex.label=NA,
#      vertex.color=c("red", rep("lightblue", 16)))
# plot(k.34, vertex.label=NA,
#      vertex.color=c(rep("lightblue", 17), "red"))

# Example 2: Partnership #--------------------------------#

help(lazega, package = "sand")
data(lazega, package = "sand")
lazega

lazega <- upgrade_graph(lazega)
lazega

colbar <- c("red", "dodgerblue", "goldenrod")              # Office location indicated by color.
v.colors <- colbar[V(lazega)$Office]

v.shapes <- c("circle", "square")[V(lazega)$Practice]      # Type of practice indicated by vertex
                                                           # shape.
v.size <- 3.5*sqrt(V(lazega)$Years)                        # Vertex size proportional to years with
                                                           # firm.
set.seed(42)
l <- layout.fruchterman.reingold(lazega)

plot(lazega, layout = l, vertex.color = v.colors,
     vertex.shape = v.shapes, vertex.size = v.size)


# Example 3: Blog (Layout) #------------------------------#

help(fblog, package = "sand")
data(fblog, package = "sand")

fblog <- upgrade_graph(fblog)

party.names <- sort(unique(V(fblog)$PolParty))
party.names

set.seed(42)
l <- layout.kamada.kawai(fblog)
party.nums.f <- as.factor(V(fblog)$PolParty)
party.nums <- as.numeric(party.nums.f)
plot(fblog, layout = l, vertex.label = NA,
     vertex.color = party.nums, vertex.size = 3)

set.seed(42)
l <- layout.drl(fblog)
plot(fblog, layout = l, vertex.size = 5, vertex.label = NA,
     vertex.color = party.nums)

fblog.c <- contract.vertices(fblog, party.nums)
E(fblog.c)$weight <- 1
fblog.c <- simplify(fblog.c)

party.size <- as.vector(table(V(fblog)$PolParty))
plot(fblog.c, vertex.size = 5*sqrt(party.size),
     vertex.label = party.names,
     vertex.color = V(fblog.c),
     edge.width = sqrt(E(fblog.c)$weight),
     vertex.label.dist = 1.5, edge.arrow.size = 0)


rm(list = ls())

###################################################################################################
#                                                                                                 #
# Descriptive Analysis of Network Graph Characteristics                                           #
#                                                                                                 #
###################################################################################################

library(igraph)

data(karate, package = "igraphdata")
data(yeast, package = "igraphdata")
data(aidsblog, package = "sand")


# Vertex Degree #-----------------------------------------#

karate
degree(karate)
hist(degree(karate), col = "lightblue", xlim = c(0,50),
     xlab = "Vertex Degree", ylab = "Frequency", main = "")


ecount(yeast);
vcount(yeast)
d.yeast <- degree(yeast)
hist(d.yeast,col="blue",
     xlab="Degree", ylab="Frequency",
     main="Degree Distribution")


dd.yeast <- degree.distribution(yeast)
d <- 1:max(d.yeast)-1
ind <- (dd.yeast != 0)
plot(d[ind], dd.yeast[ind], log="xy", col="blue",
     xlab=c("Log-Degree"), ylab=c("Log-Intensity"),
     main="Log-Log Degree Distribution")

# Vertex Centrality #-------------------------------------#

closeness(karate)
hist(closeness(karate), col = "lightgreen", 
     xlab = "Vertex Closeness", main = "")

betweenness(karate)
hist(betweenness(karate, normalized = T), col = "salmon", 
     xlab = "Vertex Betweenness", main = "")

transitivity(karate, type = "local")


# Subgraphs and Censuses #--------------------------------#

table(sapply(cliques(karate), length))

cliques(karate)[sapply(cliques(karate), length) == 5]

table(sapply(maximal.cliques(karate), length))

clique.number(yeast)

cores <- graph.coreness(karate)

# Density and Related Notions of Relative Frequency #-----#

ego.instr <- induced.subgraph(karate,
                              neighborhood(karate, 1, 1)[[1]])
ego.admin <- induced.subgraph(karate,
                              neighborhood(karate, 1, 34)[[1]])
graph.density(karate)
graph.density(ego.instr)
graph.density(ego.admin)

transitivity(karate)
transitivity(karate, "local", vids=c(1,34))

reciprocity(aidsblog, mode = "default")
reciprocity(aidsblog, mode = "ratio")

plot(aidsblog)

# Connectivity, Cuts, and Flows #-------------------------#

is.connected(yeast)

comps <- decompose.graph(yeast)
table(sapply(comps, vcount))

yeast.gc <- decompose.graph(yeast)[[1]]

average.path.length(yeast.gc)

diameter(yeast.gc)   #longest distance between nodes

average.path.length(karate)  #mean distance


# Hierarchical Clustering #-------------------------------#

kc <- fastgreedy.community(karate)
length(kc); sizes(kc)
membership(kc)

plot(kc, karate)

install.packages('ape')
library(ape)

dendPlot(kc, mode = "phylo")


wc <- cluster_walktrap(karate)
membership(wc)

plot(wc, karate)

dendPlot(wc, mode = "phylo")

cb <- cohesive.blocks(karate)  

cb$blocks
cb$block.cohesion

blocks(cb)
cohesion(cb)

plot(cb, karate)

#cohesive block tried to identify the core community, 

rm(list = ls())


###################################################################################################
#                                                                                                 #
# Case Study: Brexit                                                                             #
#                                                                                                 #
###################################################################################################




library(igraph)

load("COREPER15.rda")
load("COREPER18.rda")

par(mfrow = c(1,2))
plot(cp15); plot(cp18)

rm(list = ls())


# Loading data #------------------------------------------#


# read csv edgelist data
raw15 <- read.csv("CP15_edgelist.csv", header = F)
class(raw15)


# convert data into matrix
raw15 <- as.matrix(raw15)   #igraph can only works with matrix'es so we have to convert the dataframe to a matrix
class(raw15)

# create igraph object
# ?graph_from_edgelist
cp15 <- graph_from_edgelist(raw15, directed = F)
plot(cp15)
cp15

cp15_alt <- graph_from_edgelist(
  as.matrix(
    read.csv("CP15_edgelist.csv", header = F
             )
  ),directed = F)                                # same done in a single line

rm(list = ls())


# read csv adjacent matrix data
raw15 <- read.csv("CP15_adj.csv")
class(raw15)

# convert data into matrix
raw15 <- raw15[,-1]  #we need to remove the first column  to make it into a matrix since this data has a column and row names
raw15 <- as.matrix(raw15)
rownames(raw15) <- colnames(raw15)
class(raw15)

# create igraph object
# ?graph_from_adjacency_matrix

cp15 <- graph_from_adjacency_matrix(raw15, mode = 'undirected')

plot(cp15)




rm(list = ls())


# Descriptive statistics #--------------------------------#

load("COREPER15.rda")

# degree

dgr <- degree(cp15)
dgr

# closeness
cls <- closeness(cp15)
cls

# betweenness
btw <- betweenness(cp15)
btw


# transitivity
trn <- transitivity(cp15, type = "local")
trn


# graph density
graph.density(cp15)



# transitivity
transitivity()


# diameter
diameter(cp15)

# average path length
average.path.length(cp15)

network.stat <- data.frame(cbind(dgr,cls,btw,trn))   #made the info into a data frame so its easier to see
write.csv(network.stat, file = "cloudyweathernet.csv")


 # Estimate Brexit impact #------------------------------- #
# to predict what the exit of uk willl do is we create a modified chart where we take out the node representing UK



cp15.bx <- delete_vertices(cp15, v = c("UK"))

par(mfrow = c(1,2))
plot(cp15); plot(cp15.bx)

# calculate changes in vertex changes
