# install.packages("igraph")
# install.packages("igraphdata")
# install.packages("sand")

# install.packages("network")
# install.packages("sna")
# install.packages("ergm")
# install.packages("tergm")


###################################################################################################
#                                                                                                 #
# A Quick R Tour                                                                                  #
#                                                                                                 #
###################################################################################################

# Assignment #--------------------------------------------#
x <- 3                                                    # Assignment
x                                                         # Evaluate the expression and print result

y <- 4                                                    # Assignment
y + 5                                                     # Evaluation, y remains 4

z <- x + 23*y                                             # Assignment
z                                                         # Evaluation

# NA for missing or undefined data

# Scalar, Vector and Matrix #-----------------------------#

v0 <- 5
length(v0)
dim(v0)

v1_n <- c(1, 5, 11, 33)                                   # Numeric vector, length 4
v2_n <- c("hello","world")                                # Character vector, length 2 (a vector of strings)
v3_n <- c(TRUE, TRUE, FALSE)                              # Logical vector, same as c(T, T, F)
length(v1_n)
dim(v1_n)

m <- matrix(1:10,10,10)
m[2,3]                                                    # Matrix m, row 2, column 3 - a single cell
m[2,]                                                     # The whole second row of m as a vector
m[,2]                                                     # The whole second column of m as a vector
m[1:2,4:6]                                                # submatrix: rows 1 and 2, columns 4, 5 and 6
m[-1,]                                                    # all rows *except* the first one
dim(m)

# Dataframe #---------------------------------------------#

dfr1 <- data.frame(ID = 1:4,
                   FirstName = c("John", "Jim", "Jane", "Jill"),
                   Female = c(F, F,T, T), 
                   Age = c(22,33,44,55))
dfr1

rm(list = ls())


###################################################################################################
#                                                                                                 #
# Project                                                                                         #
#                                                                                                 #
###################################################################################################

getwd()
# setwd()

brexit.df <- data.frame(read.csv("/Users/narisonghuhe/Desktop/SNA Workshop/brexit.csv",
                                 header = T))

# RStudio IDE

# lm
# glm
# ?glm

library(igraph)


###################################################################################################
#                                                                                                 #
# Network Data                                                                                    #
#                                                                                                 #
###################################################################################################

# Creating Netwok Data #----------------------------------#

g1 <- graph(edges = c(1,2,
                      2,3,
                      3,1),
            n = 3, directed = F) 
plot(g1)                                                  # A simple plot of the network

class(g1)
g1

g2 <- graph(edges = c(1,2,
                      2,3,
                      3,1),
            n = 12)
plot(g2)  

g2

# A network of 3 -> 5, 6 -> 2, 1 -> 2, 1 -> 6, 5 -> 6


g3 <- graph(c("Spain", "UK",
              "UK", "France",
              "France", "Spain"))                         # named vertices
# When the edge list has vertex names, the number of nodes is not needed
plot(g3)

g3

g4 <- graph(c("Spain", "UK",
              "UK", "France",
              "France", "Spain"),
            isolates = c("Sweden", "Malta", "Greece", "Finland") )  
# In named graphs we can specify isolates by providing a list of their names.
plot(g4)


plot(g4,
     edge.arrow.size = .5,
     vertex.color = "red",
     vertex.size = 5, 
     vertex.frame.color = "gray",
     vertex.label.color = "black", 
     vertex.label.cex = 1,
     vertex.label.dist = 2,
     edge.curved = .2)

# Graph Model #-------------------------------------------#

fg <- make_full_graph(40)
plot(fg, vertex.size=10, vertex.label=NA)

st <- make_star(40)
plot(st, vertex.size=10, vertex.label=NA)

tr <- make_tree(40, children = 3, mode = "undirected")
plot(tr, vertex.size=10, vertex.label=NA) 

rn <- make_ring(40)
plot(rn, vertex.size=10, vertex.label=NA)

er <- sample_gnm(n=100, m=40) 
plot(er, vertex.size=6, vertex.label=NA)

sw <- sample_smallworld(dim=2, size=10, nei=1, p=0.1)
plot(sw, vertex.size=6, vertex.label=NA, layout=layout_in_circle)

ba <-  sample_pa(n=100, power=1, m=1,  directed=F)
plot(ba, vertex.size=6, vertex.label=NA)

zach <- graph("Zachary")                                  # the Zachary carate club
plot(zach, vertex.size=10, vertex.label=NA)

?make_graph()


###################################################################################################
#                                                                                                 #
# Brexit Data and Network layouts                                                                 #
#                                                                                                 #
###################################################################################################

rm(list = ls())

load("C:/Users/huhen/Google Cloud/[Lyx]/Network Analysis Tutorial/COREPER15.rda")

class(cp15)
cp15
V(cp15)
V(cp15)$type <- c(rep(c(1, 2, 3, 4), n = 7))

plot(cp15)
plot(cp15, edge.arrow.size = .4, edge.curved = .1)

# Network layout #----------------------------------------#
plot(cp15, layout = layout_randomly)

l <- layout_in_circle(cp15)
plot(cp15, layout = l)

l <- cbind(1:vcount(cp15), c(1, vcount(cp15):2))
plot(cp15, layout=l)

l <- layout_randomly(cp15)
plot(cp15, layout=l)

l <- layout_in_circle(cp15)
plot(cp15, layout=l)

l <- layout_on_sphere(cp15)
plot(cp15, layout=l)

l <- layout_with_fr(cp15)
plot(cp15, layout=l)

par(mfrow=c(2,2), mar=c(0,0,0,0))                         # plot four figures - 2 rows, 2 columns
plot(cp15, layout=layout_with_fr)
plot(cp15, layout=layout_with_fr)
plot(cp15, layout=l)
plot(cp15, layout=l)
dev.off()

l <- layout_with_kk(cp15)
plot(cp15, layout=l)

plot(cp15, layout=layout_with_lgl)

layouts <- grep("^layout_", ls("package:igraph"), value=TRUE)[-1] 
layouts <- layouts[!grepl("bipartite|merge|norm|sugiyama|tree", layouts)]
par(mfrow=c(3,5), mar=c(1,1,1,1))
for (layout in layouts) {
  print(layout)
  l <- do.call(layout, list(cp15)) 
  plot(cp15, edge.arrow.mode=0, layout=l, main=layout) }

rm(list = ls())


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

