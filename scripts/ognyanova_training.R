#### Introduction ####
## This script records my initial attempts to learn network analysis methods in R.
## It is based on the tutorial by Katya Ognyanova - https://kateto.net/network-visualization
# This site is archived here:  https://archive.vn/coMO2

## Header
# Start date: 7 January 2022
# Author: Mark Limb


#### Installing and loading required packages ####
## Installation
# install.packages('tidyverse')
# install.packages('dplyr')
# install.packages('tidygraph')
# install.packages('ggraph')
# install.packages('visNetwork')
# install.packages('networkD3')
# install.packages("igraph")
# install.packages("network")
# install.packages("sna")
# install.packages("threejs")
# install.packages("ndtv")
# install.packages('viridis')

## Loading
library(tidyverse)
library(dplyr)
library(tidygraph)
library(ggraph)
library(visNetwork)
library(networkD3)
library(igraph)
library(network)
library(sna)
library(threejs)
library(ndtv)
library(viridis)

#### Colours ####
## Katya provides some useful tips about plotting colours
# This includes how to choose colours, transparency, grading, and drawing from palettes

# Basic plots with colour
plot(x=1:10, y=rep(5,10), pch=19, cex=3, col="dark red") #10 dots - pch is shape type, cex is size, col is colour
points(x=1:10, y=rep(6, 10), pch=19, cex=3, col="557799") # here col is selected by its hex
points(x=1:10, y=rep(4, 10), pch=19, cex=3, col=rgb(.25, .5, .3)) # here col is from RGB on 0 to 1. While this is the R default, you can also set it to the 0-255 range using something like rgb(10, 100, 100, maxColorValue=255)

plot(x=1:5, y=rep(5,5), pch=19, cex=22, col=rgb(.10, .2, .3, alpha = 0.8), xlim = c(0,6)) #alpha sets transparency, xlim x-axis scale

# Finding colours
colors() # lists the colours by name
grep("blue", colors(), value = TRUE) # shows all the colours with blue in the name

# Colour palettes and ramps
pal1 <- heat.colors(5, alpha=1)   #  5 colors from the base R heat palette, opaque
pal2 <- rainbow(5, alpha=.5)      #  5 colors from the base R heat palette, transparent
pal3 <- cm.colors(5, alpha = .5) # 5 colours from the base r pastel set
pal4 <- viridis(10, alpha = .8) # 10 colours from the viridis package - this gives maxiumum contrast between colours
plot(x=1:10, y=1:10, pch=19, cex=5, col=pal1)
plot(x=1:10, y=1:10, pch=19, cex=5, col=pal2)
plot(x=1:10, y=1:10, pch=19, cex=5, col=pal3)
plot(x=1:10, y=1:10, pch=19, cex=12, col=pal4, xlim = c(0,11), ylim = c(0,11))

 
palf <- colorRampPalette(c("darkred", "gray40")) # This saves as a function. 
palf <- colorRampPalette(c(rgb(1,1,1, 0.5), rgb(.5,0,0, 0.9)), alpha = TRUE) # Transparency needs to be set using RGB with alpha aspect, then alpha set true (see the tutorial for deets)
plot(x=1:10, y=1:10, pch=19, cex=10, col=palf(10), xlim = c(0,11), ylim = c(0,11)) # calling the function then specifies the number of grades between the two colours

#### Network data prep 1 ####
# Read in data
nodes <- read.csv("data/Dataset1-Media-Example-NODES.csv", header=T, as.is=T)
links <- read.csv("data/Dataset1-Media-Example-EDGES.csv", header=T, as.is=T)

# this data is similar to what we say in the Sadler tutorial consisting of nodes and edges data. Edges are named links here.
# But same concept - links show the unique ids and to and from. It also contains a weighting and type field
# The nodes data contains metadata for each ID... so the mdeia source, it's type as code and label, and its audience size.
links
nodes

## igraph prep
# Katya uses the igraph package here, but this should work for tidygraph+ggraph too I imagine

# Creating an igraph object
# Uses the graph_from_data_frame() function, which takes two data frames: d and vertices.
net <- graph_from_data_frame(d=links, vertices = nodes, directed = TRUE)

# From this, can call out specific elements using E() and V() commands
E(net) # shows the edges
V(net) # shows the nodes
E(net)$type # shows the field type from the edges data
V(net)$media # shows the field media from the nodes data

# Can change the igraph format into other handy formats
as_edgelist(net, names = TRUE) # back to an edge list
as_adjacency_matrix(net, attr="weight") # to an adjacency_mattrix

#### Plotting network 1 data ####
# Basic plot - pretty messy
plot(net)

# The simplify function can tidy things up a little
net <- simplify(net, remove.multiple = FALSE, remove.loops = TRUE) # this removes the loops from the links
plot(net, edge.arrow.size=.4,vertex.label=NA) # reduces arrow size and removes labels from nodes

#### Network data prep 2 ####
## Two-mode (bipartite) networks in igraph
# Two-mode or bipartite graphs have two different types of actors and links that go across, but not within each type. 
# This is slightly different as it containts contacts between media and audience.
# Nodes are similar concept but links is a non-squared adjacency matrix without weighting

# Read in data
nodes2 <- read.csv("data/Dataset2-Media-User-Example-NODES.csv", header=T, as.is=T)
links2 <- read.csv("data/Dataset2-Media-User-Example-EDGES.csv", header=T, row.names=1)

links2 <- as.matrix(links2) # converts to matrix

# creates igraph object from the matrix
net2 <- graph_from_incidence_matrix(links2)

#### igraph Plotting in detail ####
?igraph.plotting # gives full list of the parameteres than can be used in the igraph plots
# Katya's tutorial in Section for starst with a helpful list of common params for this

### Node & edge options can be set in two ways
## 1. Specify them in the plot() function, as we are doing below.
plot(net, edge.arrow.size = 0.4, edge.curved=0.2) # curve value - higher = more curve

# this changes the node color, it's outside edge, labels it using the node data, and sets a label colour
plot(net, edge.arrow.size = 0.3, edge.color="orange",
     vertex.color="orange", vertex.frame.color="darkred", vertex.label=V(net)$media,
     vertex.label.color="black")


## 2. Can add the attributes to the igraph object itself instead of using the plot function
# This can be helpful for coloring aspects of the network like frequencies, node types etc.

# Colouring based on node details
colrs <- c("gray50", "tomato", "gold")
V(net)$color <- colrs[V(net)$media.type] # assigns the colours to a new field in igraph object

# Node size based on the number of links to each node
deg <- igraph::degree(net, mode = "all") # note - there are conflicting packages with the degree command. This specifies to use igraph package
V(net)$size <- deg*3 # adds a new field with the number of nodes (times three to make improve size difference)

# Can remove labels by setting a label field and NA it out
V(net)$label <- NA

# Setting an edge width based on the weight
E(net)$width <- E(net)$weight/6

# Changing arrow size and line colour
E(net)$arrow.size <- 0.2
E(net)$edge.color <- "gray50"

# Setting network layout
graph_attr(net, "layout") <- layout_with_lgl

# finally, plotting all the above
plot(net)

# This can all be over ridden by the standard plot command
plot(net, edge.color="darkred", vertex.color="cornflowerblue")

# Adding a legend
plot(net)
legend(x=-1.5, y=-1.1, c("Newspaper","Television", "Online News"), pch=21,
       col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)

# Or plotting out label only with no nodes
plot(net, vertex.shape="none", vertex.label=V(net)$media, 
     vertex.label.font=2, vertex.label.color="gray40",
     vertex.label.cex=.7, edge.color="gray85")

# Can change edge colours based on their sources
edge.start <- ends(net, es=E(net), names=F)[,1] # The ends function returns start and end of each edge
edge.col <- V(net)$color[edge.start] # applies the previous color scheme to the edge

plot(net, edge.color=edge.col, edge.curved=.1)

#### Network layouts ####
## Network layouts offer different ways of laying out the coordinates of points
net.bg <- sample_pa(100) # creates a random series of points and connections in the igraph class
V(net.bg)$size <- 8
V(net.bg)$frame.color <- "white"
V(net.bg)$color <- "orange"
V(net.bg)$label <- ""
E(net.bg)$arrow.mode <- 0
plot(net.bg)

# The Layout can be set in the plot command
plot(net.bg, layout = layout_randomly)

# Or the layout can be set to an object and then inserted
l <- layout_in_circle(net.bg)
plot(net.bg, layout = l)

# There are several built in layouts
l <- layout_on_sphere(net.bg)
plot(net.bg, layout = l)

# Fruchterman-Reingold
# This is a force directed layout. So nodes repel, and edges draw together to create logical clusterings based on connections
# These forces can be adjusted. See Katya's document for more info (section 4.2 - https://kateto.net/network-visualization)
l <- layout_with_fr(net.bg, niter=50) # The niter command sets the number of iterations attempted to get the desired shape. Lower is faster. Default is 500
plot(net.bg, layout = l)

# The layout will change everytime unless assigned to object like we did before
# The following panel demonstrates this... 2 x layout set in plot, and 2 x layout set as an object. The last two are the same layouts.
par(mfrow=c(2,2), mar=c(0,0,0,0))   # plot four figures - 2 rows, 2 columns
plot(net.bg, layout=layout_with_fr)
plot(net.bg, layout=layout_with_fr)
plot(net.bg, layout=l)
plot(net.bg, layout=l)

dev.off() # stops plotting to the panel


