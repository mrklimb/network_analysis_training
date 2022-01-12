#### Introduction ####
## This script records my initial attempts to learn network analysis methods in R.
## It is based on the tutorial by Katya Ognyanova - https://kateto.net/network-visualization
# This site is archived here:  https://archive.vn/coMO2

## Header
# Start date: 7 January 2022
# Author: Mark Limb


#### Installing and loading required packages ####
## Installation
options("install.lock"=FALSE)
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
# install.packages("png")

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
library(png)

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

## Scaling
# igraph plots are set to fill the x,y axis (-1,1) be default. This can be changed with rescale=FALSE
l <- layout_with_fr(net.bg)
l <- norm_coords(l, ymin = -1, ymax = 1, xmin = -1, xmax = 1)

par(mfrow = c(2,2), mar = c(0,0,0,0))
plot(net.bg, rescale = FALSE, layout = l*0.4)
plot(net.bg, rescale = FALSE, layout = l*0.6)
plot(net.bg, rescale = FALSE, layout = l*0.8)
plot(net.bg, rescale = FALSE, layout = l*1.0)
dev.off()

# Apparently they can be 3d too by setting dim to 4... needs x,y,z coords but not sure how that works.
l <- layout_with_fr(net.bg, dim = 3)
plot(net.bg, layout = l)

# Another force-directed layout, Kamada Kawai
l <- layout_with_kk(net.bg)
plot(net.bg, layout = l)

# Graphopt is force directed with some handy cusomtisations
l <- layout_with_graphopt(net.bg)
plot(net.bg, layout = l)

# Can change the charge of nodes and spring of edges
# l1 has stronger charge compared to l2
# also use mass, spring.length, and spring.constant to alter graph
l1 <- layout_with_graphopt(net.bg, charge = 0.2)
l2 <- layout_with_graphopt(net.bg, charge = 0.000001)
par(mfrow = c(1,2), mar=c(1,1,1,1))
plot(net.bg, layout = l1)
plot(net.bg, layout = l2)
dev.off()

# LGL is a layout intended for very large sets, and can specify a central node
plot(net.bg, layout = layout_with_lgl)

# MDS tries to place nodes based on some quality. By defaul it is by shortest path, but can be weighted with param "dist"
plot(net.bg, layout = layout_with_mds)

## Looking at all available igraph layouts
# This first command makes a vector by pulling out only the layout_ layouts form the igraph package.
layouts <- grep("^layout_", ls("package:igraph"), value = TRUE) [-1]
# And this removes those that don't apply to the net.bg object. No idea how it works exactly
layouts <- layouts[!grepl("bipartite|merge|norm|sugiyama|tree", layouts)]

par(mfrow=c(5,3), mar=c(1,1,1,1))
for (layout in layouts) {
        print(layout)
        l <- do.call(layout, list(net))
        plot(net,edge.arrow.mode=0, layout=l, main=layout)
}
# Pretty cool loop to plot!
dev.off()

#### Highlighting parts of the network ####
## With complex networks it is necessary to look beyond size and weight. It might be necessary to break the network down
# Can sparisfy by keeping only some parts of the network - the bits that matter
hist(links$weight)
mean(links$weight)
sd(links$weight)

# Using stats such the above, can delete edges to show anything above or below
cut.off <- mean(links$weight)
net.sp <- delete.edges(net, E(net)[weight<cut.off]) # new object with edges with weight less than mean deleted
plot(net.sp, layout = layout_with_kk)

## highlighting rather than excluding
par(mfrow=c(1,2))
clp <- cluster_optimal(net) #using cluster command to detect communities
class(clp) # It creates a communities object which igraph understands how to plot
plot(clp, net)

# Or plot them by making a new communities field in the igraph object itself
V(net)$community <- clp$membership
colrs <- adjustcolor( c("gray50", "tomato", "gold", "yellowgreen"), alpha=.6)
plot(net, vertex.color=colrs[V(net)$community])

dev.off()

#### Highlights specific nodes ####
## For example could, highlight nodes based on their distance from a particular type of node.
# The distance function reduces a matrix of shortest paths
dist.from.NYT <- distances(net, v=V(net)[media=="NY Times"], to=V(net), weights = NA)
oranges <- colorRampPalette(c("dark red", "gold"))
col <- oranges(max(dist.from.NYT)+1)
col <- col[dist.from.NYT+1]

plot(net, vertex.color = col, vertex.label=dist.from.NYT, edge.arrow.size = 0.6, vertex.label.color = "white")

## Highlight specific paths in the network ##
news.path <- shortest_paths(net, from = V(net)[media=="MSNBC"],
                            to = V(net)[media=="New York Post"],
                            output = "both") # both path nodes and edges

# This sets the others to gray the makes the select path to the orange
ecol <- rep("gray80", ecount(net))
ecol[unlist(news.path$epath)] <- "orange"
# This creates a variable to differentiate by the width of the edge too
ew <- rep(2, ecount(net))
ew[unlist(news.path$epath)] <- 4
# set a node color variable for the different path too
vcol <- rep("gray40", vcount(net))
vcol[unlist(news.path$vpath)] <- "gold"

plot(net, vertex.color = vcol, edge.color = ecol, edge.width = ew, edge.arrow.mode = 0, vertex.label=NA)

## Highlight the edges that go into a particular node using incident or incident_edges for multilple nodes
inc.edges <- incident(net, V(net)[media=="Wall Street Journal"], mode = "all")
# Setting colours
ecol <- rep("gray80", ecount(net))
ecol[inc.edges] <- "orange"
vcol <- rep("gray40", vcount(net))
vcol[V(net)$media=="Wall Street Journal"] <- "gold"

plot (net, vertex.color = vcol, edge.color = ecol, vertex.label=NA, edge.arrow.mode = 0)

## Highlight immediate neighbours
# Neighbors function find adjecent nodes. adjacent_verticies can be used instead for multiple nodes. ego function with order setting distance out
neigh.nodes <- neighbors(net, V(net)[media=="Wall Street Journal"], mode ="out")
# Set the colours
vcol[neigh.nodes] <- "#ff9d00"
plot(net, vertex.color = vcol, vertex.label=NA, edge.arrow.mode = 0)

## Marking groups of nodes or communities ##
par(mfrow=c(1,2))
plot(net, mark.groups = c(1,2,5,8), mark.col = "#C5E5E7", mark.border = NA)
# Marking multiple groups
plot(net, mark.groups = list(c(1,4,5,8), c(4,15:17)), mark.col=c("#C5E5E7", "#ECD89A"), mark.border = NA)

#### Interactive Plotting ####
## This sounds really promising!
## Can plot out an interactive graph, adjust the location of nodes, then save the adjust locations to a layout!!
tkid <- tkplot(net) # this opens the interactive plot from igraph
l <- tkplot.getcoords(tkid) # gets the coordinates from the previous plot
plot(net, layout = l) # plot again using the previously described options

#### Plotting a two-mode network
## This is for the other type of network - it contains two types of nodes, for example to examine links between news sources and consumers
head(nodes2)
head(links2)
# The ID contains S and U types to distinguish. The links shows a contigency matrix between these

net2 # the igraph object created previously
plot(net2, vertex.label = NA)

## Can change the various details to help visualise the difference between the objects
# Setting colours and shapes
V(net2)$color <- c("steel blue", "orange")[V(net2)$type+1]
V(net2)$shape <- c("square", "circle")[V(net2)$type+1]
# Giving media outlets lables, but none for audience
V(net2)$label <- ""
V(net2)$label[V(net2)$type==F] <- nodes2$media[V(net2)$type==F]
V(net2)$label.cex=0.6
V(net2)$label.font=2

plot(net2, vertex.label.color = "white", vertex.size=(2-V(net2)$type)*8)

## There's a special layout for this form of network too
plot(net2, vertex.label = NA, vertex.size=7, layout=layout.bipartite)

## Plotting with nodes as labels is useful way to display this data too
plot(net2, vertex.shape="none", vertex.label=nodes2$media, vertex.label.color=V(net2)$color,
     vertex.label.font=2, vertex.label.cex=0.6, edge.color="gray70", edge.width=2)

## Can also use images as nodes
# needs the png package
img.1 <- readPNG("images/news.png")
img.2 <- readPNG("images/user.png")

V(net2)$raster <- list(img.1, img.2)[V(net2)$type+1]

plot(net2, vertex.shape="raster", vertex.label=NA, vertex.size=16, vertex.size2=16, edge.width=2)

# Can also just add random images
img.3 <- readPNG("images/puppy.png")
rasterImage(img.3, xleft=-1.6, xright=-0.6, ybottom=-1.1, ytop=0.1)

## Breaking out the two networks
net2.bp <- bipartite.projection(net2)

par(mfrow=c(1,2))
plot(net2.bp$proj1, vertex.label.color="black", vertex.label.dist=1, vertex.label=nodes2$media[!is.na(nodes2$media.type)])
plot(net2.bp$proj2, vertex.label.color="black", vertex.label.dist=1, vertex.label=nodes2$media[ is.na(nodes2$media.type)])
dev.off()

#### Plotting multiplex networks ####
E(net)$width <- 1.5
plot(net, edge.color=c("dark red", "slategrey")[(E(net)$type=="hyperlink")+1], vertex.color="gray40", layout=layout_in_circle, edge.curved=.3)

net.m <- net - E(net)[E(net)$type=="hyperlink"] #deletes edges using minus symbol
net.h <- net - E(net)[E(net)$type=="mention"]

# Plotting separately
par(mfrow=c(1,2))
plot(net.h, vertex.color="orange", layout=layout_with_fr, main="Tie: Hyperlink")
plot(net.m, vertex.color="lightsteelblue2", layout=layout_with_fr, main="Tie: Mention")

# By setting a layout upfront, can lock in the same layout across both
l <- layout_with_fr(net)
plot(net.h, vertex.color="orange", layout=l, main="Tie: Hyperlink")
plot(net.m, vertex.color="lightsteelblue2", layout=l, main="Tie: Mention")
dev.off()

## The above is fine when nodes are not connected by each of the types of connector. However in multigraphs this unlikely.
# The trick here is being about the visualuse when the do. The following creates a multigraph to show how to address this
multigtr <- graph(edges = c(1,2, 1,2, 1,2), n=2)
l <- layout_with_kk(multigtr)

# Plotting this simple graph
plot(multigtr, vertex.color="lightsteelblue", vertex.frame.color="white",
     vertex.size=40, vertex.shape="circle", vertex.label=NA,
     edge.color=c("gold", "tomato", "yellowgreen"), edge.width=5, 
     edge.arrow.size=3, edge.curved=0.1, layout=l)

# To avoid this overdrawing issue, can assign different curveture to each edge. Uses the curve_multiple function to do this
plot(multigtr, vertex.color="lightsteelblue", vert.frame.color="white",
     edge.color=c("gold", "tomato", "yellowgreen"), edge.width=3, vertex.label=NA,
     edge.arrow.size=2, edge.curved=curve_multiple(multigtr), layout=l)

## Detaching packages
# igraph conflicts with many other packages... turn it off when no longer needed.
detach("package:igraph")

#### Interactive Network Maps ####

### Plot animations with ndtv ###
## The animation package with ndtv allows for simple animations not only for networks.
## For this to work it also needs imagemagick installed (https://imagemagick.org/)
ani.options("convert") # checks if it knows where to find imagemagick
ani.options(convert="C:/Program Files/ImageMagick-7.1.0-Q16-HDRI/convert.exe") #sets location

## Generating four network plots, saving as gifs, then animating them.
l <- layout_with_lgl(net)

saveGIF( {  col <- rep("grey40", vcount(net))
plot(net, vertex.color=col, layout=l)

step.1 <- V(net)[media=="Wall Street Journal"]
col[step.1] <- "#ff5100"
plot(net, vertex.color=col, layout=l)

step.2 <- unlist(neighborhood(net, 1, step.1, mode="out"))
col[setdiff(step.2, step.1)] <- "#ff9d00"
plot(net, vertex.color=col, layout=l) 

step.3 <- unlist(neighborhood(net, 2, step.1, mode="out"))
col[setdiff(step.3, step.2)] <- "#FFDD1F"
plot(net, vertex.color=col, layout=l)  },
interval = .4, movie.name="network_animation.gif" )

# This is pretty... not sure how useful for us, also pretty confused about what the above does
detach("package:igraph")
detach("package:ndtv")

#### Interactive visualistion with visNetwork ####
## This uses JS outputted by the visNetwork package
visNetwork(nodes, links, width = "100%", height = "400px",
           background="wheat", main="Network", submain = "And what a great network it is!",
           footer = "Hyperlinks and mentions among media sources")

## Can set properties to the nodes and edges like in igraph. Following help section is useful
?visNodes
?visEdges

# Trying a more descriptive chart
vis.nodes <- nodes
vis.links <- links

vis.nodes$shape <- "dot"
vis.nodes$shadow <- " TRUE" #drop shadow on nodes
vis.nodes$title <- vis.nodes$media #label on click
vis.nodes$label <- vis.nodes$type.label #node label
vis.nodes$size <- vis.nodes$audience.size # node size
vis.nodes$borderWidth <- 2 # node border width

vis.nodes$color.background <- c("slategrey", "tomato", "gold")[nodes$media.type]
vis.nodes$color.border <- "black"
vis.nodes$color.highlight.background <- "orange"
vis.nodes$color.highlight.border <- "darkred"

visNetwork(vis.nodes, vis.links)

# Updating the edges...
vis.links$width <- 1+links$weight/8 # line width set by the weight
vis.links$color <- "gray" #line color
vis.links$arrows <- "middle" #arrows: "from, to or middle"
vis.links$smooth <- FALSE # should the edges be curved
vis.links$shadow <- FALSE # removes drop shadow

visnet <- visNetwork(vis.nodes, vis.links)
visnet
