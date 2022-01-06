#### Introduction ####
## This script records my initial attempts to learn network analysis methods in R.
## It is based on the tutorial by Jesse Sadler - https://www.jessesadler.com/post/network-analysis-with-r/
# This site is archived here: https://archive.vn/K47pM
# Some excellent links to useful guides at end of tutorial - such as this: https://archive.vn/coMO2

## Header
# Start date: 6 January 2022
# Author: Mark Limb


#### Installing and loading required packages ####
## Installation
# install.packages('tidyverse')
# install.packages('dplyr')
# install.packages('tidygraph')
# install.packages('ggraph')
# install.packages('visNetwork')
# install.packages('networkD3')

## Loading
library(tidyverse)
library(dplyr)
library(tidygraph)
library(ggraph)
library(visNetwork)
library(networkD3)


#### Node and edge tables theory ####
## There are special class types used in network analysis known as adjacency matricies.
# These are also know as sociomatricies -  Carter Butts, “network: A Package for Managing Relational Data in R”, Journal of Statistical Software, 24 (2008): 1–36
# The matrix is square with node names in column and row names, and a value of 0 or 1 to record if there is a connection between the nodes

# Nodes are the objects in the network and edges are the connections

## Edge lists record directionality and details about the connections.
# They have at least two columns - one records the source node and the other the target node
# Each node needs a unique ID to identify it
# Can also include magnitude column to record strength of association. If this exists, it's referred to as weighted.

# Advantage of using a combination of edge and node list is richer data compared to a simple adjacency matrix

## Creating basic example of edge and node lists
edge_list <- tibble(source = c(1,2,3,4,5), target = c(2,5,4,3,1))
node_list <- tibble(id = 1:5)
edge_list
node_list

#### Setting up data ####
## Following along with correspondence data as an example ##
# Data from Sadler's github page: https://github.com/jessesadler/intro-to-r/tree/master/data

letters <- tibble(read.csv("data/correspondence-data-1585.csv")) # read in the data as a tibble

# This is a neat example of creating summarised data.
per_correspondent <- letters %>% 
  group_by(writer) %>% # groups the following the unique writers
  summarise(count = n()) %>%  # adds a new column called count which summarises the number of entries by the group
  arrange(desc(count)) # orders the table in descending order of the column named count

# The following code saves out some of the individual aspects of the input data
writer <- distinct(letters, writer) # new table with only unique writers

source <- letters %>%
  distinct(source) %>% # new tables with only unique sources
  rename(label = source) # renames source column as label to match with format used in network analysis

destination <- letters %>% 
  distinct(destination) %>%  # new table with only unique destinations
  rename(label = destination)

## Creating a unique node list that includes all sources and destinations
nodes <- full_join(source, destination, by = "label")
nodes <- nodes %>% rowid_to_column("id") # adds a unique ID column for each node using the rowid_to_column command and saves column name as "id"

## Creating an edge list
per_route <- letters %>% 
  group_by(source, destination) %>% # groups by the source and destination
  summarise(weight = n()) %>% # makes a new column that counts each unique entry of source/destination - column name of wright
  ungroup() # ungroups the data
  
# need to bring in the unique ids for each of these
edges <- per_route %>% 
  left_join(nodes, by = c("source" = "label")) %>% # joins the nodes data using the source and label columns (from per-route and nodes tables respectively)
  rename(from = id) # renames the ID column to "from"

edges <- edges %>% 
  left_join(nodes, by = c("destination" = "label")) %>% # similar to previous command, but for destination column
  rename(to = id)

edges <- select(edges, from, to, weight) # reorders the data, and removes source and destination as these labels are already capture in nodes

#### Network visualisation of sample data ####
## The tutorial describes a number of packages for this form of analysis
# The network package looks easy but seems basic
# the igraph package looks better
# However the tidygraph and ggraph combo appears even better so I'm just going to use those.

## Getting data ready
# tidygraph package use tbl_graph objects which combine nodes and edges into a single object
# there's a function which does this

routes_tidy <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)

## Plotting data
# basic network plot
ggraph(routes_tidy) +
  geom_edge_link() +
  geom_node_point() +
  theme_graph()

# a more detailed network plot 
ggraph(routes_tidy, layout = "graphopt") + #loads the tbl_graph object and sets the layout
  geom_node_point() + #tells it to plot nodes as basic points
  geom_edge_link(aes(width = weight), alpha = 0.8) + #tells it to plot edges as links, with diff lines based on weight data and slight transparency
  scale_edge_width(range = c(0.2, 3)) + # sets the scale of the edge plot - large difference so would look crazy without this
  geom_node_text(aes(label = label), repel = TRUE) + #labels the nodes with label field, repel is used to avoid text overlap
  labs(edge_width = "Number of letters") + #legend isn't working for me, but this is setting the legend title
  theme_graph() #theme of plot

# an arc based graph with directionality - above the line goes left to right, below the opposite
ggraph(routes_tidy, layout = "linear") +
  geom_edge_arc(aes(width = weight), alpha = 0.8) +
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label), repel = TRUE) +
  labs(edge_width = "Number of letters") +
  theme_graph()

### Interactive visualisations ###
## Visnetworks - nice JS style plots. Needs specific labeling as previously setup
visNetwork(nodes, edges)

# The width attribute for visNetwork() does not scale the values, so we have to do this manually
edges <- mutate(edges, width = weight/5+1)

visNetwork(nodes, edges) %>% 
  visIgraphLayout(layout = "layout_with_fr") %>% # this sets the layout style from the igraph layouts
  visEdges(arrows = "middle") # sets the arrow position

## networkD3
# To make a networkD3 graph with a edge and node list requires that the IDs be a series of numeric integers that begin with 0. 

nodes_d3 <- mutate(nodes, id = id-1) # subtracts 1 from each of the id columns
edges_d3 <- mutate(edges, from = from-1, to = to-1) # subtracts 1 from each of the from and to columns

# plotting using D3
forceNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "from", Target = "to",
             NodeID = "label", Group = "id", Value = "weight",
             opacity = 1, fontSize = 16, zoom = TRUE)

# Or using a Sankey diagram - so hot right now
sankeyNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "from", Target = "to", 
              NodeID = "label", Value = "weight", fontSize = 16, unit = "Letter(s)")

