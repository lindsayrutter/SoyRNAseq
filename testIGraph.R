# Must use igraph version 0.7.0

setwd("/Users/MacOwner/Desktop/Cook/SBTree")

library(plyr)
library(reshape2)
library(ggplot2)
library(stringr)
library(igraph)

load("tree.rda")
# Total tree nodes (412*2)
treeGraph = as.data.frame(cbind(tree$child, tree$parent))
# Remove any rows with "NA" relationship (340*2)
treeGraph = treeGraph[-which(is.na(tree$parent)),]
# Add an edge weight to each pair of vertices (all of weight value equal to one)
treeGraph = cbind(treeGraph, rep(1, dim(treeGraph)[1]))
# Add column names the tree
colnames(treeGraph) = c("child","parent","edgeWt")

# Write the tree as a .cvs file to be read into Cytoscape
sbTreeGraph = write.csv(treeGraph, "sbTreeGraphTest.csv")

# Convert the data frame object into an object that can be read by igraph package
mygraph = graph.data.frame(treeGraph, directed= T)

v1 = "Brim"
v2 = "Bedford"
v3 = "Flambeau"
v4 = "Tokyo"
v5 = "Narow"

# pathVIndices = get.shortest.paths(mygraph, v4, v5, weights = NA, output="vpath")$vpath[[1]]
# In directed graph, order matters between Tokyo and Narow
# Flambeau is not connected
# Brim and Bedford can only get a path if undirected graph
# There could be >1 paths in some cases. This only returns the shortest path.

# pathVertices = character()
# for (i in 1:length(pathVIndices)){
#   pathVertices = c(pathVertices, get.vertex.attribute(mygraph, "name", index=pathVIndices[i]))
# }

# Below are possible graph visualization tools
# Graphs in igraph
# plot(mygraph)
# tkplot(mygraph)
# rglplot(mygraph)
# The 'Select some vertices' dialog allows to give an expression for the
# vertices to be selected: this can be a list of numeric R expessions
# separated by commas, like '1,2:10,12,14,15' for example. Similarly in the
# ‘Select some edges’ dialog two such lists can be given and all edges
# connecting a vertex in the first list to one in the second list will be
# selected.

# This function determines the shortest path between the two inputted vertices, and takes into
# account whether or not the graph is directed. If there is a path, the list of vertices of the
# path will be returned. If there is not a path, a list of character(0) will be returned. Note:
# For a directed graph, the direction matters. However, this function will check both directions
# and return the path if it exists.
getPath = function(v1, v2, isDirected){
  # If the tree is directed
  if (isDirected){
    # Convert the data frame object into an object that can be read by igraph package
    mygraph = graph.data.frame(treeGraph, directed= T)
    pathVertices = character()
    # We need to look at both forward and reverse cases of directions, because the user may not know
    # the potential direction of a path between the two vertices
    pathVIndicesForward = get.shortest.paths(mygraph, v1, v2, weights = NA, output="vpath")$vpath[[1]]
    pathVIndicesReverse = get.shortest.paths(mygraph, v2, v1, weights = NA, output="vpath")$vpath[[1]]
    # If there is a path in the forward direction, then we save the names of the vertices in that order
    if (length(pathVIndicesForward) != 0){
      for (i in 1:length(pathVIndicesForward)){
        pathVertices = c(pathVertices, get.vertex.attribute(mygraph, "name", index=pathVIndicesForward[i]))
      }
    }
    # If there is a path in the reverse direction, then we save the names of the vertices in that order
    if (length(pathVIndicesReverse) != 0){
      for (i in 1:length(pathVIndicesReverse)){
        pathVertices = c(pathVertices, get.vertex.attribute(mygraph, "name", index=pathVIndicesReverse[i]))
      }
    }
  }
  # If the tree is undirected
  if (!isDirected){
    mygraph = graph.data.frame(treeGraph, directed= F)
    pathVertices = character()
    # The direction does not matter, any shortest path between the vertices will be listed
    pathVIndices = get.shortest.paths(mygraph, v1, v2, weights = NA, output="vpath")$vpath[[1]]
    if (length(pathVIndices) != 0){
      for (i in 1:length(pathVIndices)){
        pathVertices = c(pathVertices, get.vertex.attribute(mygraph, "name", index=pathVIndices[i]))
      }      
    }
  }
  # Return the shortest path, if it exists
  pathVertices
}

# This function prints to console the basic graph theoretical measurements of the inputted graph.
getBasicStatistics = function(mygraph){
  # Get edge and node count from "structure.info" function of igraph
  numNodes = vcount(mygraph)
  numEdges = ecount(mygraph)
  # Determine if the graph is connected or not from "clusters" function of igraph
  isConnected = is.connected(mygraph)
  # Determine the number of connected components in the graph from "clusters"
  # function of igraph
  numComponents = no.clusters(mygraph)
  # Compute the average path length of the graph
  connected = FALSE
  if(isConnected)
  {
    connected = TRUE
  }
  avePathLength = average.path.length(mygraph, directed=F, unconnected= !isConnected)
  # Determine the log(N) value of the graph
  logN = log(numNodes)
  # Determine the network diameter
  graphDiameter = diameter(mygraph, directed = F, unconnected = !isConnected, weights = NULL)
  # Print all statistics to console
  print(paste("The current graph is connected?: ", isConnected))
  print(paste("The current graph has ", numComponents," connected components." ))
  print(paste("The average path length of the graph is: ", avePathLength)) 
  print(paste("The diameter of the graph is: ", graphDiameter))
  print(paste("The number of nodes in the graph is: ", numNodes))
  print(paste("The number of edges in the graph is: ", numEdges))
  print(paste("The log of the network size is: ", logN))
}

getBasicStatistics(mygraph)
getPath(v1, v2, F)
