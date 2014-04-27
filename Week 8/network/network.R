#!/usr/bin/Rscript

install.packages("igraph")
library(igraph)
# VISUALIZING NETWORK DATA
####################################################################################
# PROBLEM 1 - SUMMARIZING THE DATA
# Loading data from csv
edges = read.csv("edges.csv")
users = read.csv("users.csv")
# How many Facebook users are there in our dataset?
nrow(users)
# In our dataset, what is the average number of friends per user?

# Out of all the students who listed a school, what was the most common locale?
summary(users)


####################################################################################
# PROBLEM 2 - CREATING A NETWORK
g = graph.data.frame(edges, FALSE, users)
# Plotting a graph
plot(g, vertex.size=5, vertex.label=NA)
mean(degree(g))

# Visualizing important nodes
V(g)$size = degree(g)/2+2
plot(g, vertex.label=NA)
# What is the largest/smallest size we assigned to any node in our graph?
max(V(g)$size)
min(V(g)$size)


####################################################################################
# PROBLEM 3 - COLORING VERTICES
# Continuing modifying our graph
# Coloring the gender
V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "gray"
plot(g, vertex.label=NA)

# Coloring the school attending
V(g)$color = "black"
V(g)$color[V(g)$school == "A"] = "red"
V(g)$color[V(g)$school == "AB"] = "gray"
plot(g, vertex.label=NA)

# Coloring the locale
V(g)$color = "black"
V(g)$color[V(g)$locale == "A"] = "red"
V(g)$color[V(g)$locale == "B"] = "gray"
plot(g, vertex.label=NA)

# Changing width
plot(g, vertex.label=NA, edge.width=3)
