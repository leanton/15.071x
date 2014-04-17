#!/usr/bin/Rscript

# DOCUMENT CLUSTERING WITH DAILY KOS
#########################################################################################
# PROBLEM 1 - HIERARCHICAL CLUSTERING
documents = read.csv("dailykos.csv")

# Compute the distances (using method="euclidean"), and use hclust to build the model (using method="ward")
# Compute distances
distances = dist(documents[2:1546], method = "euclidean") # Ignoring first variable Document
# Hierarchical clustering
kosHierClust = hclust(distances, method = "ward")

# Plot the dendrogram
plot(kosHierClust)

# Let's pick 7 clusters. This number is reasonable according to the dendrogram, and also seems reasonable for the application. 
# Use the cutree function to split your data into 7 clusters.
clusterGroups = cutree(kosHierClust, k = 7)
# Create 7 new datasets, each containing the observations from one of the clusters.
cluster1 = subset(documents, clusterGroups==1)
cluster2 = subset(documents, clusterGroups==2)
cluster3 = subset(documents, clusterGroups==3)
cluster4 = subset(documents, clusterGroups==4)
cluster5 = subset(documents, clusterGroups==5)
cluster6 = subset(documents, clusterGroups==6)
cluster7 = subset(documents, clusterGroups==7)
# Number of observations in each cluster
table(clusterGroups)
which.min(table(clusterGroups))
which.max(table(clusterGroups))

# What is the most frequent word in this cluster, in terms of average value?
tail(sort(colMeans(cluster1[-1])))

# Which words best describe cluster 2?
tail(sort(colMeans(cluster2[-1])))
# Which cluster could best be described as the cluster related to the Iraq war?
# ...which cluster best corresponds to the democratic party?
tail(sort(colMeans(cluster1[-1])))
tail(sort(colMeans(cluster2[-1])))
tail(sort(colMeans(cluster3[-1])))
tail(sort(colMeans(cluster4[-1])))
tail(sort(colMeans(cluster5[-1])))
tail(sort(colMeans(cluster6[-1])))
tail(sort(colMeans(cluster7[-1])))


#########################################################################################
# PROBLEM 2 - K-MEANS CLUSTERING
# Starting k-means clustering
set.seed(1000)
KMC = kmeans(documents[2:1546], centers = 7)
str(KMC)
# How many observations are in Cluster 3?
# Which cluster has the most/fewest observations?
table(KMC$cluster)
# Subset your data into the 7 clusters (7 new datasets) by using the "cluster" variable
kCluster1 = subset(documents, KMC$cluster==1)
kCluster2 = subset(documents, KMC$cluster==2)
kCluster3 = subset(documents, KMC$cluster==3)
kCluster4 = subset(documents, KMC$cluster==4)
kCluster5 = subset(documents, KMC$cluster==5)
kCluster6 = subset(documents, KMC$cluster==6)
kCluster7 = subset(documents, KMC$cluster==7)

# output the six most frequent words in each cluster, like we did in the previous problem
# Which k-means cluster best corresponds to the Iraq War?
# Which k-means cluster best corresponds to the democratic party?
tail(sort(colMeans(kCluster1[-1])))
tail(sort(colMeans(kCluster2[-1])))
tail(sort(colMeans(kCluster3[-1])))
tail(sort(colMeans(kCluster4[-1])))
tail(sort(colMeans(kCluster5[-1])))
tail(sort(colMeans(kCluster6[-1])))
tail(sort(colMeans(kCluster7[-1])))

# Comparing k-means and clusteringGroups
table(KMC$cluster, clusterGroups)
