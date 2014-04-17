#!/usr/bin/Rscript

# MARKET SEGMENTATION FOR AIRLINES
#########################################################################################
# PROBLEM 1 - NORMALIZING THE DATA
# loading data
airlines = read.csv("AirlinesCluster.csv")
# which two variables have (on average) the smallest values?
# Which two variables have (on average) the largest values?
summary(airlines)

# install.packages("caret")
library(caret)
# normalizing data
preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)
# In the normalized data, which variable has the largest maximum value?
summary(airlinesNorm)


#########################################################################################
# PROBLEM 2 - HIERARCHICAL CLUSTERING
# Computing distances and building hierarchical clustering
distances = dist(airlinesNorm, method = "euclidean")
# Hierarchical clustering
airHeirClust = hclust(distances, method = "ward")
plot(airHeirClust)

# Divide the data points into 5 clusters by using the cutree function.
clusterGroups = cutree(airHeirClust, k = 5)
# How many data points are in Cluster 1?
table(clusterGroups)

# Analyzing clusters
tapply(airlines$Balance, clusterGroups, mean)
tapply(airlines$QualMiles, clusterGroups, mean)
tapply(airlines$BonusMiles, clusterGroups, mean)
tapply(airlines$BonusTrans, clusterGroups, mean)
tapply(airlines$FlightMiles, clusterGroups, mean)
tapply(airlines$FlightTrans, clusterGroups, mean)
tapply(airlines$DaysSinceEnroll, clusterGroups, mean)


#########################################################################################
# PROBLEM 3 - K-MEANS CLUSTERING
set.seed(88)
KMC = kmeans(airlinesNorm, centers = 5, iter.max = 1000)
str(KMC)
# How many clusters have more than 1,000 observations?
tapply(airlines$Balance, KMC$cluster, mean)
tapply(airlines$QualMiles, KMC$cluster, mean)
tapply(airlines$BonusMiles, KMC$cluster, mean)
tapply(airlines$BonusTrans, KMC$cluster, mean)
tapply(airlines$FlightMiles, KMC$cluster, mean)
tapply(airlines$FlightTrans, KMC$cluster, mean)
tapply(airlines$DaysSinceEnroll, KMC$cluster, mean)


