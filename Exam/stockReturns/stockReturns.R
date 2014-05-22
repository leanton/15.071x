#!/usr/bin/Rscript

# CLUSTERING STOCK RETURNS

##################################################################
# PROBLEM 1 - LOADING THE DATA
nasdaq = read.csv("nasdaq_returns.csv")
summary(nasdaq$industry)
table(nasdaq$industry, nasdaq$stock_symbol)

# PROBLEM 3 - STOCK TRENDS IN THE DATA
nrow(subset(nasdaq, nasdaq$ret2000.12 >= 0.1))
nrow(subset(nasdaq, nasdaq$ret2000.12 <= -0.1))

# PROBLEM 4 - STOCK TRENDS IN THE DATA
tapply(nasdaq$ret2008.10, nasdaq$industry, mean)
tapply(nasdaq$ret2000.02, nasdaq$industry, mean)

##################################################################
# PROBLEM 5 - PREPARING THE DATASET
limited = nasdaq
limited$stock_symbol = NULL
limited$industry = NULL
limited$subindustry = NULL

which.max(colMeans(limited))
which.min(colMeans(limited))

##################################################################
# PROBLEM 8 - HIERARCHICAL CLUSTERING
distances = dist(limited, method = "euclidean")
limitedClust = hclust(distances, method = "ward")
plot(limitedClust)

# Dividing into 5 clusters
clusterGroups = cutree(limitedClust, k = 5)
table(clusterGroups)
# PROBLEM 10 - UNDERSTANDING THE CLUSTERS
table(nasdaq$industry, clusterGroups)
summary(nasdaq$industry)

# PROBLEM 11 - SUB-INDUSTRIES
table(nasdaq$subindustry, clusterGroups)

# PROBLEM 12 - STOCK TRENDS IN THE CLUSTERS
tapply(limited$ret2000.02, clusterGroups, mean)

tapply(limited$ret2000.03, clusterGroups, mean) # March 2000
tapply(limited$ret2005.05, clusterGroups, mean) # May 2005
tapply(limited$ret2009.10, clusterGroups, mean) # October 2009
tapply(limited$ret2009.12, clusterGroups, mean) # December 2009

##################################################################
# PROBLEM 14 - K-MEANS CLUSTERING
set.seed(144)
KMC = kmeans(limited, centers = 5)
table(KMC$cluster)
# PROBLEM 15 - COMPARING CLUSTERING ALGORITHMS
table(KMC$cluster, clusterGroups)

##################################################################
# PROBLEM 17 - CREATING A DIVERSE PORTFOLIO
aapl = subset(nasdaq, nasdaq$stock_symbol=="AAPL" | nasdaq$stock_symbol=="MSFT" | nasdaq$stock_symbol=="AMZN" | nasdaq$stock_symbol=="TROW")
table(nasdaq$stock_symbol=="AAPL", clusterGroups)
table(nasdaq$stock_symbol=="AMZN", clusterGroups)
table(nasdaq$stock_symbol=="MSFT", clusterGroups)
table(nasdaq$stock_symbol=="TROW", clusterGroups)

table(nasdaq$stock_symbol=="AAPL", KMC$cluster)
table(nasdaq$stock_symbol=="AMZN", KMC$cluster)
table(nasdaq$stock_symbol=="MSFT", KMC$cluster)
table(nasdaq$stock_symbol=="TROW", KMC$cluster)


