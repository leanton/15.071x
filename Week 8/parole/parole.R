#!/usr/bin/Rscript

# VISUALIZING ATTRIBUTES OF PAROLE VIOLATORS
library(ggplot2)
#####################################################################################
# PROBLEM 1 - LOADING THE DATA
parole = read.csv("parole.csv")
# Converting to factor variables
parole$male = as.factor(parole$male)
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)
# What fraction of parole violators are female?
nrow(subset(parole, parole$male==0 & parole$violator==1))/nrow(subset(parole, parole$violator==1))

# In this dataset, which crime is the most common in Kentucky?
summary(subset(parole, parole$state==2))


#####################################################################################
# PROBLEM 2 - CREATING A BASIC HISTOGRAM
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5)
# Adding features to histogram
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5, color="blue")


#####################################################################################
# PROBLEM 3 - ADDING ANOTHER DIMENSION
# What is the age bracket with the most female parolees?
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5) + facet_grid(male ~ .)
# Changing facet_grid
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5) + facet_grid(.~male)
# Coloring different groups
ggplot(data = parole, aes(x = age, fill = male)) + geom_histogram(binwidth = 5)
ggplot(data = parole, aes(x = age, fill = male)) + geom_histogram(binwidth = 5, position="identity", alpha = 0.5)


#####################################################################################
# PROBLEM 4 - TIME SERVED
ggplot(data = parole, aes(x = time.served)) + geom_histogram(binwidth = 0.1)
# Time served for each crime with facet_grid
ggplot(data = parole, aes(x = time.served)) + geom_histogram(binwidth = 1) + facet_grid(crime~.)
# Overlaying histograms
ggplot(data = parole, aes(x = time.served, fill = crime)) + geom_histogram(binwidth = 1)
