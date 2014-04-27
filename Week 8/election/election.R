#!/usr/bin/Rscript

# ELECTION FORECASTING REVISITED
####################################################################################
# Loading packages
library(ggplot2)
library(maps)
library(ggmap)
# Loading US map
statesMap = map_data("state")


####################################################################################
# PROBLEM 1 - DRAWING A MAP OF THE US  
str(statesMap)
# How many different groups are there?
table(statesMap$group)

# Drawing a map
ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black") + coord_map("mercator")


####################################################################################
# PROBLEM 2 - COLORING THE STATES BY PREDICTIONS
# Reading poll imputed data
polling = read.csv("PollingImputed.csv")
# Splitting the data
Train = subset(polling, polling$Year>=2004 & polling$Year<=2008)
Test = subset(polling, polling$Year==2012)
# Creating logistic regression model
mod2 = glm(Republican~SurveyUSA+DiffCount, data=Train, family="binomial")
TestPrediction = predict(mod2, newdata=Test, type="response")
# Vector of Republican/Democrat predictions
TestPredictionBinary = as.numeric(TestPrediction > 0.5)
predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)
# For how many states is our binary prediction 1, corresponding to Republican?
table(TestPredictionBinary)
# What is the average predicted probability of our model?
mean(TestPrediction)

# Merging datasets
predictionDataFrame$region = tolower(predictionDataFrame$Test.State)
predictionMap = merge(statesMap, predictionDataFrame, by = "region")
predictionMap = predictionMap[order(predictionMap$order),]
# How many observations are there in predictionMap?
nrow(predictionMap)
# How many observations are there in statesMap?
nrow(statesMap)

# Coloring the US map with predictions
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + geom_polygon(color = "black")

# Recoloring with a discrete case
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) +
  geom_polygon(color = "black") + 
  scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")
# Probability case
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction)) +
  geom_polygon(color = "black") + 
  scale_fill_gradient(low = "white", high = "red", guide = "legend", name = "Prediction 2012") + 
  scale_color_brewer(palette="Dark2")


####################################################################################
# PROBLEM 3 - UNDERSTANDING THE PREDICTIONS
# What was our predicted probability for the state of Florida?
predFlorida = subset(predictionMap, predictionMap$Test.State=="Florida")
mean(predFlorida$TestPrediction)


#####################################################################################
# PROBLEM 4 - PARAMETER SETTINGS
# Playing with ggplot geom_polygon parameter
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) +
  geom_polygon(color = "black", alpha=0.3) + 
  scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")
