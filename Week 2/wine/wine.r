#!/usr/bin/Rscript

wine = read.csv('wine.csv')

# Quick question 4
# Creating model to predict Price using HarvestRain and WinterRain data
modelR = lm(Price ~ HarvestRain +  WinterRain, data=wine)
summary(modelR) # Check "Multiple R-squared" value, coefficient for HarvestRain and intercept coefficient

# Quick question 5
cor(wine$HarvestRain, wine$WinterRain)
summary(lm(Price ~ HarvestRain + WinterRain, data=wine))