#!/usr/bin/Rscript

# FORECASTING ELANTRA SALES
####################################################################################
# PROBLEM 1 - LOADING THE DATA
elantraData = read.csv("elantra.csv")
# Splitting the data
train = subset(elantraData, elantraData$Year <= 2012)
test = subset(elantraData, elantraData$Year > 2012)
# How many observations are in the training set?
nrow(train)

####################################################################################
# PROBLEM 2 - A LINEAR REGRESSION MODEL
lmElantra = lm(ElantraSales ~ Unemployment + CPI_all + CPI_energy + Queries, data=train)
summary(lmElantra)

####################################################################################
# PROBLEM 6 - MODELING SEASONALITY
lmElantra2 = lm(ElantraSales ~ Month + Unemployment + CPI_all + CPI_energy + Queries, data=train)
summary(lmElantra2)

# PROBLEM 8 - UNDERSTANDING THE MODEL
# absolute difference in predicted Elantra sales given that one period is in January and one is in March
110.69*(3-1)
# absolute difference in predicted Elantra sales given that one period is in January and one is in May
110.69*(5-1)

####################################################################################
# PROBLEM 10 - A NEW MODEL
train$Month = as.factor(train$Month)
lmElantra3 = lm(ElantraSales ~ Month + Unemployment + CPI_all + CPI_energy + Queries, data=train)
summary(lmElantra3)

####################################################################################
# PROBLEM 13 - MULTICOLINEARITY
train = subset(elantraData, elantraData$Year <= 2012)
cor(train)

####################################################################################
# PROBLEM 15 - A REDUCED MODEL
train$Month = as.factor(train$Month)
lmElantraM1 = lm(ElantraSales ~ Month + Unemployment + CPI_all + CPI_energy + Queries, data=train)
summary(lmElantraM1)
#  - Queries
lmElantraM2 = lm(ElantraSales ~ Month + Unemployment + CPI_energy + CPI_all, data=train)
summary(lmElantraM2)

####################################################################################
# PROBLEM 16 - TEST SET PREDICTIONS
test$Month = as.factor(test$Month)
salesPrediction = predict(lmElantraM2, newdata=test)
# Computing sum of the squared errors
SSE = sum((salesPrediction - test$ElantraSales)^2)
SSE
# Computing test set R-squared
baselineSales = mean(train$ElantraSales)
SST = sum((baselineSales - test$ElantraSales)^2)
R2 = 1 - SSE/SST
# Maximum absolute test set error
max(abs(salesPrediction - test$ElantraSales))
# In which period (Month,Year pair) do we make the largest absolute error in our prediction?
which.max(abs(salesPrediction - test$ElantraSales))
