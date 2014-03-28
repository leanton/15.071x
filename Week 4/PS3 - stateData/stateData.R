#!/usr/bin/Rscript

# STATE DATA REVISITED
# Loading data
data(state)
statedata = data.frame(state.x77)
str(statedata)

# PROBLEM 1 - LINEAR REGRESSION MODELS
# Let's recreate the linear regression models we made in the previous homework question.
stateReg = lm(Life.Exp ~ Population +  Income + Illiteracy + Murder +  HS.Grad + Frost + Area, data=statedata)
summary(stateReg)

# Calculate the sum of squared errors (SSE) between the predicted life expectancies 
# using this model and the actual life expectancies:
predReg = predict(stateReg, newdata=statedata)
SSE = sum((statedata$Life.Exp - predReg)^2)
SSE

# Build a second linear regression model using just Population, Murder, Frost, and HS.Grad as independent variables
stateReg2 = lm(Life.Exp ~ Population + Murder +  HS.Grad + Frost, data=statedata)
# What is the adjusted R-squared for this model?
summary(stateReg2)

# Calculate the sum of squared errors again, using this reduced model:
predReg2 = predict(stateReg2, newdata=statedata)
SSE2 = sum((statedata$Life.Exp - predReg2)^2)
SSE2

########################################################################
# PROBLEM 2 - CART MODELS
# Let's now build a CART model to predict Life.Exp using all of the other variables as independent variables
# Loading required libraries
library(rpart)
library(rpart.plot)
CARTmodel = rpart(Life.Exp ~ ., data=statedata)
# Plotting model
prp(CARTmodel)

# SSE of CART model
PredictCART = predict(CARTmodel, newdata = statedata)
SSE3 = sum((statedata$Life.Exp - PredictCART)^2)
SSE3

# Rebuilding model with minbucket
CARTmodel2 = rpart(Life.Exp ~ ., data=statedata, control=rpart.control(minbucket=5))
prp(CARTmodel2)
# SSE of this model
PredictCART2 = predict(CARTmodel2, newdata = statedata)
SSE4 = sum((statedata$Life.Exp - PredictCART2)^2)
SSE4

# Create a tree that predicts Life.Exp using only Area, with the minbucket parameter to 1.
CARTmodel3 = rpart(Life.Exp ~ Area, data=statedata, control=rpart.control(minbucket=1))
PredictCART3 = predict(CARTmodel3, newdata = statedata)
SSE5 = sum((statedata$Life.Exp - PredictCART3)^2)
SSE5

#################################################################################
# PROBLEM 3 - CROSS-VALIDATION
# Loading caret library for cross validation
# install.packages("caret")
library(caret)
# install.packages("e1071")
library(e1071)
set.seed(111)

# Define cross-validation experiment
fitControl = trainControl( method = "cv", number = 10 )
cartGrid = expand.grid( .cp = (1:50)*0.01) 
# Perform the cross validation
train(Life.Exp ~ ., data = statedata, method = "rpart", trControl = fitControl, tuneGrid = cartGrid)

# Building a CART model with known cp
CARTmodel4 = rpart(Life.Exp ~ ., data = statedata, control=rpart.control(cp = 0.12))
prp(CARTmodel4)
# Calculating SSE of this model
PredictCART4 = predict(CARTmodel4, newdata = statedata)
SSE6 = sum((statedata$Life.Exp - PredictCART4)^2)
SSE6

# Building tree with ind var Area only using CV
set.seed(111)
train(Life.Exp ~ Area, data = statedata, method = "rpart", trControl = fitControl, tuneGrid = cartGrid) # cp=0.02
CARTmodel5 = rpart(Life.Exp ~ Area, data = statedata, control=rpart.control(cp = 0.02))
prp(CARTmodel5)
# Calculating SSE
PredictCART5 = predict(CARTmodel5, newdata = statedata)
SSE7 = sum((statedata$Life.Exp - PredictCART5)^2)
SSE7

