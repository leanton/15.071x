#!/usr/bin/Rscript

# Loading data
census = read.csv("census.csv")

###################################################################
# PROBLEM 1 - A LOGISTIC REGRESSION MODEL
# Splitting data
library(caTools)
set.seed(2000)
split = sample.split(census$over50k, SplitRatio = 0.6)
train = subset(census, split==TRUE)
test = subset(census, split==FALSE)

# Building logistic regression
censusLog = glm(over50k ~ ., data=train, family=binomial)
summary(censusLog)

# What is the accuracy of the model on the testing set? Use a threshold of 0.5.
predictLog = predict(censusLog, newdata=test, type="response")
table(test$over50k, predictLog>=0.5)
# (9051+1888)/(9051+1888+662+1190)
# baseline accuracy for the testing set
summary(test$over50k)
# 9713/(9713+3078)
# area-under-the-curve (AUC) for this model on the test set
library(ROCR)
ROCRpredTest = prediction(predictLog, test$over50k)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc

#####################################################################
# PROBLEM 2 - A CART MODEL
# Building a CART model
library(rpart)
library(rpart.plot)
CARTmodel = rpart(over50k ~ ., data=train, method="class")
prp(CARTmodel)
# Accuracy on the testing set
predCART = predict(CARTmodel, newdata=test)
table(test$over50k, predCART)
# (9243+1596)/(9243+470+1482+1596)
# AUC of the model
#library(ROCR)
predCART = predict(CARTmodel, newdata=test, type="prob" )
ROCRpredTest2 = prediction(predCART[, 2], test$over50k)
auc2 = as.numeric(performance(ROCRpredTest2, "auc")@y.values)
auc2

##########################################################################
# PROBLEM 3 - A RANDOM FOREST MODEL
# Building random forest
set.seed(1)
trainSmall = train[sample(nrow(train), 2000), ]
library(randomForest)
randFor = randomForest(over50k ~ ., data = trainSmall)
str(trainSmall)
randFor = randomForest(over50k ~ . - nativecountry, data = trainSmall)
PredictEarnings = predict(randFor, newdata = test)
table(test$over50k, PredictEarnings)
# (8883+2035)/(8883+830+1043+2035)
# Building metrics
vu = varUsed(randFor, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(randFor$forest$xlevels[vusorted$ix]))
varImpPlot(randFor)

############################################################################
# PROBLEM 4 - SELECTING CP BY CROSS-VALIDATION
# install.packages("caret")
library(caret)
# install.packages("e1071")
library(e1071)
set.seed(2)
# Define cross-validation experiment
fitControl = trainControl( method = "cv", number = 10 )
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))
# Perform the cross validation
train(over50k ~ ., data = train, method = "rpart", trControl = fitControl, tuneGrid = cartGrid) # cp = 0.002

# Building CART model. Calculate the prediction accuracy
CARTmodel2 = rpart(over50k ~ ., data = train, control=rpart.control(cp = 0.002), method="class")
PredictCART2 = predict(CARTmodel2, newdata = test, type="class")
table(test$over50k, PredictCART2)

# Plotting tree
prp(CARTmodel2)