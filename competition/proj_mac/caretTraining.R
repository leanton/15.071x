#!/usr/bin/Rscript

library(caret)
fitControl = trainControl( method = "cv", number = 10 )
glmFit = train(Happy ~ ., data = fac.train, method="glm", trControl=fitControl)

bglmFit = train(Happy ~., data=train, method="bayesglm", trControl=fitControl)

fitControl = trainControl( method = "cv", number = 10 )
grid = expand.grid(alpha=(1:10)*0.02, lambda=(1:10)*0.1)
glmnFit = train(x = x.train, y = train$Happy, method="glmnet", family="binomial", trControl=fitControl, tuneGrid=grid)

###########################
# Logistic regression training
fitControl = trainControl( method = "cv", number = 10 )
glmFit = train(Happy ~ ., data = gtrain, method="glm", trControl=fitControl)
glmFit

###
# reducing data
gtrain$Party = NULL
gtrain$Income = fac.train$Income

gtrain$Q113584 = NULL
gtrain$Q109244 = NULL
gtrain$Q107491 = NULL
gtrain$Q120472 = NULL
gtrain$Q124122 = NULL
gtrain$Q98059 = NULL

##############################
# RPART
fitControl = trainControl( method = "cv", number = 10 )
cartGrid = expand.grid( .cp = (1:10)*0.002) 
rFit = train(Happy ~ ., data = fac.train, method="rpart", trControl=fitControl, tuneGrid=cartGrid)