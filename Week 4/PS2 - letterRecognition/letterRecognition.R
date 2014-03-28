#!/sur/bin/Rscript

# LETTER RECOGNITION
# Loading data
letter = read.csv("letters_ABPR.csv")
str(letter)

# PROBLEM 1 - PREDICTING B OR NOT B
letter$isB = as.factor(letter$letter == "B")

# Splitting data
library(caTools)
set.seed(1000)
split = sample.split(letter$isB, SplitRatio = 0.5)
Train = subset(letter, split==TRUE)
Test = subset(letter, split==FALSE)
summary(Train)


library(rpart)
library(rpart.plot)
CARTb = rpart(isB ~ . - letter, data=Train, method="class")
# What is the accuracy of the CART model on the test set?
PredictCART = predict(CARTb, newdata = Test, type = "class")
table(Test$isB, PredictCART)

# building a random forest model to predict whether the letter is a B or not
library(randomForest)
set.seed(1000)
ForestB = randomForest(isB ~ . - letter, data=Train)
# What is the accuracy of the model on the test set?
PredictForest = predict(ForestB, newdata = Test)
table(Test$isB, PredictForest)
# 0.9878049

#################################################################
# PROBLEM 2 - PREDICTING THE LETTERS A, B, P, R
letter = read.csv("letters_ABPR.csv")
letter$letter = as.factor( letter$letter )
# Generating new data
set.seed(2000)
split = sample.split(letter$letter, SplitRatio = 0.5)
Train = subset(letter, split==TRUE)
Test = subset(letter, split==FALSE)
summary(Test)

# Building CART model
CART = rpart(letter ~ . - letter, data=Train, method="class")
PredictCART = predict(CART, newdata = Test, type = "class")
nrow(Test)
table(Test$letter, PredictCART)

# Buliding Random Forest Model
set.seed(1000)
ForestL = randomForest(letter ~ . - letter, data=Train)
# What is the accuracy of the model on the test set?
PredictForest = predict(ForestL, newdata = Test)
nrow(Test)
table(Test$letter, PredictForest)
# 0.9801027
