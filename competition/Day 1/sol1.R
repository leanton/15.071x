#!/usr/bin/Rscript

#################################################################################
# Loading data
train = read.csv("train.csv", na.string="")
test = read.csv("test.csv", na.string="")
str(train)
summary(train)

# Getting entries with full data (no NA in data)
train.full = train[!rowSums(is.na(train)), ]
test.full = test[!rowSums(is.na(test)), ]

colCluster = c()


#################################################################################
# ATTEMPT 1
# Logistic regression for all training data
trainLog = glm(Happy ~ ., data = train, family=binomial)
predictTest = predict(trainLog, newdata=test, na.action = na.pass, type="response")
predictTest[is.na(predictTest)] = 1

# Submitting results
submission = data.frame(UserID = test$UserID, Probability1 = CARTtest)
write.csv(submission, "submissionCART.csv", row.names=FALSE)

# ATTEMPT 2
# Using CART trees
library(rpart)
library(rpart.plot)
CARTmodel = rpart(Happy ~ ., data=train, method="class")
prp(CARTmodel)
CARTtest = predict(CARTmodel, newdata = test, type = "class")
# Submitting results
submission = data.frame(UserID = test$UserID, Probability1 = CARTtest)
write.csv(submission, "submissionCART.csv", row.names=FALSE)

#################################################################################
# GOT ACCURACY 0.68
train = read.csv("train.csv")
test = read.csv("test.csv")
trainLog = glm(Happy ~ ., data = train, family=binomial)
predictTest = predict(trainLog, newdata=test, na.action = na.pass, type="response")
predictTest[is.na(predictTest)] = 1
submission = data.frame(UserID = test$UserID, Probability1 = predictTest)
write.csv(submission, "submissionCART.csv", row.names=FALSE)


#################################################################################
# ACCURACY 0.72248
# Making skipped variable
train = read.csv("train.csv")
test = read.csv("test.csv")
old.train = read.csv("train.csv")
old.test = read.csv("test.csv")
for (i in names(train)) {
    levels(train[,i]) <- c(levels(train[,i]), "Skipped")
    train[,i][train[,i] == ''] <- 'Skipped'
}
for (i in names(test)) {
    levels(test[,i]) <- c(levels(test[,i]), "Skipped")
    test[,i][test[,i] == ''] <- 'Skipped'
}
# Correcting train vars
train$UserID = old.train$UserID
train$YOB = old.train$YOB
train$Happy = old.train$Happy
train$votes = old.train$votes
# Correcting test vars
test$UserID = old.test$UserID
test$YOB = old.test$YOB
test$votes = old.test$votes
trainLog = glm(Happy ~ . - YOB - votes, data = train, family=binomial)
predictTest = predict(trainLog, newdata=test, type="response")
submission = data.frame(UserID = test$UserID, Probability1 = predictTest)
write.csv(submission, "submissionLogSkipped.csv", row.names=FALSE)

#################################################################################
# Making skipped variable
train = read.csv("train.csv")
test = read.csv("test.csv")
old.train = read.csv("train.csv")
old.test = read.csv("test.csv")
for (i in names(train)) {
    levels(train[,i]) <- c(levels(train[,i]), "Skipped")
    train[,i][train[,i] == ''] <- 'Skipped'
}
for (i in names(test)) {
    levels(test[,i]) <- c(levels(test[,i]), "Skipped")
    test[,i][test[,i] == ''] <- 'Skipped'
}
# Correcting train vars
train$UserID = old.train$UserID
train$YOB = old.train$YOB
train$Happy = old.train$Happy
train$votes = old.train$votes
# Correcting test vars
test$UserID = old.test$UserID
test$YOB = old.test$YOB
test$votes = old.test$votes
trainLog = glm(Happy ~ . - YOB - votes, data = train, family=binomial)
predictTest = predict(trainLog, newdata=test, type="response")
predictTrain = predict(trainLog, newdata=train, type="response")
table(train$Happy, predictTrain > 0.5)

trainLog2 = glm(Happy ~ HouseholdStatus + EducationLevel + Party + Q122769 + Q122770 + Q121700 + Q121011 + Q120194 + Q120012 + Q120014 + Q119334 + Q119650 + Q118237 + Q116797 + Q116881 + Q116441 + Q116197 + Q115610 + Q115611 + Q115899 + Q115390 + Q114961 + Q114386 + Q113992 + Q113583 + Q113584 + Q111848 + Q110740 + Q109367 + Q108855 + Q108617 + Q108856 + Q108754 + Q108342 + Q108343 + Q107869 + Q106388 + Q106389 + Q102906 + Q102674 + Q102687 + Q102289 + Q101162 + Q100680 + Q100562 + Q99716 + Q98869, data=train, family=binomial)
predictTrain2 = predict(trainLog, newdata=train, type="response")
predictTest2 = predict(trainLog2, newdata=test, type="response")
submission = data.frame(UserID = test$UserID, Probability1 = predictTest2)
write.csv(submission, "submissionLogVarsSkipped.csv", row.names=FALSE)

trainLog3 = glm(Happy ~ EducationLevel + Q122769 + Q121011 + Q120012 + Q120014 + Q119334 + Q118237 + Q116797 + Q116441 + Q115610 + Q115390 + Q111848 + Q107869 + Q106389 + Q102906 + Q102687 + Q102289 + Q101162 + Q100562 + Q98869)
predictTest3 = predict(trainLog3, newdata=test, type="response")
submission = data.frame(UserID = test$UserID, Probability1 = predictTest3)
write.csv(submission, "submissionLogVarsSkipped2.csv", row.names=FALSE)
#################################################################################

















