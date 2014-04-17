#!/usr/bin/Rscript

#################################################################################
# installing required packages
#! install.packages("gbm")
library(gbm)

# Downloading data
train = read.csv("train.csv")
test = read.csv("test.csv")
old.train = read.csv("train.csv")
old.test = read.csv("test.csv")
for (i in names(train)) {
    levels(train[,i]) <- c(levels(train[,i]), "Skipped")
    train[,i][train[,i] == ''] <- 'Skipped'
    train[,i] = factor(train[,i])
}
for (i in names(test)) {
    levels(test[,i]) <- c(levels(test[,i]), "Skipped")
    test[,i][test[,i] == ''] <- 'Skipped'
    test[,i] = factor(test[,i])
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

# Imputting YOB variable with mice package
library(mice)
train = complete(mice(train))
test = complete(mice(test))
write.csv(train, "train_imputed.csv")
write.csv(test, "test_imputed.csv")

# 