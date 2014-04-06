#!/usr/bin/Rscript

# SEPARATING SPAM FROM HAM
######################################################################################################
# PROBLEM 1 - LOADING THE DATASET
# Loading data
emails = read.csv("emails.csv", stringsAsFactors=FALSE)
str(emails)
# How many emails are in the dataset?
nrow(emails)

# How many of the emails are spam?
table(emails$spam)

# Which word appears at the beginning of every email in the dataset?
emails$text[1:10]

# How many characters are in the longest email in the dataset?
max(nchar(emails$text))

# Which row contains the shortest email in the dataset?
which.min(nchar(emails$text))


######################################################################################################
# PROBLEM 2 - PREPARING THE CORPUS
# Building a corpus and preprocessing data
library(tm)

corpus = Corpus(VectorSource(emails$text))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
dtm = DocumentTermMatrix(corpus)

# How many terms are in dtm?
dtm

# Limit dtm to contain terms appearing in at least 5% of documents
spdtm = removeSparseTerms(dtm, 0.95)
# How many terms are in spdtm?
spdtm

# Build a data frame called emailsSparse from spdtm, 
# and use the make.names function to make the variable names of emailsSparse valid.
emailsSparse = as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) = make.names(colnames(emailsSparse))
# What is the word stem that shows up most frequently across all the emails in the dataset?
which.max(colSums(emailsSparse))

# Adding spam variable to emailsSparse
emailsSparse$spam = emails$spam

# How many word stems appear at least 5000 times in the ham emails in the dataset?
hamSparse = subset(emailsSparse, emailsSparse$spam == FALSE)
hamSparse = colSums(hamSparse)
sort(hamSparse)

# How many word stems appear at least 1000 times in the spam emails in the dataset?
spamSparse = subset(emailsSparse, emailsSparse$spam == TRUE)
spamSparse = colSums(spamSparse)
sort(spamSparse)


######################################################################################################
# PROBLEM 3 - BUILDING MACHINE LEARNING MODELS

emailsSparse$spam = as.factor(emailsSparse$spam)
# Creating training and testing sets
library(caTools)
set.seed(123)
spl = sample.split(emailsSparse$spam, 0.7)
train = subset(emailsSparse, spl == TRUE)
test = subset(emailsSparse, spl == FALSE)

# Building a logistic regression model
spamLog = glm(spam ~ ., data=train, family=binomial)

# Building CART model
library(rpart)
library(rpart.plot)
spamCART = rpart(spam ~., data=train, method="class")

# Building a random forest model
set.seed(123)
library(randomForest)
spamRF = randomForest(spam ~ ., data=train)

# Prediction on training set
predTrainLog = predict(spamLog, newdata=train, type="response")
predTrainCART = predict(spamCART, newdata=train)
predTrainRF = predict(spamRF, newdata=train, type="prob")

# How many of the training set predicted probabilities from spamLog are less than 0.00001?
length(which(predTrainLog < 0.00001))
# How many of the training set predicted probabilities from spamLog are more than 0.99999?
length(which(predTrainLog > 0.99999))
# How many of the training set predicted probabilities from spamLog are between 0.00001 and 0.99999?
length(which(predTrainLog > 0.00001 & predTrainLog < 0.99999))

#How many of the word stems "enron", "hou", "vinc", and "kaminski" appear in the CART tree?
prp(spamCART)
# What is the training set accuracy of spamLog, using a threshold of 0.5 for predictions?
table(train$spam, predTrainLog >= 0.5)
# What is the training set AUC of spamLog?
library(ROCR)
ROCRpred = prediction(predTrainLog, train$spam)
as.numeric(performance(ROCRpred, "auc")@y.values)

# What is the training set accuracy of spamCART, using a threshold of 0.5 for predictions?
predTrainCART = predTrainCART[, 2]
table(train$spam, predTrainCART >= 0.5)
# What is the training set AUC of spamCART?
ROCRpred = prediction(predTrainCART, train$spam)
as.numeric(performance(ROCRpred, "auc")@y.values)

# What is the training set accuracy of spamRF, using a threshold of 0.5 for predictions?
table(train$spam, predTrainRF[, 2] >= 0.5)
# What is the training set AUC of spamRF?
ROCRpred = prediction(predTrainRF[, 2], train$spam)
as.numeric(performance(ROCRpred, "auc")@y.values)


######################################################################################################
# PROBLEM 4 - EVALUATING ON THE TEST SET
predTestLog = predict(spamLog, newdata=test, type="response")
predTestCART = predict(spamCART, newdata=test)
predTestRF = predict(spamRF, newdata=test, type="prob")

# Logistic regression model
# What is the testing set accuracy of spamLog, using a threshold of 0.5 for predictions?
table(test$spam, predTestLog >= 0.5)
# What is the testing set AUC of spamLog?
ROCRpred = prediction(predTestLog, test$spam)
as.numeric(performance(ROCRpred, "auc")@y.values)

# CART model
# What is the testing set accuracy of spamCART, using a threshold of 0.5 for predictions?
table(test$spam, predTestCART[, 2] >= 0.5)
# What is the testing set AUC of spamCART?
ROCRpred = prediction(predTestCART[, 2], test$spam)
as.numeric(performance(ROCRpred, "auc")@y.values)

# Random Forest model
# What is the testing set accuracy of spamRF, using a threshold of 0.5 for predictions?
table(test$spam, predTestRF[, 2] >= 0.5)
# What is the testing set AUC of spamRF?
ROCRpred = prediction(predTestRF[, 2], test$spam)
as.numeric(performance(ROCRpred, "auc")@y.values)


######################################################################################################
# PROBLEM 5 - ASSIGNING WEIGHTS TO DIFFERENT TYPES OF ERRORS
# Tests only


######################################################################################################
# PROBLEM 6 - INTEGRATING WORD COUNT INFORMATION
# Counting number of words in each email
wordCount = rowSums(as.matrix(dtm))
# What best describes the distribution of the data?
hist(wordCount)

hist(log(wordCount))
logWordCount = log(wordCount)
# Adding new variable to emailsSparse
emailsSparse$logWordCount = log(wordCount)
boxplot(logWordCount ~ emails$spam)

# Creating new train and test data sets
train2 = subset(emailsSparse, spl == TRUE)
test2 = subset(emailsSparse, spl == FALSE)

# Creating CART model
spam2CART = rpart(spam ~., data=train2, method="class")

# Creating random forest model
set.seed(123)
spam2RF = randomForest(spam ~ ., data=train2)

# Was the new variable used in the new CART tree spam2CART?
prp(spam2CART)

# Perform test-set predictions using the new CART and random forest models.
predTest2CART = predict(spam2CART, newdata=test2)
predTest2RF = predict(spam2RF, newdata=test2, type="prob")
# spam2CART model accuracy and AUC
table(test2$spam, predTest2CART[, 2] >= 0.5)
ROCRpred = prediction(predTest2CART[, 2], test$spam)
as.numeric(performance(ROCRpred, "auc")@y.values)
# spam2RF model accuracy and AUC
table(test2$spam, predTest2RF[, 2] >= 0.5)
ROCRpred = prediction(predTest2RF[, 2], test$spam)
as.numeric(performance(ROCRpred, "auc")@y.values)


######################################################################################################
# PROBLEM 7.1 - USING 2-GRAMS TO PREDICT SPAM
#! install.packages("RTextTools")
library(RTextTools)
dtm2gram = create_matrix(as.character(corpus), ngramLength=2)
spdtm2gram = removeSparseTerms(dtm2gram, 0.95)

# Building data from 2-grams
emailsSparse2gram = as.data.frame(as.matrix(spdtm2gram))
colnames(emailsSparse2gram) = make.names(colnames(emailsSparse2gram))
emailsCombined = cbind(emailsSparse, emailsSparse2gram)
# Creating training and testing sets
trainCombined = subset(emailsCombined, spl == TRUE)
testCombined = subset(emailsCombined, spl == FALSE)
# Creating CART model
spamCARTcombined = rpart(spam ~., data=trainCombined, method="class")
# Creating random forest model
set.seed(123)
spamRFcombined = randomForest(spam ~ ., data=trainCombined)

# How many 2-grams were used as splits in spamCARTcombined? 
prp(spamCARTcombined)

# Perform test-set predictions using the new CART and random forest models.
predTestCARTcombined = predict(spamCARTcombined, newdata=testCombined)
predTestRFcombined = predict(spamRFcombined, newdata=testCombined, type="prob")
# spam2CART model accuracy and AUC
table(testCombined$spam, predTestCARTcombined[, 2] >= 0.5)
ROCRpred = prediction(predTestCARTcombined[, 2], test$spam)
as.numeric(performance(ROCRpred, "auc")@y.values)
# spam2RF model accuracy and AUC
table(testCombined$spam, predTestRFcombined[, 2] >= 0.5)
ROCRpred = prediction(predTestRFcombined[, 2], test$spam)
as.numeric(performance(ROCRpred, "auc")@y.values)



