#!/usr/bin/Rscript

# AUTOMATING REVIEWS IN MEDICINE
######################################################################################################
# PROBLEM 1 - LOADING THE DATA
# Loading data
trials = read.csv("clinical_trial.csv", stringsAsFactors=FALSE)
summary(trials)
str(trials)
# How many characters are there in the longest abstract?
max(nchar(trials$abstract))

# How many search results provided no abstract? 
str(subset(trials, nchar(trials$abstract) == 0))
# What is the shortest title of any article?
trials$title[which.min(nchar(trials$title))]

######################################################################################################
# PROBLEM 2 - PREPARING THE CORPUS
library(tm)
# Preprocessing title variable
corpusTitle = Corpus(VectorSource(trials$title))
corpusTitle = tm_map(corpusTitle, tolower)
corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))
corpusTitle = tm_map(corpusTitle, stemDocument)
dtmTitle = DocumentTermMatrix(corpusTitle)

# Preprocessing abstract variable
corpusAbstract = Corpus(VectorSource(trials$abstract))
corpusAbstract = tm_map(corpusAbstract, tolower)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords("english"))
corpusAbstract = tm_map(corpusAbstract, stemDocument)
dtmAbstract = DocumentTermMatrix(corpusAbstract)

# Limit dtmTitle and dtmAbstract to terms with sparseness of at most 95%
dtmTitle = removeSparseTerms(dtmTitle, 0.95)
dtmAbstract = removeSparseTerms(dtmAbstract, 0.95)

# How many terms remain in dtmTitle and dtmAbstract after removing sparse terms
dtmTitle
dtmAbstract

#
dtmAbstractMatrix = as.data.frame(as.matrix(dtmAbstract))
which.max(colSums(dtmAbstractMatrix))


######################################################################################################
# PROBLEM 3 - BUILDING A MODEL
colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))
dtm = cbind(dtmTitle, dtmAbstract)

dtmAbstractMatrix= as.data.frame(as.matrix(dtmAbstract))
dtmTitleMatrix = as.data.frame(as.matrix(dtmTitle))
dtm = cbind(dtmTitleMatrix, dtmAbstractMatrix)
dtm$trial = trials$trial

# How many columns are in this combined data frame?
str(dtm)

# Splitting data
library(caTools)
set.seed(144)
spl = sample.split(dtm$trial, 0.7)
train = subset(dtm, spl == TRUE)
test = subset(dtm, spl == FALSE)
# What is the accuracy of the baseline model on the training set?
table(train$trial)

# Building a CART model
library(rpart)
library(rpart.plot)
trialCART = rpart(trial ~., data=train, method="class")
# What is the name of the first variable the model split on?
prp(trialCART)

# What is the maximum predicted probability for any result?
predTrain = predict(trialCART, newdata=train, method="class")
predTrain = predTrain[, 2]
max(predTrain)

# What is the training set accuracy of the CART model?
table(train$trial, predTrain >= 0.5)
# 0.8233487
# Sensitivity (TPR) and Specificity (SPC)
TPR = 441/(131+441)
SPC = 631/(631+99)

# Evaluate the CART model on the testing set
# What is the testing set accuracy, assuming a probability threshold of 0.5
predTest = predict(trialCART, newdata=test, method="class")
predTest = predTest[, 2]
table(test$trial, predTest >= 0.5)

# Using the ROCR package, what is the testing set AUC of the prediction model?
library(ROCR)
predROCR = prediction(predTest, test$trial)
perfROCR = performance(predROCR, "tpr", "fpr")
#plot(perfROCR, colorize=TRUE)
performance(predROCR, "auc")@y.values

######################################################################################################
# PART 5: DECISION-MAKER TRADEOFFS










