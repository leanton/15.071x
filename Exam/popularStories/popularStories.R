#!/usr/bin/Rscript

# PREDICTING THE POPULARITY OF NEWS STORIES
############################################################################
# PROBLEM 1 - LOADING THE DATASET
stories = read.csv("nytimes.csv", stringsAsFactors=FALSE)
# Proportion of popular stories
nrow(subset(stories, stories$popular==1))/nrow(stories)
# PROBLEM 2 - COMPUTING A CORRELATION
cor(nchar(stories$headline), stories$popular)

# PROBLEM 3 - CONVERTING VARIABLES TO FACTORS
# Converting "popular" and "type" as factors
stories$popular = as.factor(stories$popular)
stories$type = as.factor(stories$type)

# PROBLEM 4 - SPLITTING INTO A TRAINING AND TESTING SET
library(caTools)
set.seed(144)
spl = sample.split(stories$popular, SplitRatio = 0.7)
train = subset(stories, spl==TRUE)
test = subset(stories, spl==FALSE)

############################################################################
# PROBLEM 5 - TRAINING A LOGISTIC REGRESSION MODEL
glmTrain = glm(popular ~ print + type + word.count, data=train, family=binomial)
summary(glmTrain)

# PROBLEM 6 - PREDICTING USING A LOGISTIC REGRESSION MODEL
sc = 1.181e-01 - 1.095e-01 + 5.566e-02 + 682*4.260e-05

# PROBLEM 8 - OBTAINING TEST SET PREDICTIONS
predictions = predict(glmTrain, newdata=test, type="response")
table(predictions>0.5, test$popular)

# PROBLEM 9 - COMPUTING TEST SET AUC  
library(ROCR)
ROCRpred = prediction(predictions, test$popular)
as.numeric(performance(ROCRpred, "auc")@y.values)

# PROBLEM 11 - ROC CURVES
library(ROCR)
PredictROC = predict(glmTrain, newdata=test)
pred = prediction(PredictROC, test$popular)
perf = performance(pred, "tpr", "fpr")
plot(perf)

################################################################################
# PROBLEM 14 - CROSS-VALIDATION FOR A CART MODEL
set.seed(144)
library(caret)
library(e1071)
library(rpart)
library(rpart.plot)
fitControl = trainControl( method = "cv", number = 10 )
cartGrid = expand.grid( .cp = (1:50)*0.01)
train(popular ~ print + type + word.count, data = train, method = "rpart", trControl = fitControl, tuneGrid = cartGrid )

# PROBLEM 15 - TRAIN CART MODEL
tree = rpart(popular ~ print + type + word.count, method="class", data = train, control=rpart.control(cp = 0.01))
prp(tree)

################################################################################
# PROBLEM 16 - BUILDING A CORPUS FROM ARTICLE SNIPPETS
library(tm)
corpus = Corpus(VectorSource(stories$snippet))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
dtm = DocumentTermMatrix(corpus)
# How many unique word stems are in dtm?
dtm
spdtm = removeSparseTerms(dtm, 0.95)
# How many unique terms are in spdtm?
spdtm

# PROBLEM 18 - EVALUATING WORD FREQUENCIES IN A CORPUS
articleText = as.data.frame(as.matrix(spdtm))
which.max(colSums(articleText))

# PROBLEM 19 - ADDING DATA FROM ORIGINAL DATA FRAME
articleText$print = stories$print
articleText$type = stories$type
articleText$word.count = stories$word.count
articleText$popular = stories$popular
trainText = subset(articleText, spl==TRUE)
testText = subset(articleText, spl==FALSE)

# PROBLEM 20 - TRAINING ANOTHER LOGISTIC REGRESSION MODEL
glmText = glm(popular ~ ., data=trainText, family=binomial)
summary(glmText)


library(ROCR)
predictionsText = predict(glmText, newdata=testText, type="response")
ROCRpred = prediction(predictionsText, testText$popular)
as.numeric(performance(ROCRpred, "auc")@y.values)









