#!/usr/bin/Rscript

#################################################################################
# Loading imputed data
train = read.csv("train_imputed.csv")
test = read.csv("test_imputed.csv")

trainLog2 = glm(Happy ~ HouseholdStatus + EducationLevel + Party + Q122769 + Q122770 + Q121700 + Q121011 + Q120194 + Q120012 + Q120014 + Q119334 + Q119650 + Q118237 + Q116797 + Q116881 + Q116441 + Q116197 + Q115610 + Q115611 + Q115899 + Q115390 + Q114961 + Q114386 + Q113992 + Q113583 + Q113584 + Q111848 + Q110740 + Q109367 + Q108855 + Q108617 + Q108856 + Q108754 + Q108342 + Q108343 + Q107869 + Q106388 + Q106389 + Q102906 + Q102674 + Q102687 + Q102289 + Q101162 + Q100680 + Q100562 + Q99716 + Q98869, data=train, family=binomial)
predictTrain2 = predict(trainLog2, newdata=train, type="response")
predictTest2 = predict(trainLog2, newdata=test, type="response")

for (i in seq(1, length(predictTest2), 1)) {
	if (predictTest2[i] > 0.8) predictTest2[i] = 1
	if (predictTest2[i] < 0.1) predictTest2[i] = 0
}
predictTest2

submission = data.frame(UserID = test$UserID, Probability1 = predictTest2)
write.csv(submission, "submissionLogisticTradeoff.csv", row.names=FALSE)

# Doesn't work at all


# Clustering
train.limited = data.frame(train$YOB, train$votes)
test.limited = data.frame(train$YOB, train$votes)

library(caret)
preproc = preProcess(train.limited)
train.norm = predict(preproc, train.limited)
test.norm = predict(preproc, test.limited)

distances = dist(train.norm, method = "euclidean")
clusterTrain = hclust(distances, method = "ward")
km = kmeans(train.norm, centers = 3)
library(flexclust)
km.kcca = as.kcca(km, train.norm)
cluster.train = predict(km.kcca)
cluster.test = predict(km.kcca, newdata=test.norm)
# splitting data
train1 = subset(train, cluster.train==1)
train2 = subset(train, cluster.train==2)
train3 = subset(train, cluster.train==3)
test1 = subset(test, cluster.test==1)
test2 = subset(test, cluster.test==2)
test3 = subset(test, cluster.test==3)
# Training each cluster
glm1 = glm(Happy ~ ., data=train1, family=binomial)
glm2 = glm(Happy ~ ., data=train2, family=binomial)
glm3 = glm(Happy ~ ., data=train3, family=binomial)

pred.test1 = predict(glm1, newdata=test1, type="response")
pred.test2 = predict(glm2, newdata=test2, type="response")
pred.test3 = predict(glm3, newdata=test3, type="response")

all.predictions = c(pred.test1, pred.test2, pred.test3)
all.predictions = all.predictions[!is.na(all.predictions)]
submission = data.frame(UserID = test$UserID, Probability1 = all.predictions)
write.csv(submission, "submissionClusteringLogistic.csv", row.names=FALSE)
# 0.48342!!!!! BULLSHIT, maybe problem with test variables that recognized not so good


