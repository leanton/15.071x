#!/usr/bin/Rscript
# PREDICTING MEDICAL COSTS WITH CLUSTER-THEN-PREDICT

#########################################################################################
# PROBLEM 1 - PREPARING THE DATASET
# loading data
claims = read.csv("reimbursement.csv")
# How many Medicare beneficiaries are included in the dataset?
nrow(claims)

# What proportion of patients have at least one of the chronic conditions described in the independent variables...?
nrow(subset(claims, claims$alzheimers==1 | claims$arthritis==1 | claims$cancer==1 | claims$copd==1 | claims$depression==1 | claims$diabetes==1 | claims$heart.failure==1 | claims$ihd==1 | claims$kidney==1 | claims$osteoporosis==1 | claims$stroke==1))/nrow(claims)

# What is the maximum correlation between independent variables in the dataset?
sort(cor(claims))

# Plot the histogram of the dependent variable. What is the shape of the distribution?
hist(claims$reimbursement2009)

# log transform of reimbursement vars
claims$reimbursement2008 = log(claims$reimbursement2008+1)
claims$reimbursement2009 = log(claims$reimbursement2009+1)

# Plot the histogram of the log-transformed dependent variable.
hist(claims$reimbursement2009)

# What proportion of beneficiaries had $0 in reimbursements in 2009?
nrow(subset(claims, claims$reimbursement2009==0))/nrow(claims)


#########################################################################################
# PROBLEM 2 - INITIAL LINEAR REGRESSION MODEL
# Splitting data
set.seed(144)
spl = sample(1:nrow(claims), size=0.7*nrow(claims))
train = claims[spl,]
test = claims[-spl,]
# Building linear regression model
lm.claims = lm(reimbursement2009 ~ ., data=train)
# What is the training set Multiple R-squared value of lm.claims?
summary(lm.claims)

# Obtain testing set predictions from lm.claims. What is the testing set RMSE of the model?
lm.predTest = predict(lm.claims, newdata=test)
lm.RMSE = sqrt(sum((test$reimbursement2009 - lm.predTest)^2)/nrow(test))

# What is the testing set RMSE of the naive baseline model?
lm.baselineRMSE = sqrt(sum((test$reimbursement2009 - mean(train$reimbursement2009))^2)/nrow(test))

# What is the testing set RMSE of this smart baseline model?
lm.smartRMSE = sqrt(sum((test$reimbursement2009 - test$reimbursement2008)^2)/nrow(test))


#########################################################################################
# PROBLEM 3 - CLUSTERING MEDICARE BENEFICIARIES
# deleting dependent variable
train.limited = train
train.limited$reimbursement2009 = NULL
test.limited = test
test.limited$reimbursement2009 = NULL

# Preprocessing step: normalization by the mean and stdev of training set
library(caret)
preproc = preProcess(train.limited)
train.norm = predict(preproc, train.limited)
test.norm = predict(preproc, test.limited)
# What is the mean of the arthritis variable in train.norm/test.norm?
mean(train.norm$arthritis)
mean(test.norm$arthritis)

# k-means
set.seed(144)
km = kmeans(train.norm, centers = 3)
# older-than-average beneficiaries with below average incidence of stroke and above-average 2008 reimbursements
tapply(train.norm$age, km$cluster, mean)
tapply(train.norm$stroke, km$cluster, mean)
tapply(train.norm$reimbursement2008, km$cluster, mean)

# How many test-set observations were assigned to Cluster 2?
library(flexclust)
km.kcca = as.kcca(km, train.norm)
cluster.train = predict(km.kcca)
cluster.test = predict(km.kcca, newdata=test.norm)


#########################################################################################
# PROBLEM 4 - CLUSTER-SPECIFIC PREDICTIONS
# Splitting data
train1 = subset(train, cluster.train==1)
train2 = subset(train, cluster.train==2)
train3 = subset(train, cluster.train==3)
test1 = subset(test, cluster.test==1)
test2 = subset(test, cluster.test==2)
test3 = subset(test, cluster.test==3)
# Which training set data frame has the highest average value of the dependent variable?
c(mean(train1$reimbursement2009), mean(train2$reimbursement2009), mean(train3$reimbursement2009))

# Building linear regression models
lm1 = lm(reimbursement2009 ~ ., data=train1)
lm2 = lm(reimbursement2009 ~ ., data=train2)
lm3 = lm(reimbursement2009 ~ ., data=train3)

# Which variables have a positive sign for the coefficient in at least one of lm1, lm2, and lm3 and a negative sign for the coefficient in at least one of lm1, lm2, and lm3?
summary(lm1)
summary(lm2)
summary(lm3)

# Making predictions
pred.test1 = predict(lm1, newdata=test1)
pred.test2 = predict(lm2, newdata=test2)
pred.test3 = predict(lm3, newdata=test3)
# Which vector of test-set predictions has the smallest average predicted reimbursement amount?
c(mean(pred.test1), mean(pred.test2), mean(pred.test3))

# RMSE of test sets
RMSE1 = sqrt(mean((test1$reimbursement2009 - pred.test1)^2))
RMSE2 = sqrt(mean((test2$reimbursement2009 - pred.test2)^2))
RMSE3 = sqrt(mean((test3$reimbursement2009 - pred.test3)^2))
c(RMSE1, RMSE2, RMSE3)

# Overall test set RMSE
all.predictions = c(pred.test1, pred.test2, pred.test3)
all.outcomes = c(test1$reimbursement2009, test2$reimbursement2009, test3$reimbursement2009)
RMSE = sqrt(mean((all.outcomes - all.predictions)^2))
RMSE
