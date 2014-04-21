#!/usr/bin/Rscript

#################################################################################
# Loading data
train = read.csv("train.csv", na.string="")
old.train = read.csv("train.csv")

# Importing YOB from old.train
train$YOB = old.train$YOB

############################
# PREPARING FOR IMPUTING

# train set first
# Removing dependent variables
train$UserID = NULL

# Removing variables with >2 factors
train$Income = NULL
train$HouseholdStatus = NULL
train$EducationLevel = NULL
train$Party = NULL

library(mice)
imp.train = complete(mice(train))

# Adding train variables with NA -> Skipped
train = read.csv("train.csv")
for (i in names(train)) {
    levels(train[,i]) <- c(levels(train[,i]), "Skipped")
    train[,i][train[,i] == ''] <- 'Skipped'
    train[,i] = factor(train[,i])
}

imp.train$Income = train$Income
imp.train$HouseholdStatus = train$HouseholdStatus
imp.train$EducationLevel = train$EducationLevel
imp.train$Party = train$Party

write.csv(imp.train, "trainAllImputed.csv")


#################################################################################
############################
# Test set
test = read.csv("test.csv", na.string="")
old.test = read.csv("test.csv")
test$YOB = old.test$YOB

test$UserID = NULL

# Removing variables with >2 factors
test$Income = NULL
test$HouseholdStatus = NULL
test$EducationLevel = NULL
test$Party = NULL

#library(mice)
imp.test = complete(mice(test))

# Adding test variables with NA -> Skipped
test = read.csv("test.csv")
for (i in names(test)) {
    levels(test[,i]) <- c(levels(test[,i]), "Skipped")
    test[,i][test[,i] == ''] <- 'Skipped'
    test[,i] = factor(test[,i])
}

imp.test$Income = test$Income
imp.test$HouseholdStatus = test$HouseholdStatus
imp.test$EducationLevel = test$EducationLevel
imp.test$Party = test$Party

write.csv(imp.test, "testAllImputed.csv")

# Training data with logistic regression
glm1 = glm(Happy ~ ., data=train, family=binomial)
predictTest = predict(glm1, newdata=test, type="response")

# Making csv file
submission = data.frame(UserID = old.test$UserID, Probability1 = predictTest)
write.csv(submission, "submissionImputedAllLog.csv", row.names=FALSE)


# Choosing significant variables
glm2 = glm(Happy ~ Q122769 + Q121700 + Q121011 + Q120194 + Q120014 + Q119334 + Q118892 + Q118233 + Q118237 + Q117186 + Q116881 + Q116953 + Q116441 + Q116197 + Q115602 + Q115610 + Q115899 + Q115390 + Q114961 + Q114748 + Q114386 + Q111848 + Q109367 + Q108855 + Q108342 + Q108343 + Q107869 + Q106389 + Q103293 + Q102906 + Q102687 + Q102289 + Q101162 + Q100680 + Q100562 + Q99716 + Q99581 + Q98869 + votes + HouseholdStatus + EducationLevel, data=train, family=binomial)
predictTest2 = predict(glm2, newdata=test, type="response")
submission = data.frame(UserID = old.test$UserID, Probability1 = predictTest2)
write.csv(submission, "submissionImputedAllLog.csv", row.names=FALSE)




