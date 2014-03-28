#!/usr/bin/Rscript

# UNDERSTANDING WHY PEOPLE VOTE
# PROBLEM 1.1 - EXPLORATION AND LOGISTIC REGRESSION
# Loading data
vote = read.csv("gerber.csv")
str(vote)

# What proportion of people in this dataset voted in this election?
nrow(subset(vote, vote$voting == 1))/nrow(vote)

# Which of the four "treatment groups" had the largest fraction of voters?
nrow(subset(vote, vote$civicduty == 1)) # Civic Duty
nrow(subset(vote, vote$hawthorne == 1)) # Hawthorne Effect
nrow(subset(vote, vote$self == 1)) # Self
nrow(subset(vote, vote$neighbors == 1)) # Neighbors

tapply(vote$voting, vote$civicduty, mean)
tapply(vote$voting, vote$hawthorne, mean)
tapply(vote$voting, vote$self, mean)
tapply(vote$voting, vote$neighbors, mean) # THIS

# Build a logistic regression model for voting using the four treatment group variables as the independent variables
voteLog = glm(voting ~ civicduty + hawthorne + self + neighbors, data=vote, family=binomial)
summary(voteLog)

# Using a threshold of 0.3, what is the accuracy of the logistic regression model?
predictVote = predict(voteLog, type="response")
table(vote$voting, predictVote>=0.3)
# (134513 + 51966)/(134513 + 100875 + 56730 + 51966)
table(vote$voting, predictVote>=0.5)
# 235388/(235388+108696)

# PROBLEM 2 - TREES
library(rpart)
library(rpart.plot)
CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=vote)
prp(CARTmodel)

CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=vote, cp=0.0)
prp(CARTmodel2)

# Make a new tree that includes the "sex" variable, again with cp = 0.0. 
# Notice that sex appears as a split that is of secondary importance to the treatment group.

CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=vote, cp=0.0)
prp(CARTmodel3)

# PROBLEM 3 - INTERACTION TERMS
CARTmodel4 = rpart(voting ~ control, data=vote, cp=0.0)
prp(CARTmodel4, digits=6)
CARTmodel5 = rpart(voting ~ control + sex, data=vote, cp=0.0)
prp(CARTmodel5, digits=6)

# Creating logistic regression
voteLog2 = glm(voting ~ control + sex, data=vote, family=binomial)
summary(voteLog2)

Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(voteLog2, newdata=Possibilities, type="response")
# 0.290456 - 0.2908065
LogModel2 = glm(voting ~ sex + control + sex:control, data=vote, family="binomial")
summary(LogModel2)
predict(LogModel2, newdata=Possibilities, type="response")
# 0.290456 - 0.2904558
