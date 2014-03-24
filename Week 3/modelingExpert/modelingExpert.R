# Week 3, Modeling Expert, Problem Set solving
# Installing additional packages
# install.packages("caTools")

# Loading data
quality = read.csv("quality.csv")
library(caTools)
set.seed(88)
split = sample.split(quality$PoorCare, SplitRatio = 0.75)
qualityTrain = subset(quality, split == TRUE)
qualityTest = subset(quality, split == FALSE)

# Building Model
QualityLog = glm(PoorCare ~ StartedOnCombination + ProviderCount, data=qualityTrain, family=binomial)
summary(QualityLog)

# Building new Model for PS7
library(ROCR)
QualityLog2 = glm(PoorCare ~ OfficeVisits + Narcotics, data=qualityTrain, family=binomial)
predictTest = predict(QualityLog2, type="response", newdata=qualityTest)
ROCRpredTest = prediction(predictTest, qualityTest$PoorCare)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc