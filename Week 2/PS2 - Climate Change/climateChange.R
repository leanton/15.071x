#!/usr/bin/Rscript

# Loading data
# The file climate_change.csv contains climate data from May 1983 to December 2008.
climate = read.csv("climate_change.csv")
str(climate)

# PROBLEM 1.1 - CREATING OUR FIRST MODEL
# Split the data into a training set, consisting of all the observations up to and including 2006, and a testing set consisting of the remaining years
climateTrain = subset(climate, climate$Year <= 2006)
climateTest = subset(climate, climate$Year > 2006)
# build a linear regression model using all of the independent variables (except Year and Month) to predict the dependent variable Temp. Enter the model R^2
climateReg = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data=climateTrain)

# Which variables are significant in the model? We will consider a variable signficant only if the p-value is below 0.05.
summary(climateReg)

# PROBLEM 2.1 - UNDERSTANDING THE MODEL
# Which of the following is the simplest correct explanation for this contradiction?
## Think yourself! :)

# PROBLEM 2.2 - UNDERSTANDING THE MODEL
# Compute the correlations between all the variables in the training set. Which of the following independent variables is N2O highly correlated with (absolute correlation greater than 0.7)?
cor(climateTrain)

# PROBLEM 3 - SIMPLIFYING THE MODEL
# Given that the correlations are so high, let us focus on the N2O variable and build a model with only MEI, TSI, Aerosols and N2O. Remember to use the training set to build the model.
climateReg2 = lm(Temp ~ MEI + N2O + TSI + Aerosols, data=climateTrain)
# Enter the coefficient of N2O in this reduced model:
summary(climateReg2)

# PROBLEM 4 - AUTOMATICALLY BUILDING THE MODEL
# Enter the R^2 value of the model produced by the step function:
stepModel = step(climateReg)
summary(stepModel)

# PROBLEM 5 - TESTING ON UNSEEN DATA
# Enter the testing set R2:
predictTest = predict(stepModel, newdata=climateTest)
summary(predictTest)
# Compute R-squared
SSE = sum((climateTest$Temp - predictTest)^2)
SST = sum((climateTest$Temp - mean(climateTrain$Temp))^2)
1 - SSE/SST