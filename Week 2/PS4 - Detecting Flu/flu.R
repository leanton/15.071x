#!/usr/bin/Rscript

# Loading data
# PROBLEM 1.1 - UNDERSTANDING THE DATA
FluTrain = read.csv("FluTrain.csv")
# Looking at the time period 2004-2011, which week corresponds to the highest percentage of ILI-related physician visits?
FluTrain$Week[which.max(FluTrain$ILI)]
FluTrain$Week[which.max(FluTrain$Queries)]

# PROBLEM 1.2 - UNDERSTANDING THE DATA
# Let us now understand the data at an aggregate level. Plot the histogram of the dependent variable, ILI. What best describes the distribution of values of ILI?
hist(FluTrain$ILI)

# Plot the natural logarithm of ILI versus Queries. What does the plot suggest?
plot(FluTrain$Queries ,log(FluTrain$ILI))

# PROBLEM 2.1 - LINEAR REGRESSION MODEL
# Based on our understanding of the data from the previous subproblem, which model best describes our estimation problem?
FluTrend1 = lm(log(ILI) ~ Queries, data=FluTrain)
summary(FluTrend1)

# For a single variable linear regression model, there is a direct relationship between the R-squared and the correlation between the independent and the dependent variables. What is the relationship we infer from our problem?
COR = cor(log(FluTrain$ILI), FluTrain$Queries)
COR^2

# PROBLEM 3.1 - PERFORMANCE ON THE TEST SET
# Loading test data
FluTest = read.csv("FluTest.csv")
PredTest1 = exp(predict(FluTrend1, newdata=FluTest))

# What is our estimate for the percentage of ILI-related physician visits for the week of March 11, 2012?
PredTest1[11]

# (Observed ILI - Estimated ILI)/Observed ILI for week 11
RE1 = (FluTest$ILI[11] - PredTest1[11])/FluTest$ILI[11]

# What is the Root Mean Square Error (RMSE) between our estimates and the actual observations for the percentage of ILI-related physician visits?
SSE1 = sum((FluTest$ILI - PredTest1)^2)
RMSE1 = sqrt(SSE1/nrow(FluTest))


# PROBLEM 4.1 - TRAINING A TIME SERIES MODEL
# Installing package
# install.packages("zoo")
# Loading package
library(zoo)

ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)
summary(FluTrain)

# Use the plot() function to plot the log of ILILag2 against the log of ILI. Which best describes the relationship between these two variables?
plot(log(FluTrain$ILI), log(FluTrain$ILILag2))

# Train a linear regression model on the FluTrain dataset to predict the log of the ILI variable using the Queries variable as well as the log of the ILILag2 variable. Call this model FluTrend2.
# Which coefficients are significant at the p=0.05 level in this regression model?
FluTrend2 = lm(log(ILI) ~ log(ILILag2) + Queries, data=FluTrain)
summary(FluTrend2)

# On the basis of R-squared value and significance of coefficients, which statement is the most accurate?
## the last one.

# PROBLEM 5.1 - EVALUATING THE TIME SERIES MODEL IN THE TEST SET
# Modify the code from the previous subproblem to add an ILILag2 variable to the FluTest data frame.
FluTest$ILILag2 = coredata(lag(zoo(FluTest$ILI), -2, na.pad=TRUE))

# Fill in the missing values for ILILag2 in FluTest.
FluTest$ILILag2[1] = FluTrain$ILI[nrow(FluTrain)-1]
FluTest$ILILag2[2] = FluTrain$ILI[nrow(FluTrain)]
FluTest$ILILag2[1]
FluTest$ILILag2[2]

# What is the test-set RMSE of the FluTrend2 model?
PredTest2 = exp(predict(FluTrend2, newdata=FluTest))
SSE2 = sum((FluTest$ILI - PredTest2)^2)
RMSE2 = sqrt(SSE2/nrow(FluTest))
RMSE2