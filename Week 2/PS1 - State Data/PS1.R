# Loading data
data(state)
statedata = cbind(data.frame(state.x77), state.abb, state.area, state.center,  state.division, state.name, state.region)
str(statedata) # inspecting data

# PROBLEM 1.1 - DATA EXPLORATION
# We begin by exploring the data by examining the latitude and longitude of each state. Plot all of the states' centers with latitude on the y axis (the "y" variable in our dataset) and longitude on the x axis (the "x" variable in our dataset). The shape of the plot should be the familiar outline of the United States! Note that Alaska and Hawaii have had their coordinates adjusted to appear just off of the west coast.

# In the R command you used to generate this plot, which variable name did you use as the first argument?
plot(statedata$x, statedata$y)

# PROBLEM 1.2 - DATA EXPLORATION
# Using the tapply command, determine which region of the US (West, North Central, South, or Northeast) has the highest average high school graduation rate of all the states in the region:
tapply(statedata$HS.Grad, statedata$state.region, mean)

# PROBLEM 1.3 - DATA EXPLORATION
# Now, make a boxplot of the murder rate by region (for more information about creating boxplots in R, type ?boxplot in your console).
# Which region has the highest median murder rate?
tapply(statedata$Murder, statedata$state.region, median)
boxplot(statedata$Murder ~ statedata$state.region)

# PROBLEM 1.4 - DATA EXPLORATION
# You should see that there is an outlier in the Northeast region of the boxplot you just generated. Which state does this correspond to? (Hint: There are many ways to find the answer to this question, but one way is to use the subset command to only look at the Northeast data.)
northeast = subset(statedata, statedata$state.region=="Northeast")
boxplot(northeast$Murder ~ northeast$state.name)

# PROBLEM 2.1 - PREDICTING LIFE EXPECTANCY - AN INITIAL MODEL
# We would like to build a model to predict life expectancy by state using the state statistics we have in our dataset.
# Build the model with all potential variables included (Population, Income, Illiteracy, Murder, HS.Grad, Frost, and Area). Note that you should use the variable "Area" in your model, NOT the variable "state.area".
# What is coefficient for income?
lifeExpReg = lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost + Area, data=statedata)
summary(lifeExpReg)

# PROBLEM 2.3 - PREDICTING LIFE EXPECTANCY - AN INITIAL MODEL
# Now plot a graph of life expectancy vs. income using the command:
plot(statedata$Income, statedata$Life.Exp)
# Visually observe the plot. What appears to be the relationship?

# PROBLEM 3.1 - PREDICTING LIFE EXPECTANCY - REFINING THE MODEL AND ANALYZING PREDICTIONS
# You should be able to find a good model with only 4 independent variables, instead of the original 7. Which variables does this model contain?
lifeExpReg = lm(Life.Exp ~ Income + HS.Grad + Frost + Murder , data=statedata)
summary(lifeExpReg)
lifeExpReg = lm(Life.Exp ~ HS.Grad + Population + Income + Frost, data=statedata)
summary(lifeExpReg)
lifeExpReg = lm(Life.Exp ~ Frost + Murder + HS.Grad + Illiteracy, data=statedata)
summary(lifeExpReg)
lifeExpReg = lm(Life.Exp ~ Population + Murder + Frost + HS.Grad, data=statedata)
summary(lifeExpReg)
# Last one

# PROBLEM 3.2 - PREDICTING LIFE EXPECTANCY - REFINING THE MODEL AND ANALYZING PREDICTIONS
lifeExpReg = lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost + Area, data=statedata)
summary(lifeExpReg)
lifeExpReg = lm(Life.Exp ~ Population + Murder + Frost + HS.Grad, data=statedata)
summary(lifeExpReg)

# PROBLEM 3.3 - PREDICTING LIFE EXPECTANCY - REFINING THE MODEL AND ANALYZING PREDICTIONS
# Which state do we predict to have the lowest life expectancy? (Hint: use the sort function)
sort(predict(lifeExpReg))
# Which state actually has the lowest life expectancy? (Hint: use the which.min function)
statedata$state.name[which.min(statedata$Life.Exp)]

# PROBLEM 3.4 - PREDICTING LIFE EXPECTANCY - REFINING THE MODEL AND ANALYZING PREDICTIONS
# Which state do we predict to have the highest life expectancy?
sort(predict(lifeExpReg))
# Which state actually has the highest life expectancy?
statedata$state.name[which.max(statedata$Life.Exp)]

# PROBLEM 3.5 - PREDICTING LIFE EXPECTANCY - REFINING THE MODEL AND ANALYZING PREDICTIONS
# Take a look at the vector of residuals (the difference between the predicted and actual values).
# For which state do we make the smallest absolute error?
# For which state do we make the largest absolute error?
sort(lifeExpReg$residuals)