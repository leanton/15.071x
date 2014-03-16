#!usr/bin/Rscript

# Loading data
pisaTrain = read.csv("pisa2009train.csv")
pisaTest = read.csv("pisa2009test.csv")

# PROBLEM 1.1 - DATASET SIZE
# How many students are there in the training set?
nrow(pisaTrain)
str(pisaTrain)

# PROBLEM 1.2 - SUMMARIZING THE DATASET
# Using tapply() on pisaTrain, what is the average reading test score of males/females?
tapply(pisaTrain$readingScore, pisaTrain$male, mean)
tapply(pisaTrain$readingScore, pisaTrain$female, mean)

# PROBLEM 1.3 - LOCATING MISSING VALUES
# Which variables are missing data in at least one observation in the training set?
summary(pisaTrain)

# PROBLEM 1.4 - REMOVING MISSING VALUES
# Type the following commands into your R console to remove observations with any missing value from pisaTrain and pisaTest:
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)
# How many observations are now in the training/test set?
str(pisaTrain)
str(pisaTest)

# PROBLEM 2.1 - FACTOR VARIABLES
# raceeth
# grade

# PROBLEM 2.2 - UNORDERED FACTORS IN REGRESSION MODELS
# all except ...

# PROBLEM 2.3 - EXAMPLE UNORDERED FACTORS

# PROBLEM 3.1 - BUILDING A MODEL
# Set the reference level of the factor by typing the following two lines in your R console:
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")
# What is the Multiple R-squared value of lmScore on the training set?
lmScore = lm(readingScore ~ ., data=pisaTrain)
summary(lmScore)

# PROBLEM 3.2 - COMPUTING THE ROOT-MEAN SQUARED ERROR OF THE MODEL
# What is the training-set root-mean squared error (RMSE) of lmScore?
summary(lmScore)

# PROBLEM 3.3 - COMPARING PREDICTIONS FOR SIMILAR STUDENTS
# PROBLEM 3.4 - INTERPRETING MODEL COEFFICIENTS
# PROBLEM 3.5 - IDENTIFYING VARIABLES LACKING STATISTICAL SIGNIFICANCE

# PROBLEM 4.1 - PREDICTING ON UNSEEN DATA
# What is the range between the maximum and minimum predicted reading score on the test set?
predTest = predict(lmScore, newdata=pisaTest)
summary(predTest)
max(predTest) - min(predTest)
# Compute R-squared
SSE = sum((pisaTest$readingScore - predTest)^2)
RMSE = sqrt(SSE/nrow(pisaTest))

# PROBLEM 4.3 - BASELINE PREDICTION AND TEST-SET SSE
# What is the predicted test score used in the baseline model? Remember to compute this value using the training set and not the test set.
bias = mean(pisaTrain$readingScore)
SST = sum((pisaTest$readingScore - bias)^2)

# PROBLEM 4.4 - TEST-SET R-SQUARED
# What is the test-set R-squared value of lmScore?
R2 = 1 - SSE/SST