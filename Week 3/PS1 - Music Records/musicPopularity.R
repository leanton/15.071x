#!/usr/bin/Rscript

# POPULARITY OF MUSIC RECORDS
# PROBLEM 1 - UNDERSTANDING THE DATA
# Loading DATA

music = read.csv("songs.csv")
str(music)
# How many observations (songs) are from the year 2010?
nrow(subset(music, music$year == 2010))

# How many songs does the dataset include for which the artist name is "Michael Jackson"?
nrow(subset(music, music$artistname == "Michael Jackson"))

# Which of these songs by Michael Jackson made it to the Top 10?
top10MJ = subset(music, music$artistname == "Michael Jackson" & music$Top10 == 1)
top10MJ$songtitle

# The variable corresponding to the estimated time signature (timesignature) is discrete,
# meaning that it only takes integer values (0, 1, 2, 3, . . . ). 
# What are the values of this variable that occur in our dataset?
hist(music$timesignature)

# Out of all of the songs in our dataset, the song with the highest tempo is one of the following songs. Which one is it?
subset(music, music$tempo == max(music$tempo))$songtitle


# PROBLEM 2 - CREATING OUR PREDICTION MODEL
# Building training and test sets
SongsTrain = subset(music, music$year <= 2009)
SongsTest = subset(music, music$year >= 2010)

# How many observations (songs) are in the training set?
nrow(SongsTrain)

# Making model
# SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)

# excluding some independent variables
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]

SongsLog = glm(Top10 ~ ., data=SongsTrain, family=binomial)
summary(SongsLog)

# PROBLEM 3 - BEWARE OF MULTICOLLINEARITY ISSUES!
# What is the correlation between the variables "loudness" and "energy" in the training set?
cor(SongsTrain$energy,SongsTrain$loudness)

# Creating new model without multicollinearity
SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
summary(SongsLog2)

#  create Model 3, which should be exactly like Model 1, but without the variable "energy".
SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(SongsLog3)

# PROBLEM 4 - VALIDATING OUR MODEL
# Make predictions on the test set using Model 3. What is the accuracy of Model 3 on the test set, using a threshold of 0.45?
pred1 = predict(SongsLog3, type = "response", newdata=SongsTest)
table(SongsTest$Top10, pred1 >= 0.45)
# (TP + TN) / N

# Baseline model
table(SongsTest$Top10)

# How many songs does Model 3 correctly predict as Top 10 hits in 2010, using a threshold of 0.45?
table(SongsTest$Top10, pred1 > 0.45) # TP and FP
# Sensitivity = TP/( TP + FN)
# Specificity = TN/( TN + FP)