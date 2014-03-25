#!usr/bin/Rscript

# PREDICTING THE BASEBALL WORLD SERIES CHAMPION
# PROBLEM 1 - LIMITING TO TEAMS MAKING THE PLAYOFFS
# Loading data
baseball = read.csv("baseball.csv")

# How many team/year pairs are there in the whole dataset?
str(baseball)


baseball = subset(baseball,Playoffs==1)
# Which of the following has been the number of teams making the playoffs in some season?
table(baseball$Year)

# No comments below, just script
PlayoffTable = table(baseball$Year)
PlayoffTable
PlayoffTable = table(baseball$Year)
PlayoffTable
names(PlayoffTable)
str(PlayoffTable)
PlayoffTable[c("1990", "2001")]
baseball$NumCompetitors = PlayoffTable[as.character(baseball$Year)]
baseball1 = subset(baseball, NumCompetitors==8)
str(baseball1)
baseball$WorldSeries = as.numeric(baseball$RankPlayoffs == 1)
baseball2 = subset(baseball, WorldSeries==0)
str(baseball2)
mLg = glm(WorldSeries~Lg,data=baseball,family=”binomial”) 
summary(mNumLg)
cor(baseball$RankSeason,baseball$RA)