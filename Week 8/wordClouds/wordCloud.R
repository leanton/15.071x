#!/usr/bin/Rscript

# installing required packages
install.packages("tm")
library(tm)
install.packages("SnowballC")
library(SnowballC)
install.packages("wordcloud")
library(wordcloud)

# VISUALIZING TEXT DATA USING WORD CLOUDS
###############################################################################
# PROBLEM 1 - PREPARING THE DATA
# Loading data
tweets = read.csv("tweets.csv")
# Preprocessing the data
corpus = Corpus(VectorSource(tweets$Tweet))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c(stopwords("english")))
# Build a document-term matrix out of the corpus
frequencies = DocumentTermMatrix(corpus)
#
allTweets = as.data.frame(as.matrix(frequencies))
str(allTweets)


###############################################################################
# PROBLEM 2 - BUILDING A WORD CLOUD
vecTweets = colSums(allTweets)
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25))
# Deleting the most frequentword
allTweets$apple = NULL
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25))

wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25), rot.per=.6, random.order=FALSE)
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25), random.order=FALSE, colors=c("red", "green", "blue"))

# Playing with a color
install.packages("RColorBrewer")
library(RColorBrewer)
brewer.pal() 
display.brewer.all() 

library(wordcloud)
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25), colors=brewer.pal(9, "Blues"))
brewer.pal(9, "Blues")
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25), colors=brewer.pal(9, "Blues")[c(-5, -6, -7, -8, -9)])
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25), colors=brewer.pal(9, "Blues")[c(-1, -2, -3, -4)])



