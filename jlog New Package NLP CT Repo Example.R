##Load Libraries

library(ggplot2)
library(tidyverse)
library(dplyr)
library(readr)
library(cowplot)
library(blorr)
library(olsrr)
library(readr)
library(caret)
library(pscl)
library(lmtest)
library(ipred)
library(survival)
library(ResourceSelection)
library(survey)
library(s2dverification)
library(lmtest)
library(pROC)
library(DescTools)

## NOTE: This is a proof of concept. Further validation work needs to take place.
##need to create train/datasets

data = read.csv('C:\\CTReportsJune2020\\radio01.csv', stringsAsFactors = F)
head(data) #allows you to check the data, first few entries
summary(data) #produce result summaries of the results of various model fitting functions.
dim(data) #the dimension (e.g. the number of columns and rows) of a matrix, array or data frame.
str(data)

data$report <- as.character(data$report)
data$abnormal <- as.integer(data$abnormal=="1")
str(data)

library(tm)
corpus <- Corpus(VectorSource(data$report))

corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stemDocument)
corpus <- tm_map(corpus, removeWords, c("the", "and", "are"))

library(wordcloud)
library(RColorBrewer)
wordcloud(corpus,max.words=150,random.order=FALSE, rot.per=0.15, colors=brewer.pal(8,"Dark2"))

freq <- DocumentTermMatrix(corpus)
freq

freq <- removeSparseTerms(freq, 0.90)
freq

findAssocs(freq, terms="report", corlimit=0.25)

newcorpus <- as.data.frame(as.matrix(freq))
colnames(newcorpus) <- make.names(colnames(newcorpus))
newcorpus$abnormal <- data$abnormal

library(caTools)
library(JLog)
set.seed(1)
split <- sample.split(newcorpus$abnormal, SplitRatio = 0.7)
datatrain <- subset(newcorpus, split==TRUE)
test <- subset(newcorpus, split==FALSE)
t=abnormal~.
jlog(t)
predictLog = predict(jlog(t), newdata=test, type="response")
table(test$abnormal, predictLog > 0.5)

# Accuracy is 85%
