library(class)
library(ggplot2)
library(caret)
library(lattice)
library(tidyverse)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tm)
library(slam)
library(e1071)
library(ISLR)
library(magrittr)
library(naivebayes)
library(wordcloud)
url <- "https://raw.githubusercontent.com/HAN-M3DM-Data-Mining/assignments/master/datasets/NB-reddit-hate-speech.csv"
rawDF <- read_csv(url)
head(rawDF)
str(rawDF)
cleanDF <- rawDF[-1]
head(cleanDF)
str(rawDF)
cleanDF$hate_speech_idx <- cleanDF$hate_speech_idx %>% factor 
rawCorpus <- Corpus(VectorSource(cleanDF$text))
inspect(rawCorpus[1:3])
cleanCorpus <- rawCorpus %>% tm_map(tolower) %>% tm_map(removeNumbers)
cleanCorpus <- cleanCorpus %>% tm_map(tolower) %>% tm_map(removeWords, stopwords()) %>% tm_map(removePunctuation)
cleanCorpus <- cleanCorpus %>% tm_map(stripWhitespace)
tibble(Raw = rawCorpus$content[1:3], Clean = cleanCorpus$content[1:3])
cleanDTM <- cleanCorpus %>% DocumentTermMatrix
inspect(cleanDTM)
set.seed(1234)
trainIndex <- createDataPartition(rawDF$hate_speech_idx, p = .75, 
                                  list = FALSE, 
                                  times = 1)
head(trainIndex)
trainDF <- rawDF[trainIndex, ]
testDF <- rawDF[-trainIndex, ]
trainCorpus <- cleanCorpus[trainIndex]
testCorpus <- cleanCorpus[-trainIndex]
trainDTM <- cleanDTM[trainIndex, ]
testDTM <- cleanDTM[-trainIndex, ]
freqWords <- trainDTM %>% findFreqTerms(5)
trainDTM <-  DocumentTermMatrix(trainCorpus, list(dictionary = freqWords))
testDTM <-  DocumentTermMatrix(testCorpus, list(dictionary = freqWords))
convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0) %>% factor(levels = c(0,1), labels = c("No", "Yes"))
}

nColsDTM <- dim(trainDTM)[2]
trainDTM <- apply(trainDTM, MARGIN = 2, convert_counts)
testDTM <- apply(testDTM, MARGIN = 2, convert_counts)

head(trainDTM[,1:10])

nbayesModel <-  naiveBayes(trainDTM, trainDF$text, laplace = 1)
predVec <- predict(nbayesModel, testDTM)
confusionMatrix(predVec, testDF$text, positive = "hate", dnn = c("Prediction", "True"))
