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

url <- "https://raw.githubusercontent.com/HAN-M3DM-Data-Mining/assignments/master/datasets/NB-fakenews.csv"
rawDF <- read.csv(url)
head(rawDF)

rawDF$label <- rawDF$label %>% factor %>% relevel("1")
class(rawDF$label)

rawDF$label <- rawDF$label %>% factor %>% relevel("0")
class(rawDF$label) 

fake <- rawDF %>% filter(label == "1")
real <- rawDF %>% filter(label == "0")

worldcloud(fake$text, max.words = 20, scale = c(4, 0.8), colors= c("indianred1","indianred2","indianred3","indianred"))
worldcloud(real$text, max.words = 20, scale = c(4, 0.8), colors= c("lightsteelblue1","lightsteelblue2","lightsteelblue3","lightsteelblue"))


rawCorpus <- Corpus(VectorSource(rawDF$text))
inspect(rawCorpus[1:3])
cleanCorpus <- rawCorpus %>% tm_map(tolower) %>% tm_map(removeNumbers)
cleanCorpus <- cleanCorpus %>% tm_map(tolower) %>% tm_map(removeWords, stopwords()) %>% tm_map(removePunctuation)
cleanCorpus <- cleanCorpus %>% tm_map(stripWhitespace)


tibble(Raw = rawCorpus$content[1:3], Clean = cleanCorpus$content[1:3])

cleanDM <- cleanCorpus %>% DocumentTermMatrix
inspect(cleanDTM)

set.seed(1234)
trainIndex <- createDataPartition(rawDF$label, p = .75, 
                                  list = FALSE, 
                                  times = 1)
head(trainIndex)

trainDF <- rawDF[trainIndex, ]
testDF <- rawDF[-trainIndex, ]

trainCorpus <- cleanCorpus[trainIndex]
testCorpus <- cleanCorpus[-trainIndex]

trainDTM <- cleanDTM[trainIndex, ]
testDTM <- cleanDTM[trainIndex, ]


freqWords <- trainDTM %>% findFreqTerms(100000)
trainDTM <-  DocumentTermMatrix(trainCorpus, list(dictionary = freqWords))
testDTM <-  DocumentTermMatrix(testCorpus, list(dictionary = freqWords))

convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0) %>% factors(levels = c(0,1), labels = c("No", "Yes"))
}
nColsDTM <- dim(trainDTM)[2]
trainDTM <- apply(trainDTM, MARGIN = 2, convert_counts)
testDTM <- apply(testDTM, MARGIN = 2, convert_counts)
head(trainDTM[,1:10])

nbayesModel <- naiveBayes(trainDTM, trainDF$label, laplace = 1)

predVec <- predict(nbayesModel, testDTM)

confusionMatrix(predVec, testDF$label, positive = "Yes", dnn = c("No", "yes"))
