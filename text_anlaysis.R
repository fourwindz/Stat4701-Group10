setwd("D:/Google Drive/Courses_G/1B_Exploratory Data Analysis and Visualization/Project 2")

require(xlsx)
library(tm)
library(SnowballC)
library(wordcloud)


a <- read.xlsx("GFR_small.xlsx", sheetName = "Sheet1")

news <- read.delim("news.csv", stringsAsFactors=FALSE)

#original one
news_corpus <- VCorpus(VectorSource(news$News))

#writeCorpus(news_corpus,,filenames = "news_corpus.txt")

#clean the corpus
news_corpus1 <- tm_map(news_corpus,  stripWhitespace)
news_corpus1 <- tm_map(news_corpus1, content_transformer(tolower))
news_corpus1 <- tm_map(news_corpus1, removePunctuation)
news_corpus1 <- tm_map(news_corpus1, removeWords, stopwords("english"))

#stemmed corpus
news_corpus2 <- tm_map(news_corpus1, stemDocument)

#######################################################
#word cloud
n= 200

pal1 <- brewer.pal(11,"Spectral")
pal2 <- heat.colors(100, alpha = 1)
pal3 <- rainbow(n, s = 1, v = 1, start = 0.7, end = max(1, n - 1)/n, alpha = 1)

wordcloud(news_corpus1, max.words = n, random.order = FALSE, colors=pal1)
wordcloud(news_corpus2, max.words = n, random.order = FALSE, colors=pal1)

######################################################
