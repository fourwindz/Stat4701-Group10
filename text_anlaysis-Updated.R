setwd("D:/Google Drive/Courses_G/1B_Exploratory Data Analysis and Visualization/Project 2")

library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

#news <- read.delim("news.csv", stringsAsFactors=FALSE)
#news_complete <- read.csv("news_complete.csv", stringsAsFactors=FALSE)
#news_complete <- read.xlsx("news_complete.csv",sheetName = "Sheet1")

news_complete <- read.csv("GlobalFloodsRecord_updated.csv", na.strings = "NA", sep="~")

news_complete$Country <- factor(news_complete$Country)

#change column names 
names(news_complete)[1] <- 'ID'
names(news_complete)[30] <- 'News'


#clean the corpus
makeCorpus <- function (dataset) {
  news_corpus <- VCorpus(VectorSource(dataset$News))
  news_corpus <- tm_map(news_corpus,  stripWhitespace)
  news_corpus <- tm_map(news_corpus, content_transformer(tolower))
  news_corpus <- tm_map(news_corpus, removePunctuation)
  result <- tm_map(news_corpus, removeWords, stopwords("english"))
  return(result)
}

#stem the corpus
makestemmedCorpus <- function (corpus) {
  news_corpus2 <- tm_map(corpus, stemDocument)
  return(news_corpus2)
}

corpus_complete <- makeCorpus(news_complete)

stemmedcorpus_complete <- makestemmedCorpus(corpus_complete)


######################################################
#subgroup corpus
subgroup_usa = news_complete[news_complete$Country=='USA',]
corpus_usa = makeCorpus(subgroup_usa)
stemmedcorpus_usa = makestemmedCorpus(corpus_usa)

subgroup_china = news_complete[news_complete$Country=='China',]
corpus_china = makeCorpus(subgroup_china)
stemmedcorpus_china = makestemmedCorpus(corpus_china)

subgroup_india = news_complete[news_complete$Country=='India',]
corpus_india = makeCorpus(subgroup_india)
stemmedcorpus_india = makestemmedCorpus(corpus_india)


#######################################################
#word cloud
n= 100

#colors
pal1 <- brewer.pal(9,"YlGn")
pal2 <- sort(heat.colors(10, alpha = 1),decreasing = T)
pal3 <- rainbow(n, s = 1, v = 1, start = 0.7, end = max(1, n - 1)/n, alpha = 1)
pal4 <- brewer.pal(9,"OrRd")
pal5 <- brewer.pal(9,"Blues")

par(mfrow=c(1,1))
wordcloud(corpus_complete, max.words = n, random.order = FALSE, colors=pal1)
wordcloud(stemmedcorpus_complete, max.words = n, random.order = FALSE, rot.per = 0.2,colors=pal4)

######################################################
#1.Comparing three countries
par(mfrow=c(1,3))
corpus_usa_2 <- tm_map(corpus_usa,  removeWords, c("flooding","flood","floods",'said'))
corpus_china_2 <- tm_map(corpus_china,  removeWords, c("flooding","flood","floods",'said'))
corpus_india_2 <- tm_map(corpus_india,  removeWords, c("flooding","flood","floods",'said'))

wordcloud(corpus_usa_2, max.words = n, random.order = FALSE, colors=pal5)
title("USA",line = -10)
wordcloud(corpus_china_2, max.words = n, random.order = FALSE, colors=pal4)
title("China",line = -10)
wordcloud(corpus_india_2, max.words = n, random.order = FALSE, colors=pal1)
title("India",line = -10)


wordcloud(stemmedcorpus_usa, max.words = 50, random.order = FALSE, colors=pal5)
wordcloud(stemmedcorpus_china, max.words = 50, random.order = FALSE, colors=pal4)
wordcloud(stemmedcorpus_india, max.words = n-50, random.order = FALSE, colors=pal4)

#######################################################
#2.Comparing severity
# This section compares the words for flood with varying magnitudes

subgroup_severe = news_complete[news_complete$Magnitude..M...>=6.1,]
corpus_severe = makeCorpus(subgroup_severe)
stemmedcorpus_severe = makestemmedCorpus(corpus_severe)

corpus_severe_2 <- tm_map(corpus_severe,  removeWords, c("flooding","flood","flooded","floods",'said'))

subgroup_mild = news_complete[news_complete$Magnitude..M...<4.6,]
corpus_mild = makeCorpus(subgroup_mild)
stemmedcorpus_mild = makestemmedCorpus(corpus_mild)

corpus_mild_2 <- tm_map(corpus_mild,  removeWords, c("flooding","flood","floods",'said'))


par(mfrow=c(1,2))
wordcloud(corpus_severe_2, max.words = n, random.order = FALSE, rot.per = 0.3, colors=pal4)
title("Magnitude > 6",line=-3)
wordcloud(corpus_mild_2, max.words = n, random.order = FALSE, rot.per = 0.3, colors=pal5)
title("Magnitude < 4.6",line=-3)

#par(mfrow=c(1,2))
#wordcloud(stemmedcorpus_severe, max.words = n, random.order = FALSE,  colors=pal4)
#wordcloud(stemmedcorpus_mild, max.words = n, random.order = FALSE, colors=pal5)


subgroup_severe = subgroup_usa[as.numeric(subgroup_usa$Dead)>164]
stemmedcorpus_severe = makestemmedCorpus(corpus_severe)

corpus_severe_2 <- tm_map(corpus_severe,  removeWords, c("flooding","flood","floods","flooded",'said'))

wordcloud(corpus_severe_2, max.words = n, random.order = FALSE, rot.per = 0.3, colors=pal4)

subgroup_mild = subgroup_usa[as.numeric(subgroup_usa$Dead)<=2,]
corpus_mild = makeCorpus(subgroup_mild)
stemmedcorpus_mild = makestemmedCorpus(corpus_mild)

corpus_mild_2 <- tm_map(corpus_mild,  removeWords, c("flooding","flood","floods","flooded",'said'))
wordcloud(corpus_mild_2, max.words = n, random.order = FALSE, rot.per = 0.3, colors=pal5)

#######################################################
#3. Words Association
dtm_usa <- DocumentTermMatrix(corpus_usa,control = list(minWordLength = 10))
dtm_china <- DocumentTermMatrix(corpus_china,control = list(minWordLength = 10))
dtm_india <- DocumentTermMatrix(corpus_india,control = list(minWordLength = 10))

findAssocs(dtm_usa, "killed",0.5)

findAssocs(dtm_china, "killed",0.5)

findAssocs(dtm_india, "killed",0.5)


