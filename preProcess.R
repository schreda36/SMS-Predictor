# This file is intended to be run initially to create an RDS file which
# includes ngrams based on the 3 files provided for this project. The Phrase 
# Predictor Shiny app will read the RDS file once for all sessions. This file may 
# be re-run in order to re-create the RDS file if the model or files are updated.


#library("R.utils")
library("RWeka")
library("tm")
library("SnowballC") 


setwd("../CAPSTONE")
dir <- "data"
enFiles <- list.files(path=dir, recursive=T, pattern=".*en_.*.txt")


############# Grab data if necessary
url  <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
fname <- "Coursera-SwiftKey.zip"
fpath <- paste(dir, fname, sep="/")
if (!file.exists(fpath)){
      download.file(url, destfile=fpath, method="curl")
}
unzip(zipfile=fpath, exdir=dir)

#Reading in chunks or lines using R's readLines or scan function
#Only needs to run once, unless source files change
set.seed(768)
blogFile <- file("./data/final/en_US/en_US.blogs.txt", open="r")
blog_lines <- readLines(blogFile, skipNul=TRUE)
num_blog_lines <- length(blog_lines)
blog_sample <- blog_lines[sample(1:num_blog_lines, num_blog_lines * 0.07, replace=FALSE)]
con2 <- file("data/sample/sampleCorpus.txt", "w+")
writeLines(blog_sample, con2)
close(con2)
close(blogFile)
remove(blog_lines,blog_sample)

newsFile <- file("./data/final/en_US/en_US.news.txt", open="r")
news_lines <- readLines(newsFile, skipNul = TRUE)
num_news_lines <- length(news_lines)
news_sample <- news_lines[sample(1:num_news_lines, num_news_lines * 0.07, replace=FALSE)]
con2 <- file("data/sample/sampleCorpus.txt", "w+")
writeLines(news_sample, con2)
close(newsFile)
close(con2)
remove(news_lines,news_sample)

twitterFile <- file("./data/final/en_US/en_US.twitter.txt", open="r")
twit_lines <- readLines(twitterFile, skipNul=TRUE)
num_twit_lines <- length(twit_lines)
twit_sample <- twit_lines[sample(1:num_twit_lines, num_twit_lines * 0.07, replace=FALSE)]
con2 <- file("data/sample/sampleCorpus.txt", "w+")
writeLines(twit_sample, con2)
close(con2)
close(twitterFile)
remove(twit_lines,twit_sample)


############# Data Pre-processing of data/sample/sample_corpus.txt
#docCon <- file("./data/sample/combinedSample.txt")
docCon <- file("./data/sample/sampleCorpus.txt")
docs <- readLines(docCon)
docs <- Corpus(VectorSource(docs)) #??????????
docs <- tm_map(docs, removePunctuation)  
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, tolower)
#docs <- tm_map(docs, removeWords, stopwords("english")) #removes common words (this, is, etc)
docs <- tm_map(docs, stemDocument)  #removes word endings (ing, es, s, etc)
docs <- tm_map(docs, stripWhitespace) 
profanity_vector <- readLines("data/profanity_filter.txt")
docs <- tm_map(docs, removeWords, profanity_vector) 
#docs <- tm_map(docs, PlainTextDocument) #??????????

## save sample corpus
saveRDS(docs, file = "./data/sample/cleanSample.RData")
remove(docs, profanity_vector, docCon)
## if Term Doc Matrix is needed
#termDocMatSample <- TermDocumentMatrix(docs)
#saveRDS(termDocMatSample, file = "./data/termDocMatSample.RData")

docs <- readRDS(file="./data/sample/cleanSample.RData")

# create uni/bi/tri/quad-gram tokenizers
Unigram <- NGramTokenizer(docs, Weka_control(min = 1, max = 1,delimiters = " \\r\\n\\t.,;:\"()?!"))
Unigrams <- data.frame(table(Unigram))
Unigrams <- Unigrams[order(Unigrams$Freq,decreasing = TRUE),]
#only used for plotting Unigrams <- Unigrams[1:30,]
colnames(Unigrams) <- c("String","Freq")
saveRDS(Unigrams, file = "./data/unigram.RData")
remove(Unigram,Unigrams)

Bigram <- NGramTokenizer(docs, Weka_control(min = 2, max = 2,delimiters = " \\r\\n\\t.,;:\"()?!"))
Bigrams <- data.frame(table(Bigram))
Bigrams <- Bigrams[order(Bigrams$Freq,decreasing = TRUE),]
#only used for plotting Bigrams <- Bigrams[1:30,]
colnames(Bigrams) <- c("String","Freq")
saveRDS(Bigrams, file = "./data/bigram.RData")
remove(Bigram,Bigrams)

Trigram <- NGramTokenizer(docs, Weka_control(min = 3, max = 3,delimiters = " \\r\\n\\t.,;:\"()?!"))
Trigrams <- data.frame(table(Trigram))
Trigrams <- Trigrams[order(Trigrams$Freq,decreasing = TRUE),]
#only used for plotting Trigrams <- Trigrams[1:30,]
colnames(Trigrams) <- c("String","Freq")
saveRDS(Trigrams, file = "./data/trigram.RData")
remove(Trigram,Trigrams)

Quadgram <- NGramTokenizer(docs, Weka_control(min = 4, max = 4,delimiters = " \\r\\n\\t.,;:\"()?!"))
Quadgrams <- data.frame(table(Quadgram))
Quadgrams <- Quadgrams[order(Quadgrams$Freq,decreasing = TRUE),]
#only used for plotting Quadgrams <- Quadgrams[1:30,]
colnames(Quadgrams) <- c("String","Freq")
saveRDS(Quadgrams, file = "./data/quadgram.RData")
remove(Quadgram,Quadgrams)

#uniData <- readRDS(file = "./data/unigram.RData")

# split ngrams into distinct columns and save
uniData <- readRDS(file = "./data/unigram.RData")
uniData <- uniData[1:10,]
uniData2 <- read.table( text=paste(gsub("(\\S+)","\\1,\\2", uniData$String),uniData$Freq), sep="," )
colnames(uniData2) <- c("word1","Freq")
saveRDS(uniData2, file = "./data/uniData.RData")
remove(uniData, uniData2)

biData <- readRDS(file = "./data/bigram.RData")
biData <- biData[1:300000,]
biData2 <- read.table( text=paste(gsub("(\\S+)\\s+(\\S+)","\\1,\\2,\\3", biData$String),biData$Freq), sep="," )
colnames(biData2) <- c("word1","word2","Freq")
saveRDS(biData2, file = "./data/biData.RData")
remove(biData, biData2)

triData <- readRDS(file = "./data/trigram.RData")
triData <- triData[1:300000,]
triData2 <- read.table( text=paste(gsub("(\\S+)\\s+(\\S+)\\s+(\\S+)","\\1,\\2,\\3,\\4", triData$String),triData$Freq), sep="," )
colnames(triData2) <- c("word1","word2","word3","Freq")
saveRDS(triData2, file = "./data/triData.RData")
remove(triData, triData2)

quadData <- readRDS(file = "./data/quadgram.RData")
quadData <- quadData[1:300000,]
quadData2 <- read.table( text=paste(gsub("(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)","\\1,\\2,\\3,\\4,\\5", quadData$String),quadData$Freq), sep="," )
colnames(quadData2) <- c("word1","word2","word3","word4","Freq")
saveRDS(quadData2, file = "./data/quadData.RData")
remove(quadData, quadData2)

#remove duplicates