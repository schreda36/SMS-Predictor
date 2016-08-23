suppressPackageStartupMessages( c(library(tm),library(shiny),library(stringr),
                                  library(data.table),library(NLP)) )

quadData <- readRDS(file="./data/quadData.RData")
triData <- readRDS(file="./data/triData.RData")
biData <- readRDS(file="./data/biData.RData")
uniData <- readRDS(file="./data/uniData.RData")


# remove whitespace/punctuation/numbers and make lower case
transformInput <- function(Text){
      transInput <- stripWhitespace(Text)
      transInput <- removePunctuation(transInput)
      transInput <- removeNumbers(transInput)
      transInput <- tolower(transInput)
      #transInput <- gsub("[^\\p{L}\\s]+", "", transInput, perl=TRUE) #replace with nothing
      return(transInput)
}

replaceContractions <- function(str) {
      str <- gsub("^s\\s","\'s ",str)
      str <- gsub("^n\\s","\'n ",str)
      str <- gsub("^d\\s","\'d ",str)
      str <- gsub("^t\\s","\'t ",str)
      str <- gsub("^ve\\s","\'ve ",str)
      str <- gsub("^ll\\s","\'ll ",str)
      str <- gsub("^re\\s","\'re ",str)
      return(str)
      
      # rules <- c("i'm" = "i am", "it's" = "it is", "he's" = "he is",
      #            "that's" = "that is", "here's" = "here is", 
      #            "they're" = "they are", "i've" = "i have",
      #            "i'd" = "i would", "you're" = "you are",
      #            "i'll" = "i will",
      #            "life's" = "life is", "school's" = "school is",
      #            "there's" = "there is", "aren't" = "are not",
      #            "can't" = "can not", "couldn't" = "could not", 
      #            "didn't" = "did not", "doesn't" = "does not",
      #            "don't" = "do not", "haven't" = "have not",
      #            "isn't" = "is not", "wasn't" = "was not",
      #            "weren't" = "were not", "won't" = "will not",
      #            "wouldn't" = "would not", "let's" = "let us")
      # 
      # stringr::str_replace_all(x, rules)
}

#predict next word using ngram files
predictNextWords <- function(textLength,textInput){
      
      prediction <- data.frame()
      
      if (textLength>=3) {
            textInput <- textInput[(textLength-2):textLength]
            prediction <- as.character(quadData[quadData$word1==textInput[1] & 
                       quadData$word2==textInput[2] & 
                       quadData$word3==textInput[3],][1:5,]$word4)
      }

      else if(textLength==2) {
            textInput <- c(NA,textInput)
            prediction <- as.character(triData[triData$word1==textInput[2] & 
                       triData$word2==textInput[3],][1:5,]$word3)
            
      }
      else if(textLength==1) {
            textInput <- c(NA,NA,textInput)
            prediction <- as.character(biData[biData$word1==textInput[3],][1:5,]$word2)
      }
      # else {
      #        prediction <- as.character(uniData[1:5,]$word1) #must be start to sentence
      # }

      #if prediction can't be made grab top unigram words
      if(is.data.frame(prediction) && nrow(prediction)==0) {
                    prediction <- as.character(uniData[1:5,]$word1)

      }
      #probably NAs returned, so return top unigrams
      else if (is.na(prediction[1])) {
            #prediction <- c("failed safe", as.character(uniData[1:5,]$word1))
            prediction <- as.character(uniData[1:5,]$word1)
      }

      #prediction <- replaceContractions(gsub(lastW,"",dfsub$Word)
      print(prediction)
}

