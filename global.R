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
      
      else {
            prediction <- as.character(uniData[1:5,]$word1)
      }
      
      #prediction[is.na(prediction)] <- "" #replace NAs with blanks
      print(prediction)
}

