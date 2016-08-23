
suppressPackageStartupMessages(c(library(tm),library(shiny),library(stringr)))

source("global.R")

shinyServer(function(input, output) {
        
      predictWords <- reactive({
            if(input$inText!="") {
                  inText <- input$inText
                            
                  # get last 3 words of the last sentence
                  tranformed_inText <- sub(".*[\\.|\\?|!]", "",input$inText)
                  tranformed_inText <- unlist(strsplit(tranformed_inText, " "))
                  tranformed_inText <- tail(tranformed_inText, 3)
                
                  # clean input, remove whitespace/punctuation/lower-case 
                  tranformed_inText <- transformInput(tranformed_inText)
                
                  # predict
                  predictWords <- predictNextWords(length(tranformed_inText),tranformed_inText)
                  predictWords <- replaceContractions(predictWords)
            }
      })
      
      #output$predictWords <- renderPrint(predictWords())
      output$predictWords <- renderText(predictWords()) #don't print index [1]
      
             
})