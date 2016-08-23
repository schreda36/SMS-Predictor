suppressPackageStartupMessages(library(shiny))

shinyUI(fluidPage(
      titlePanel("Phrase Predictor"),
      fluidRow(
            column(12, align="center",
                   mainPanel(
                         br() 
                         ,textInput(inputId="inText", label = "Enter text:")
                         ,br()
                         ,h6("Top 5 potential next single words:")
                         ,div(textOutput("predictWords"), style = "color:red")
                   )
            )
      )
))
