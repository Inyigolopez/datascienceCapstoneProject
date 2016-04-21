

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Predicting text like SwiftKey - DSS Final Project - Natural Languaje Processing"),
  
  # Sidebar with input text
  sidebarLayout(
    
    sidebarPanel(
      
      h5("Capstone Project"),
      h5("by inyigolopez"),
      br(),
      h3("Put here a sentence and observe the prediction for the next word!!"),
      textInput(inputId = "userSentence", 
                label = "", 
                value = "" # 
      ),
      
      #checkboxInput("profFilter", label = "Profanity filter On (use at your own discretion)", value = TRUE),
      h4("The most probable word you put the next is:"),
      HTML("<span style='color:blue'>"),
      h3(textOutput("predictedWordMain"), align="center"),
      HTML("</span>"),
      br()
      #sliderInput("numPredicted", "Number of words to predict (max. 20):", min=1, max=20, value=5)
    ), #sidebarPanel
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Next Word Probability Table",
          #h4(textOutput("kText")),
          hr(),
          div(dataTableOutput("predictionTable"), style='font-size:150%')        
        ),
        tabPanel("App documentation", includeMarkdown("documentation.Rmd")
        )
      )
    ) #mainPanel ends
  )
))