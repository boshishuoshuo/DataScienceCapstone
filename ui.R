library(shiny)

shinyUI(fluidPage(
        
        titlePanel("Capstone project - Predict the next word with N-gram"),
        
        sidebarLayout(
                sidebarPanel(
                        h4("Enter some texts,and select the number of choices 
                           for the next word,"),
                        br(),
                        textInput("text1", "Enter some texts:", value = ""),
                        br(),
                        sliderInput("choice", "Number of choices:", min = 1,
                                    max = 10, value = 1),
                        hr(),
                        submitButton("predict")
                        ),
                
                mainPanel(h3("The next predicted word is: "),
                          verbatimTextOutput("predicted_word"),
                          h3("The used N-gram is: "),
                          textOutput("Ngram"),
                          hr(),
                          h3("The word cloud for the next word based on probablity."),
                          plotOutput("cloud")
                )
        )
))
