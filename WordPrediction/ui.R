#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Word Prediction Application"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
        h4("How to use:"),
        p("Type a sentence on the textbox on the right. Choices completing the word should update while you type")
    ),
    
    #Text input for word with selectize to show predicted sentence
    mainPanel(
        textInput('words', label="Write some words", placeholder="hint"),
        verbatimTextOutput('predictedsentence')
    )
  )
))
