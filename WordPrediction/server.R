#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(quanteda)
library(data.table)
library(dplyr)

#Read in ngram models
UnigramProb <- fread("UnigramProb.csv", header = T, sep = ",")
BigramProb <- fread("BigramProb.csv", header = T, sep = ",")
TrigramProb <- fread("TrigramProb.csv", header = T, sep = ",")


#Create function for predicting next words
predictNextWord <- function(sentence, choices=NULL) {
    
    #Clean up input sentence similar to how training set was cleaned up
    sentenceToken <- tokenize(tolower(sentence), removeNumbers = TRUE, removePunct = TRUE, 
                              removeSeparators = TRUE, removeSymbols = TRUE, removeTwitter = TRUE, 
                              removeHyphens = TRUE, what="fasterword", simplify = TRUE)
    
    #Initialize empty data frame
    match <- data.frame(Next=character())
    
    #Check if entered text is valid
    if (length(sentenceToken) == 0) {
        return("Please enter a phrase with valid characters from the alphabet in the textbox.")
    }
    
    #Attempt to match to a Trigram if sentence has 2 or more words
    if (length(sentenceToken) >= 2) {
        lastBigram <- paste0(sentenceToken[length(sentenceToken)-1], " ", sentenceToken[length(sentenceToken)])
        match <- filter(TrigramProb, lastBigram==Bigram) %>% 
            arrange(desc(MLEProb))  %>% 
            top_n(5, MLEProb)
    }
    
    #If sentence has only 1 word or Trigram match has failed, attempt to match to a Bigram using Kneser-Ney Probability
    if (length(sentenceToken) == 1 | nrow(match) == 0){
        lastWord <- sentenceToken[length(sentenceToken)]
        match <- filter(BigramProb, lastWord==Prev) %>% 
            arrange(desc(MLEProb)) %>% 
            top_n(5, MLEProb) 
    } 
    
    #If Bigram match has failed, attempt to match to a Unigram using Kneser-Ney Probability
    if (nrow(match) == 0){
        match <- arrange(UnigramProb, desc(KNProb)) %>% top_n(5, KNProb) 
    } 
    
    return(paste0(sentence, " ", match$Next))
    
}

#Shiny Server functions
shinyServer(function(input, output) {
    output$predictedsentence <- renderText({ 
        text <- predictNextWord(input$words) 
        paste(text, collapse = "\n")
        })
})
