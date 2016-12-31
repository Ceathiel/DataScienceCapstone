#
# This is the server logic of a Word Prediction Application
#

#Initializa libraries
library(shiny)
library(quanteda)
library(data.table)
library(dplyr)

#Read in ngram models
UnigramProb <- fread("UnigramProb.csv", header = T, sep = ",")
BigramProb <- fread("BigramProb.csv", header = T, sep = ",")
TrigramProb <- fread("TrigramProb.csv", header = T, sep = ",")

#Create top ngrams for wordcloud
top3 <- top_n(TrigramProb, 30, TrigramFreq)
top2 <- top_n(BigramProb, 30, BigramFreq)

#Create function for predicting next words using ngram model
predictNextWord <- function(sentence, choices=NULL) {
    
    #Clean up input sentence similar to how training set was cleaned up
    #Remove numbers, punctuation, symbols
    sentenceToken <- tokenize(tolower(sentence), removeNumbers = TRUE, removePunct = TRUE, 
                              removeSeparators = TRUE, removeSymbols = TRUE, removeTwitter = TRUE, 
                              removeHyphens = TRUE, what="fasterword", simplify = TRUE)
    
    #Initialize empty data frame to hold the next word predictions
    match <- data.frame(Next=character())
    
    #Check if entered text is valid and display a message
    if (length(sentenceToken) == 0) {
        return("App is ready. Please enter a phrase with valid characters from the alphabet in the textbox.")
    }
    
    #Attempt to match to a Trigram if sentence has 2 or more words using MLE Probability
    if (length(sentenceToken) >= 2) {
        lastBigram <- paste0(sentenceToken[length(sentenceToken)-1], " ", sentenceToken[length(sentenceToken)])
        match <- filter(TrigramProb, lastBigram==Bigram) %>% 
            arrange(desc(MLEProb))  %>% 
            top_n(5, MLEProb)
    }
    
    #If sentence has only 1 word or Trigram match has failed, attempt to match to a Bigram using MLE Probability
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

#Update the UI with the output of next word prediction
shinyServer(function(input, output) {
    output$predictedsentence <- renderText({ 
        text <- predictNextWord(input$words) 
        paste(text, collapse = "\n")
        })
    
    output$wordcloud <- renderPlot({
        par(mfrow=c(1,3))
        wordcloud(top3$Trigram, top3$TrigramFreq, scale=c(3,.3), colors=(brewer.pal(8, 'Dark2')))
        wordcloud(top2$Bigram, top2$BigramFreq, scale=c(3,.3), colors=(brewer.pal(8, 'Dark2')))   
    })
})
