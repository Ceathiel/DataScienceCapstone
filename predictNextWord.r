## Function that predicts next word based on stupid backoff model but using 
## kneser-ney probability when it reaches unigram
##

library(dplyr)
library (quanteda)

predictNextWord <- function(sentence) {

sentenceToken <- tokenize(tolower(sentence), removeNumbers = TRUE, removePunct = TRUE, 
                          removeSeparators = TRUE, removeSymbols = TRUE, removeTwitter = TRUE, 
                          removeHyphens = TRUE, what="fasterword", simplify = TRUE)

if (length(sentenceToken) == 0) {
    return("No input")
}

#Initialize empty data frame
match <- data.frame(Next=character())

#Attempt to match to a Trigram if sentence has 2 or more words
if (length(sentenceToken) >= 2) {
    lastBigram <- paste0(sentenceToken[length(sentenceToken)-1], " ", sentenceToken[length(sentenceToken)])
    match <- filter(TrigramProb, lastBigram==Bigram) %>% arrange(desc(MLEProb))
    match <- head(match,1)
}

#If sentence has only 1 word or Trigram match has failed, attempt to match to a Bigram using Kneser-Ney Probability
if (length(sentenceToken) == 1 | nrow(match) == 0){
    lastWord <- sentenceToken[length(sentenceToken)]
    match <- filter(BigramProb, lastWord==Prev) %>% arrange(desc(MLEProb*0.4))
    match <- head(match,1)
} 

#If Bigram match has failed, attempt to match to a Unigram using Kneser-Ney Probability
if (nrow(match) == 0){
    match <- arrange(UnigramProb, desc(KNProb*0.4)) 
    match <- head(match,1)
} 

return(match$Next)

}
