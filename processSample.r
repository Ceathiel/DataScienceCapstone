library(quanteda)
library(dplyr)

# Set seed for reproducibility. 
set.seed(123)

# Set sample size to be used from whole corpus
samplesize <- .25

# Set percent of sample to be taken for testing
devsize <- .25

# Set percent of sample to be taken for development
testsize <- .20

# Read twitter sample
twitter <- readLines("final/en_US/en_US.twitter.txt", skipNul = TRUE)
sample <- as.logical(rbinom (n=length(twitter),size=1, prob = samplesize))
sampleTweets <- twitter[sample]
#Set aside text for testing
test <- as.logical(rbinom (n=length(sampleTweets),size=1, prob = testsize))
testTweets <- sampleTweets[test]
#Set aside text for training and developing model
modelTweets <- sampleTweets[!test]
dev <- as.logical(rbinom (n=length(modelTweets),size=1, prob = devsize))
devTweets <- modelTweets[dev]
modelTweets <- modelTweets[!dev]
rm(twitter)

# Read blogs sample
blogs <- readLines("final/en_US/en_US.blogs.txt")
sample <- as.logical(rbinom (n=length(blogs),size=1, prob = samplesize))
sampleBlogs <- blogs[sample]
#Set aside text for testing
test <- as.logical(rbinom (n=length(sampleBlogs),size=1, prob = testsize))
testBlogs <- sampleBlogs[test]
#Set aside text for training and developing model
modelBlogs <- sampleBlogs[!test]
dev <- as.logical(rbinom (n=length(modelBlogs),size=1, prob = devsize))
devBlogs <- modelTweets[dev]
modelBlogs <- modelBlogs[!dev]
rm(blogs)

# Read news sample
conn <- file("final/en_US/en_US.news.txt", open = "rb")
news <- readLines(conn, skipNul = TRUE)
sample <- as.logical(rbinom (n=length(news),size=1, prob = samplesize))
close(conn)
sampleNews <- news[sample]
#Set aside text for testing
test <- as.logical(rbinom (n=length(sampleNews),size=1, prob = testsize))
testNews <- sampleNews[test]
#Set aside text for training and developing model
modelNews <- sampleNews[!test]
dev <- as.logical(rbinom (n=length(modelNews),size=1, prob = devsize))
devNews <- modelNews[dev]
modelNews <- modelNews[!dev]
rm(news, conn)

# Join all model, dev, and test text to separate vectors and clean up objects
modelText <- c(modelTweets, modelBlogs, modelNews)
rm(modelTweets, modelBlogs, modelNews)
devText <- c(devTweets, devBlogs, devNews)
rm(devTweets, devBlogs, devNews)
testText <- c(testTweets, testBlogs, testNews)
rm(testTweets, testBlogs, testNews)
rm(dev, test, sample)
rm(sampleTweets, sampleBlogs, sampleNews)

# Remove non-ASCII characters
modelText <- iconv(modelText, "latin1", "ASCII", sub="")
devText <- iconv(devText, "latin1", "ASCII", sub="") 
testText <- iconv(testText, "latin1", "ASCII", sub="") 

# Tokenize into sentences, clean the data (remove numbers, punctuation and separators)
# and add start and end of sentence markers
# sampleSentences <- tokenize(sampleText, what="sentence", removeNumbers = TRUE, removePunct = TRUE,
#                             removeSeparators = TRUE, simplify = TRUE)
# sampleSentences <- paste0("<s> ", sampleSentences, " <e>")
sampleSentences <- modelText
rm(modelText)

# Generate Unigrams and their frequency of occurence in the corpus
textDFM <- dfm(sampleSentences,  toLower = TRUE, removeNumbers = TRUE, removePunct = TRUE, 
               removeSeparators = TRUE, removeSymbols = TRUE, removeTwitter = TRUE, 
               removeHyphens = TRUE, what="fasterword")
WordFreq <- data.frame(freq=colSums(textDFM))
WordFreq$Word <- rownames(WordFreq)
rownames(WordFreq) <- NULL
rm(textDFM)

# Generate Bigrams and their frequency of occurence in the corpus
textBigram <- dfm(sampleSentences,  toLower = TRUE, removeNumbers = TRUE, removePunct = TRUE, 
                  removeSeparators = TRUE, removeSymbols = TRUE, removeTwitter = TRUE, 
                  removeHyphens = TRUE, what="fasterword", ngrams=2)
BigramFreq <- data.frame(freq=colSums(textBigram))

# Limit Bigrams to those with more than 1 occurence in the corpus
BigramFreq$ngram <- rownames(BigramFreq)
BigramFreq <- select(BigramFreq, ngram, freq) %>% filter(freq>1) %>% arrange(desc(freq))
rm(textBigram)

## Extract Previous and Next words from Bigram
BigramFreq$ngram <- gsub("_", " ", BigramFreq$ngram)
BigramFreq$Prev <- gsub("^(\\w+|<s>) .*$", "\\1", BigramFreq$ngram)
BigramFreq$Next <-  gsub("^.* (\\w+|<e>)$", "\\1", BigramFreq$ngram)
format(object.size(BigramFreq), units = "Mb")

#Generate Trigrams and their frequency of occurence in the corpus
textTrigram <- dfm(sampleSentences,  toLower = TRUE, removeNumbers = TRUE, removePunct = TRUE, 
                   removeSeparators = TRUE, removeSymbols = TRUE, removeTwitter = TRUE, 
                   removeHyphens = TRUE, what="fasterword", ngrams=3)
TrigramFreq <- data.frame(freq=colSums(textTrigram))
rm(textTrigram)

# Limit Trigrams to those with more than 1 occurence in the corpus
TrigramFreq$ngram <- rownames(TrigramFreq)
TrigramFreq <- select(TrigramFreq, ngram, freq) %>% filter(freq>1)

# Divide Trigram into Bigram and Unigram
TrigramFreq$ngram <- gsub("_", " ", TrigramFreq$ngram)
TrigramFreq$Prev <- gsub("^((\\w+\\W+){1}\\w+).*$", "\\1", TrigramFreq$ngram)
TrigramFreq$Next <-  gsub("^.* (\\w+|<e>)$", "\\1", TrigramFreq$ngram)
format(object.size(TrigramFreq), units = "Mb")

# Generate Trigram probabilities using MLE
TrigramProb <- inner_join(TrigramFreq, BigramFreq, by=c("Prev"="ngram"))
TrigramProb <- TrigramProb[,1:5]
names(TrigramProb) <- c("Trigram", "TrigramFreq", "Bigram", "Next", "BigramFreq")
TrigramProb$MLEProb <- TrigramProb$TrigramFreq/TrigramProb$BigramFreq
format(object.size(TrigramProb), units = "Mb")

## Generate Bigram Probabilities using MLE
BigramProb <- inner_join(BigramFreq, WordFreq, by=c("Prev"="Word"))
names(BigramProb) <- c("Bigram", "BigramFreq", "Prev", "Next", "PrevFreq")
BigramProb$MLEProb <- BigramProb$BigramFreq/BigramProb$PrevFreq
format(object.size(BigramProb), units = "Mb")
rm(BigramFreq, TrigramFreq)


#Generate Unigram probabilities (MLE and Kneser Ney) 
#Remove words occuring less than once in the corpus
WordProb <- select(WordFreq, Word, freq) %>% mutate(MLEProb = freq/sum(WordFreq$freq))
#Using Bigram Probabilities table, find the number of bigrams preceeding each word
PrevWordCount <- group_by(BigramProb, Next) %>% summarize(PrevCount=n()) %>% arrange(desc(PrevCount))
UnigramProb <- left_join(WordProb, PrevWordCount, by=c("Word"="Next"))
UnigramProb$KNProb <- UnigramProb$PrevCount/nrow(BigramProb)
names(UnigramProb) <- c( "Next", "freq", "MLEProb", "PrevCount", "KNProb")

#Clean Up
rm(WordProb, WordFreq)

#Write computed Ngram probabilities into files
write.csv(UnigramProb, "WordPrediction/UnigramProb.csv", quote=FALSE)
write.csv(BigramProb, "WordPrediction/BigramProb.csv", quote=FALSE)
write.csv(TrigramProb, "WordPrediction/TrigramProb.csv", quote=FALSE)

##Generate test data
choices <- list(c("eat", "give", "sleep", "die")
                , c("spiritual", "financial","marital", "horticultural")
                , c("decade", "weekend", "morning", "month")
                , c("happiness", "hunger", "sleepiness", "stress")
                , c("look", "picture", "minute", "work")
                , c("account", "incident", "case", "matter")
                , c("hand", "arm", "toe", "finger")
                , c("side", "top", "middle", "center")
                , c("inside", "outside", "weekly", "daily")
                , c("novels", "pictures", "stories", "movies"))

sentences <- c("When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd"
               ,"Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his"
               ,"I'd give anything to see arctic monkeys this"
               ,"Talking to your mom has the same effect as a hug and helps reduce your"
               ,"When you were in Holland you were like 1 inch away from me but you hadn't time to take a"
               ,"I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the"
               ,"I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each"
               ,"Every inch of you is perfect from the bottom to the"
               ,"I’m thankful my childhood was filled with imagination and bruises from playing"
               ,"I like how the same people are in almost all of Adam Sandler's")
