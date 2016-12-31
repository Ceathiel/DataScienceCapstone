############################# CREATE MODEL SCRIPT ####################################
## Script used for creating the N-gram language models that are used for Word 
## Prediction application
## LAST MODIFIED @ 12/31/2016
##
## 1. Reads in sample text based on a sample size and partitions it into training
##    and test set
## 2. Cleans traning set. Removes non-ASCII characters, numbers, punctuation, symbols
## 3. Tokenizes sample text into sentences and filters out sentences with profanity
## 3. Generates document frequency matrix for unigram, bigram and trigram
##    based on training data
## 4. Computes maximum likelihood estimate for each unigram, bigram and trigram
## 5. Prunes bigram and trigam models to ngrams that occur more than once in text
## 6. Saves the trigram, bigram and unigram models into CSV files that will be
##    used by the word prediction app
## 
## Input Docs:
## Twitter text: final/en_US/en_US.twitter.txt
## Blog text: final/en_US/en_US.blogs.txt
## News text: final/en_US/en_US.news.txt
## Profanity filter: swearWords.txt
##
## Output Docs
## Training data: sampleData.txt
## Test data: testdata.txt
## Trigram Model: WordPrediction/TrigramProb.csv
## Bigram Model: WordPrediction/BigramProb.csv
## Unigram Model: WordPrediction/UnigramProb.csv
##
######################################################################################

library(quanteda)
library(dplyr)

# Set seed for reproducibility. 
set.seed(123)

# Set sample size to be used from whole corpus
samplesize <- .20

# Set percent of sample to be taken for testing
testsize <- .25

# Read twitter sample
twitter <- readLines("final/en_US/en_US.twitter.txt", skipNul = TRUE)
sample <- as.logical(rbinom (n=length(twitter),size=1, prob = samplesize))
sampleTweets <- twitter[sample]
#Set aside text for testing
test <- as.logical(rbinom (n=length(sampleTweets),size=1, prob = testsize))
testTweets <- sampleTweets[test]
#Set aside text for training model
modelTweets <- sampleTweets[!test]
rm(twitter)

# Read blogs sample
blogs <- readLines("final/en_US/en_US.blogs.txt")
sample <- as.logical(rbinom (n=length(blogs),size=1, prob = samplesize))
sampleBlogs <- blogs[sample]
#Set aside text for testing
test <- as.logical(rbinom (n=length(sampleBlogs),size=1, prob = testsize))
testBlogs <- sampleBlogs[test]
#Set aside text for training model
modelBlogs <- sampleBlogs[!test]
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
#Set aside text for training model
modelNews <- sampleNews[!test]
rm(news, conn)

# Join all model and test text to separate vectors and clean up objects
modelText <- c(modelTweets, modelBlogs, modelNews)
rm(modelTweets, modelBlogs, modelNews)
testText <- c(testTweets, testBlogs, testNews)

# Write test text to file for later use
write.table(testText, "testdata.txt", col.names = FALSE, row.names = FALSE, quote=FALSE)
rm(testTweets, testBlogs, testNews, testText)
rm(test, sample)
rm(sampleTweets, sampleBlogs, sampleNews)

# Remove non-ASCII characters
modelText <- iconv(modelText, "latin1", "ASCII", sub="")

# Tokenize to sentences
sampleSentences <- tokenize(modelText, what="sentence", simplify = TRUE)
rm(modelText)

# Read in words for profanity filter
conn <- file("swearWords.txt", open = "rb")
profanityFilter <- readLines(conn, skipNul = TRUE)
close(conn)

# Filter out sentences with profanity
profane <- rowSums(sapply(profanityFilter, function(x) grepl(sprintf('\\b%s\\b', x), sampleSentences)))
sampleSentences <- sampleSentences[profane==0]
rm(profane, profanityFilter)

# Write dataset to file for later use
write.table(sampleSentences, "sampleData.txt", col.names = FALSE, row.names = FALSE, quote=FALSE)


# Generate Unigram, Bigram and Trigram frequency using quanteda
# Clean up of numbers, punctation and symbols are also done here

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

# Clean up DFM for Bigram
rm(textBigram)

#Generate Trigrams and their frequency of occurence in the corpus
textTrigram <- dfm(sampleSentences,  toLower = TRUE, removeNumbers = TRUE, removePunct = TRUE, 
                   removeSeparators = TRUE, removeSymbols = TRUE, removeTwitter = TRUE, 
                   removeHyphens = TRUE, what="fasterword", ngrams=3)
TrigramFreq <- data.frame(freq=colSums(textTrigram))
rm(textTrigram)

# Divide Trigram into Bigram and Unigram
TrigramFreq$ngram <- rownames(TrigramFreq)
rownames(TrigramFreq) <- NULL
TrigramFreq$ngram <- gsub("_", " ", TrigramFreq$ngram)
TrigramFreq$Prev <- gsub("^((\\w+\\W+){1}\\w+).*$", "\\1", TrigramFreq$ngram)
TrigramFreq$Next <-  gsub("^.* (\\w+|<e>)$", "\\1", TrigramFreq$ngram)
format(object.size(TrigramFreq), units = "Mb")

## Extract Previous and Next words from Bigram
BigramFreq$ngram <- rownames(BigramFreq)
rownames(TrigramFreq) <- NULL
BigramFreq$ngram <- gsub("_", " ", BigramFreq$ngram)
BigramFreq$Prev <- gsub("^(\\w+|<s>) .*$", "\\1", BigramFreq$ngram)
BigramFreq$Next <-  gsub("^.* (\\w+|<e>)$", "\\1", BigramFreq$ngram)
format(object.size(BigramFreq), units = "Mb")

# Calculate Kneser-Ney Discount where N1 and N2 are Trigrams with count of 1 and 2
trigramTotal <- nrow(TrigramFreq)
n1 <- nrow(TrigramFreq[TrigramFreq$freq==1,])
n2 <- nrow(TrigramFreq[TrigramFreq$freq==2,])
D = n1 / (n1 + 2*n2)

# Generate Trigram probabilities using MLE with and without Kneser-Ney Discount
TrigramProb <- inner_join(TrigramFreq, BigramFreq, by=c("Prev"="ngram"))
TrigramProb <- TrigramProb[,1:5]
names(TrigramProb) <- c("TrigramFreq", "Trigram", "Bigram", "Next", "BigramFreq")
TrigramProb$MLEProb <- TrigramProb$TrigramFreq/TrigramProb$BigramFreq
TrigramProb$MLEProbDiscount <- (TrigramProb$TrigramFreq-D)/TrigramProb$BigramFreq
format(object.size(TrigramProb), units = "Mb")

# Calculate Kneser-Ney Discount where N1 and N2 are Bigrams grams with count of 1 and 2
bigramTotal <- nrow(BigramFreq)
n1Bigram <- nrow(BigramFreq[BigramFreq$freq==1,])
n2Bigram <- nrow(BigramFreq[BigramFreq$freq==2,])
DBigram = n1Bigram / (n1Bigram + 2*n2Bigram)

## Generate Bigram Probabilities using MLE with and without Kneser-Ney Discount
BigramProb <- inner_join(BigramFreq, WordFreq, by=c("Prev"="Word"))
names(BigramProb) <- c("BigramFreq", "Bigram", "Prev", "Next", "PrevFreq")
BigramProb$MLEProb <- BigramProb$BigramFreq/BigramProb$PrevFreq
BigramProb$MLEProbDiscount <- (BigramProb$BigramFreq-DBigram)/BigramProb$PrevFreq
format(object.size(BigramProb), units = "Mb")

# Clean Up
rm(BigramFreq, TrigramFreq)

#Generate Unigram probabilities using MLE
WordProb <- select(WordFreq, Word, freq) %>% mutate(MLEProb = freq/sum(WordFreq$freq))

# Calculate Kneser-Ney Continuation for Unigram

#Using Bigram Probabilities table, find the number of bigrams preceeding each word
PrevWordCount <- group_by(BigramProb, Next) %>% summarize(PrevCount=n()) %>% arrange(desc(PrevCount))
UnigramProb <- left_join(WordProb, PrevWordCount, by=c("Word"="Next"))
UnigramProb$KNProb <- UnigramProb$PrevCount/nrow(BigramProb)
names(UnigramProb) <- c( "Next", "freq", "MLEProb", "PrevCount", "KNProb")

#Clean Up
rm(WordProb, WordFreq)

# Prune Trigrams and Bigrams to those with more than 1 occurence in the corpus
BigramProb <- filter(BigramProb, BigramFreq>1)
TrigramProb <- filter(TrigramProb, TrigramFreq>1)

#Write computed Ngram probabilities into files
write.csv(UnigramProb, "WordPrediction/UnigramProb.csv", quote=FALSE)
write.csv(BigramProb, "WordPrediction/BigramProb.csv", quote=FALSE)
write.csv(TrigramProb, "WordPrediction/TrigramProb.csv", quote=FALSE)

