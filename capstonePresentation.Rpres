Word Prediction Application
========================================================
author: Rhea Lucas
date: Dec 29, 2016
autosize: true
font-family: arial

This presentation will provide a quick background on the application, it's usage, logic and additional references.

About the App
========================================================

The Word Prediction application provides next word suggestions given an input phrase. 

It was developed for the Capstone Project for the Data Science Specialization by Johns Hopkins University in Coursera and is accessible through the ff. link:

<https://ceathiel.shinyapps.io/WordPrediction/>

The r codes for the application are available in the ff. repository:

<https://github.com/Ceathiel/DataScienceCapstone/tree/master/WordPredictionr>

***

![Caption](screen1.jpg) 

Getting Started with Word Predictions
========================================================

The application has an input text field on the "Word Predictions"" tab. After the application loads, wait until you see the "App is ready" message (about 10-20 seconds) then begin typing the phrase you want to get a prediction for. 

Your input phrase along with suggested next words will be shown below the textbox with about 1-2 seconds delay as you type.

***

![Caption](screen2.jpg) 

<small>
Additional Notes:
* The application cannot provide predictions if you only input numbers or symbols. For example, "24 hours" is a valid input but "24" is not.
* Visitors may also refer to "Application Details" tab to get more information about the application
</small>

How It Works
========================================================

### Preparing the data

The Word Prediction app uses an [N-gram language model] (https://en.wikipedia.org/wiki/Language_model#n-gram_models) created from samples of twitter, blog and news text provided as part of the course.

Trigram, Bigram and Unigram language models were created using the [quanteda] (https://cran.r-project.org/web/packages/quanteda/vignettes/quickstart.html) package which provides functionalities for cleaning up text, generating n-grams from it and counting frequencies for each ngram.

The ff. clean-up steps were done:

1. Changing text to lowercase
2. Removing non-ASCII characters
3. Removing punctuation and symbols (i.e. #, etc)
4. Removing numbers
5. Removing sentences with bad words based on list downloaded [here] (http://www.bannedwordlist.com/)

How It Works
========================================================

### Creating the Model

Once data has been cleaned up, we extract the count of occurence for each unique, Trigram, Bigram and Unigram found in the text and use this for computing the maximum likelihood estimate (MLE) for each n-gram.

For the application, a 15% sample of the entire text provided was used for training the model. Overall, it takes about 1 hr and 15 mins to generate the entire model from reading in data up to computing the MLE for each order n-gram.


How It Works
========================================================

### Generating Next Word Suggestions

To generate the next word suggestions, the application uses an algorithm called [Stupid Back-Off] (http://www.aclweb.org/anthology/D07-1090.pdf) which is efficient for calculating predictions over huge datasets.

When an input phrase is entered in the text field, the application attempts to find a suitable match using a Stupid Backoff algorithm where first it attempts to find suitable match from the Trigram Model with the highest maximum likelihood estimate (MLE). If no trigram match is found, it backs off to find a match in the Bigram Model, again using MLE.

Finally, if no Trigram or Bigram match is found, it looks at Unigrams and provides a recommendation based on the computed Kneser-Ney probability for the unigram.



