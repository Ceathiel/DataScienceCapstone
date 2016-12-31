
This application was developed for the Capstone Project for the Data Science Specialization in Coursera by Johns Hopkins University

#### How the word prediction is done
    
The prediction is done using an N-gram language model which is trained from samples of twitter, blog and news text provided as part of the course. Trigram, Bigram and Unigram models were used for this applicaiton.

When an input phrase is entered in the text field, the application attempts to find a suitable match using a Stupid Backoff algorithm where first it attempts to find suitable match from the Trigram Model with the highest maximum likelihood estimate (MLE). If no trigram match is found, it backs off to find a match in the Bigram Model, again using MLE.

Finally, if no Trigram or Bigram match is found, it looks at Unigrams and provides a recommendation based on the computed Kneser-Ney probability for the unigram.

In the interest of speed, the N-gram models have been pre-processed and resulting computations are saved into CSV files which the application reads in during initialization.

#### Additional documentation

Presentation slides fo this application may be found in:

Application code for this is available in github: 

1. Shiny Application code: https://github.com/Ceathiel/DataScienceCapstone/tree/master/WordPrediction
2. N-gram modelling codes: https://github.com/Ceathiel/DataScienceCapstone
