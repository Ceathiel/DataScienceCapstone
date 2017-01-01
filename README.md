# Data Science Capstone

This repository contains all materials used and created for the Capstone Project for the Data Science Specialization in Coursera by Johns Hopkins University

### Background

The capstone project covers the field of Natural Language Processing, particularly next word predictions. In this course we were tasked to create an application that predicts the next word given an input text.

### Repository files and directories

The ff. files and directories can be found in the repository

1. prepareData.r - r script that takes a 15% sample of each of the blog, twitter and news text available in the corpus provided in the course and partitions it to a test and training set which are written into files. Non-ASCII characters are removed from the training set along with sentences with profane words.
    * Prerequisites before running:
        - Ensure that you've downloaded a copy of the corpus [here] (https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip) and unzipped it in the same directory as the script
        - swearWords.txt - file containing "bad" words should be in the same directory as the script
        
2. createModel.r - r script that performs additional clean up steps and creates 4-gram, Trigram, Bigram and Unigram models using `quanteda` and `dplyr` package
    * Prerequisites before running:
        - sampleData.txt - file containing sample data processed by `prepareData.r` should be available in the same directory
        
3. WordPrediction (directory) - contains all codes for the shiny application as well as the n-gram models generated by `createModel.r`

4. milestone1.html, milestone1.Rmd - files used for creating the milestone report published in [rpubs] (http://rpubs.com/ceathiel/nlpmilestone1)

5. capstonePresentation.Rpres, capstonePresentation.md, capstonePresentation-rpubs.html - files for the presentation covering the shiny application. The presentation is published [here] (http://rpubs.com/ceathiel/wordprediction)

6. screen1.jpg, screen2.jpg, mle.jpg - image files used in the presentation

7. predictNextWord.r - function created for predicting next word given an input phrase and a list of choices. This was primarily used to answer the quizzes for the course
    * Prerequisites before running:
        - n-gram model csv files in WordPrediction directory should be available



