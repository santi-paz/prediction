The project is aimed to analyze a sample of the english language and produce a predictive algorithm similar to those used in cell phones and apps like Swifkey. 

In 'data.R' the data was downloaded from https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip and is composed of 3 parts:

en_US.blogs.txt
en_US.news.txt
en_US.twitter.txt

The next step was to clean the data by removing profanity and special symbols. After a sample from each text file was obtained. Since the data file was so large, I experimented with different word samples that worked for the amount of RAM I have available. Once this was accomplised a tokenizer algorithm was created. The process to tokenizing separates collections of words into 1 word, 2 word or 3 word groups. Once the unigram bigram and trigram dataframes were created, a single data frame composed of all three n-grams was generated. This data base will be the data source for the prediction algorithm 'predict.R'

'predict.R' will take the data frame created in 'data.R' and generate a function where one can imput a word or words and receive a suggestion or prediction as to what the next word could be. 

The next step of the project will be to produce a shiny app with 'predict.R' available for the public. 
