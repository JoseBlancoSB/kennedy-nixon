#  
# This script will document the creation of a wordcloud, a comparison wordcloud, and
# a sentiment analysis of the inaugural addresses of President John F. Kennedy and
# the first innaugural address of President Richerd M. Nixon.
#
# For Kennedy, the web source is the following
#
# https://www.jfklibrary.org/learn/about-jfk/historic-speeches/inaugural-address
#
# Kennedy delivered the speech on January 20, 1961
#
# I copied and paste the text of the address onto a .txt file
#
# /home/jose/R/wordcloud/kennedy-nixon/kennedy-inaugural.txt
#
# For Nixon, the web source is the following
#
# https://avalon.law.yale.edu/20th_century/nixon1.asp 
#
# Nixon deivered the speech on January 20, 1969
#
# I copied and paste the text of the address onto a .txt file
#
# /home/jose/R/wordcloud/kennedy-nixon/nixon-inaugural.txt
#
#
# ****************************************************************************
#
# Let's set the working directory
#

setwd("/home/jose/R/wordcloud/kennedy-nixon")
getwd()  # check

################################################################################
# 1) Copy the files from the web and store them in the right directory.
#    Import the files into RStudio
################################################################################

# Import the text file.  You have to do it interactively.
# First, Kennedy

kennedy <- readLines(file.choose("/home/jose/R/wordcloud/kennedy-nixon"))

# Then select the file from the directory to load it into RStudio
# Then, Nixon

nixon <- readLines(file.choose("/home/jose/R/wordcloud/kennedy-nixon"))

#
# Now we should install or load certain packages that are going to be necessary
# to proccess these documents
#
# If the packaages have not been installed, they must be installed.

library(RColorBrewer)
library(SnowballC)
library(wordcloud)
library(NLP)
library(tm)
library(stringr)
library(dplyr)
library(ggplot2)

# Check the structure of the read files
str(kennedy)  # kennedy -- should be chr
str(nixon)  # nixon -- should be chr

# 
# Use the paste command to make the files into one line
#
# First Kennedy and then Nixon

dfpaste_kennedy <- paste(readLines("kennedy-inaugural.txt"), collapse = " ")
dfpaste_nixon <- paste(readLines("nixon-inaugural.txt"), collapse = " ")

# Check

dfpaste_kennedy
dfpaste_nixon

# Remove punctuation, using gsub() -- part of grep()

dfpaste_kennedy <-  gsub(pattern = "\\W", replace = " ", dfpaste_kennedy)
dfpaste_nixon <-  gsub(pattern = "\\W", replace = " ", dfpaste_nixon)

# The "pattern = '\\W'" argument looks for all instances that are not a word.
# for a complete list of the "pattern" values for gsub(), see
# http://www.endmemo.com/r/gsub.php

# Remove numbers, again with gsub()
# the \d escape sequence finds the "d"igits 0 - 9.

dfpaste_kennedy <-  gsub(pattern = "\\d", replacement = " ", dfpaste_kennedy)
dfpaste_nixon <-  gsub(pattern = "\\d", replacement = " ", dfpaste_nixon)

# Remove upper case letters by using tolower() function

dfpaste_kennedy <- tolower(dfpaste_kennedy)
dfpaste_nixon <- tolower(dfpaste_nixon)

# Remove "non-interesting" words, such as "and", "but", "that"
#  these words are called "stopwords" in the tm package.

stopwords()  # Gives a complete list of the stopwords

dfpaste_kennedy <- removeWords(dfpaste_kennedy, stopwords())
dfpaste_nixon <- removeWords(dfpaste_nixon, stopwords())

# Remove single leftover letters like "s"

dfpaste_kennedy <- gsub(pattern = "\\b[A-z]\\b{1}", replacement = " ", dfpaste_kennedy)
dfpaste_nixon <- gsub(pattern = "\\b[A-z]\\b{1}", replacement = " ", dfpaste_nixon)

# Remove additional words, based on meta data and inspection

dfpaste_kennedy <- removeWords(dfpaste_kennedy, c("english", "mr"))
dfpaste_nixon <- removeWords(dfpaste_nixon, c("first", "inaugural", "address", "nixon", "milhouse"))

# Clean up the white space.

dfpaste_kennedy <- stripWhitespace(dfpaste_kennedy)
dfpaste_nixon <- stripWhitespace(dfpaste_nixon)

######################################################################
#   Sentiment Analysis and Wordcloud
######################################################################

library(stringr)

###########  Perform a sentiment analysis
dfpaste_kennedy <- str_split(dfpaste_kennedy, pattern = "\\s+")
dfpaste_nixon <- str_split(dfpaste_nixon, pattern = "\\s+")

# unlist the df, which was converted to a list.

dfpaste_kennedy <- unlist(dfpaste_kennedy)
dfpaste_nixon <- unlist(dfpaste_nixon)
str(dfpaste_kennedy)
str(dfpaste_nixon)

# Put the list of positive words and negative words into the working directory
# Call the positives, "positive" and the negatives "negative".

# load  the positive and the negative words

positive <- readLines("pos.txt")
negative <- readLines("neg.txt")

# Get a sentiment score for Kennedy

kennedy_positive_words <- sum(!is.na(match(positive, dfpaste_kennedy)))
kennedy_negative_words <- sum(!is.na(match(negative, dfpaste_kennedy)))

kenedy_sentiment_score <- kennedy_positive_words - kennedy_negative_words

# Get a sentiment score for Nixon

nixon_positive_words <- sum(!is.na(match(positive, dfpaste_nixon)))
nixon_negative_words <- sum(!is.na(match(negative, dfpaste_nixon)))

nixon_sentiment_score <- nixon_positive_words - nixon_negative_words

################  Do the word clouds ##########

wordcloud(dfpaste_kennedy)
wordcloud(dfpaste_nixon)

### Kennedy

wordcloud(words = dfpaste_kennedy, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

### Nixon

wordcloud(words = dfpaste_nixon, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))




######################################################################
#   Jalayer Academy, Text Mining 4, YouTube -- Postive and Negative Terms for Sentiment Analysis
#   https://www.youtube.com/watch?v=WfoVINuxIJA
######################################################################

scan('positive.txt', what = 'character', comment.char = ';')

positive_words <- scan('positive.txt', what = 'character', comment.char = ';')

negative_words <- scan('negative.txt', what = 'character', comment.char = ';')

######################################################################
#   Combine both speeches into one corpus
######################################################################

######################################################################
#
# Create a variaable called "folder" that has the filepath to the directory where
# the documents I want reside.
#

folder <- "/home/jose/R/wordcloud/kennedy-nixon/"

list.files(path=folder)

#
# Now we create a variable called "filelist" that has a list of the files we
# want, based on the pattern we specify.
#

list.files(path = folder, pattern = "*inaugural.txt")

filelist <- list.files(path = folder, pattern = "*inaugural.txt")

# Now we have to combine the filepath in "folder" with the names of the files
# in "filelist" to feed to R the filenames in the format that it can use to import
# the files.  Let's call it "filelist" and replace the filelist we have with the list
# that has the complete path name.

filelist <- paste(folder, "/", filelist, sep = "")
filelist

# Now we use lapply accross both files in "filelist" using the function "readLines"'
# which actually imports files into R.

lapply(filelist, FUN = readLines)

# Store the output in a variable, call it "a" so that we can work with it.

a <- lapply(filelist, FUN = readLines)

# Now w combine all the lines in both documents into just one line for one, and
# another line for 2.

lapply(a, FUN = paste, collapse = " ")

# Call the out put a variable, let's say "corpus".

corpus <- lapply(a, FUN = paste, collapse = " ")


######################################################################
#   Jalayer Academy, Text Mining 6, YouTube -- Cleaning Corpus text in R
#   https://www.youtube.com/watch?v=jCrQYOsAcv4
######################################################################

# First, let's get rid of punctuation using gsub.
# The command below gets rid of all (or most! ) punctuation.
#

gsub(pattern = "\\W", replacement = " ", corpus)

# Let's store the output in a variable. say, "corpus2"

corpus2 <- gsub(pattern = "\\W", replacement = " ", corpus)

# Now we get rid of numbers (digits).  We can replace the output in "corpus2"

corpus2 <- gsub(pattern = "\\d", replacement = " ", corpus2)

# Now we make UPPER CASE words, lower case.

corpus2 <- tolower(corpus2)

# Now let's remove the stopwords.

corpus2 <- removeWords(corpus2, stopwords("english"))

# Now let's remove single letter words.
# Remove single leftover letters like "s"

corpus2 <- gsub(pattern = "\\b[A-z]\\b{1}", replacement = " ", corpus2)

# Now we strip the excess white space.

corpus2 <- stripWhitespace(corpus2)

# Now let's remove specific words, namely the names of the speakers

corpus2 <- removeWords(corpus2, c("mr", "english"))

######################################################################
#  Creaate a comparison wordcloud
######################################################################

wordcloud(corpus2)  # not very descriptive
wordcloud(corpus2, random.order = FALSE)  # a little better
wordcloud(corpus2, random.order = FALSE, color = (rainbow(5)))  # better still, but...

# Can I separate the wordcloud into the number of documents in my corpus, so that I
# don't have just one wrdcloud, but rather two or three, or whatever, to reflect the
# number of documents, in this case two?  Yes....

# We use another function in the "wordcloud" package called comparison.cloud()
# but first we have to create a corpus, officially.Let's create a variable called
# "nain_corpus" and make that an official corpus.

main_corpus <- Corpus(VectorSource(corpus2))

# Using the "tm" package we created an abstract entity called a "Corpus" wich is made
# up of two documents.

# Now let's create a "term document matrix".  In our case a term document matrix 
# would create a list of EVERY term in both documents as observations and the two
# documents as variables (or columns).  The cells of the matrix are frequencies of
# occurences of that term within the document.  "0" will be very common.

# Let's store our term document matrix in a variable called "tdm"

tdm <- TermDocumentMatrix(main_corpus)
tdm
# terms: 1080, documents: 2)>>  We have 1080 terms, with 2 columns (variables)
# Non-/sparse entries: 1249/911 We have 1249 non-empty cells, and 911 empty cells
# Sparsity           : 42%
# Maximal term length: 16
# Weighting          : term frequency (tf)

# now let's convert the tdm to an actual dataframe-like matrix.
# The call returns a list of terms in alphabetical order, with the frequency of
# each term for the two cocuments.

as.matrix(tdm)

# Let's store the results in a variable called "m".

m <- as.matrix(tdm)

# So now we want to create a wordcloud for both documents and to render the
# wordclouds onto the same graphic.

# Let's change the column names from "1" and "2" to "obama" and "trump"
colnames(m)
colnames(m) <- c("kennedy", "nixon")

# Now we can create a comparison wordcloud on our "m"

comparison.cloud(m)

######################################################################
#   word cloud using ggplot  Fail, as of now
######################################################################
library(stringr)
word_bag <- str_split(corpus2, pattern = "\\s+")
word_bag

############  Test using ggplot2  FAIL

install.packages("ggwordcloud")
library(ggwordcloud)

df <- as_tibble(m)

set.seed(42)
ggplot(df, aes(label = kennedy)) +
  geom_text_wordcloud() +
  theme_minimal()
