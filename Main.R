library(tm)
library(SnowballC)
library(wordcloud)
library(ggplot2)
library(dplyr)
library(tidyr)
library(topicmodels)

#Calling setlocale() seems to cause issues with importing the data files (quotation marks get replaced with <U+201C>, for example)
#Removing this function call fixes the issue (**Ask about this**)
#Sys.setlocale("LC_ALL", "C")

#Create a list of all the files
file.list <- list.files(path="./data", pattern=".csv",full.names = TRUE)

#Loop over file list importing them and binding them together
D1 <- do.call("rbind", lapply(file.list, read.csv, header = TRUE, stringsAsFactors = FALSE, na.strings=c("")))

#Import the week-list data
D2 <- read.csv("./data/Week-List/week-list.csv", header = TRUE)

#Strip out HTML tags, comments, and the &nbsp; whitespace command
D1$Notes <- gsub("</?\\w+((\\s+\\w+(\\s*=\\s*(?:\".*?\"|'.*?'|[\\^'\">\\s]+))?)+\\s*|\\s*)/?>|<!--[\\s\\S]*?-->", "", D1$Notes)
D1$Notes <- gsub("&nbsp;", " ", D1$Notes)

#Filter out rows with missing values in either the Title or Notes column (if no notes we don't 
# include it in the analysis anyway, and no title means we can't match it with the week-list data frame)
D1 <- filter(D1, !is.na(Notes) & !is.na(Title))

#Merge data frames
D3 <- merge(D1, D2, by.x = "Title", by.y = "Title", all.x = TRUE)

#Filter rows where week is NA, since we are only doing the analysis across each week
D3 <- filter(D3, !is.na(week))

#Convert the data frame to the corpus format that the tm package uses
corpus <- Corpus(VectorSource(D3$Notes))
#Remove spaces
corpus <- tm_map(corpus, stripWhitespace)
#Convert to lower case
corpus <- tm_map(corpus, content_transformer(tolower)) 
#Remove pre-defined stop words ('the', 'a', etc)
corpus <- tm_map(corpus, removeWords, stopwords('english'))
#Convert words to stems ("education" = "edu") for analysis, for more info see  http://tartarus.org/~martin/PorterStemmer/
corpus <- tm_map(corpus, stemDocument, lazy=TRUE)
#Remove numbers
corpus <- tm_map(corpus, removeNumbers, lazy=TRUE)
#remove punctuation
corpus <- tm_map(corpus, removePunctuation, lazy=TRUE)

#PROCESS TEXT
#Convert corpus to a term document matrix - so each word can be analyzed individuallly
tdm.corpus <- TermDocumentMatrix(corpus)

#SENTIMENT ANALYSIS
#Upload positive and negative word lexicons
positive <- readLines("./data/positive-words.txt")
negative <- readLines("./data/negative-words.txt")

#Search for matches between each word and the two lexicons
D3$positive <- tm_term_score(tdm.corpus, positive)
D3$negative <- tm_term_score(tdm.corpus, negative)

#Generate an overall pos-neg score for each line
D3$score <- D3$positive - D3$negative

#SENTIMENT SCORE PLOT
ggplot(D3, aes(week, score)) 
  + geom_point(color = "firebrick") 
  + xlab("Weeks") 
  + ylab("Sentiment Score") 
  + scale_x_continuous(
      label=function(x){return(paste("Week", x))}, 
      breaks = as.numeric(unlist(distinct(D3, week))))

#LDA TOPIC MODELING
#Term Frequency Inverse Document Frequency
dtm.tfi <- DocumentTermMatrix(corpus, control = list(weighting = weightTf))

#Remove very uncommon terms (term freq inverse document freq < 0.1)
dtm.tfi <- dtm.tfi[,dtm.tfi$v >= 0.1]

#Remove non-zero entries
rowTotals <- apply(dtm.tfi , 1, sum) #Find the sum of words in each Document
dtm.tfi   <- dtm.tfi[rowTotals> 0, ] #Divide by sum across rows

lda.model = LDA(dtm.tfi, k = 3, seed = 150)

#Which terms are most common in each topic
terms(lda.model)

#Which documents belong to which topic
topics(lda.model)

