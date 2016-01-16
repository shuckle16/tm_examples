#install.packages('tm')
library(tm)

sci.electronics    <- Corpus(DirSource("sci.electronics")) # A corpus with 981 text documents
talk.religion.misc <- Corpus(DirSource("talk.religion.misc")) # A corpus with 628 text documents

############################
#### Text Data Cleaning ####
############################

# combine documents
c <- c(sci.electronics,talk.religion.misc)

# convert to lower case
res <- tm_map(c,tolower)
inspect(res[1:10])

# remove punctuation
res <- tm_map(res, removePunctuation)
inspect(res[1:10])

# remove numbers
res <- tm_map(res, removeNumbers)
inspect(res[1:10])

# stemming
res <- tm_map(res,stemDocument)
inspect(res[1:10])

# remove stop words
res <- tm_map(res, removeWords, stopwords("english"))
inspect(res[1:10])

# remove additional stop words
res <- tm_map(res, removeWords, c("one","can"))
inspect(res[1:10])

# remove extra white spaces
res <- tm_map(res,stripWhitespace)
inspect(res[1:10])

# document-term matrix
res <- tm_map(res, PlainTextDocument) # compatibility issue
dtm <- DocumentTermMatrix(res)
inspect(dtm)

##################################
#### Frequency Based Analysis ####
##################################

w<-findFreqTerms(dtm,200)
dtm1 <- dtm[,w]
findAssocs(dtm,"war",0.2)

# List unique words sorted by decreasing frequency
term.freq <- apply(dtm, 2, sum)
barplot(sort(term.freq,decreasing=TRUE))
barplot(sort(term.freq,decreasing=TRUE)[1:30])

# remove sparse terms
term_wt <- apply(dtm,2,sum)
dtm <- dtm[,term_wt>10]

##################################
#### Word Cloud Visualization ####
##################################

#install.packages("wordcloud")
library(wordcloud)

term.table <- data.frame(term=names(term.freq),frequency=term.freq)
term.table <- term.table[order(term.freq,decreasing=TRUE),]
head(term.table) # you may optionally save the word frequency table in a file
wordcloud(term.table$term, term.table$frequency, min.freq=50)

#### Compare the word cloud of "sci.electronics" and "talk.religion.misc" corpuses 
#### after the same data cleaning preprocessing above

###################################
#### Weighting Terms by TF-IDF ####
###################################

dtm.tfxidf <- weightTfIdf(dtm)
inspect(dtm.tfxidf[1:10, 1:10])

#### Question: Compare the word cloud of "sci.electronics" by TF and by TF-IDF

#### Question: find out about the weight of the word "subject" before and after tfidf conversion
#### How does it change? Why?

#############################################
#### Data Preparation for Classification ####
#############################################

# Transform the corpus back into a dataframe
df <- as.data.frame(as.matrix(dtm))

# This is the vector of class labels correponding to each document
class.labels <- c(rep("E",length(sci.electronics)),rep("R",length(talk.religion.misc)))
df <- cbind(class.labels,df)

# Use a random subset of 75% documents as training
train.ind <- rbinom(nrow(df), size=1, prob=0.75)
training.data <- df[train.ind==1,]
testing.data  <- df[train.ind==0,]

################################
#### naive Bayes classifier ####
################################

#install.packages("e1071")
library(e1071)

# train the model
mybayes <- naiveBayes(class.labels ~ ., data=training.data) 

# compare the predicted and the actual
testing <- predict(mybayes, testing.data)
table(testing, testing.data$class.labels, dnn=list('predicted','actual'))

# class distribution (prior)
mybayes$apriori

# conditional distributions for each attribute
mybayes$tables

# Dimension reduction question?
# What if I am looking for the most representative words for different classes?









