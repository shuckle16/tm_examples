library("RWeka")
library("tm")
library("wordcloud")
library("topicmodels")

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))


#which df$stars == 1, etc
crp <- Corpus(VectorSource(df$text))

crp <- tm_map(crp,tolower)
crp <- tm_map(crp,removePunctuation)
crp <- tm_map(crp,removeNumbers)
crp <- tm_map(crp,removeWords,stopwords("en"))


crp <- tm_map(crp,PlainTextDocument)

tdm <- TermDocumentMatrix(crp, control = list(tokenize = BigramTokenizer))
dtm <- DocumentTermMatrix(crp, control = list(tokenize = BigramTokenizer))

findFreqTerms(dtm,lowfreq=10)

findAssocs(dtm,terms = "terrible",corlimit = .55)

inspect(dtm)


v <- sort(rowSums(as.matrix(tdm)),decreasing=TRUE)
n <- names(v)

wordcloud(n,v,min.freq=3)



l <- LDA(dtm,2)

get_terms(l,10)
