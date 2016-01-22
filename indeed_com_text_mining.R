library("RWeka")
library("tm")
library("wordcloud")

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 3))

crp <- Corpus(VectorSource(df$text))

crp <- tm_map(crp,tolower)
crp <- tm_map(crp,removePunctuation)
crp <- tm_map(crp,removeNumbers)
crp <- tm_map(crp,removeWords,stopwords("en"))


crp <- tm_map(crp,PlainTextDocument)

tdm <- TermDocumentMatrix(crp, control = list(tokenize = BigramTokenizer))

findFreqTerms(tdm,lowfreq=10)

findAssocs(tdm,terms = "fast paced environment",corlimit = .55)

inspect(tdm)


v <- sort(rowSums(as.matrix(tdm)),decreasing=TRUE)
n <- names(v)

wordcloud(n,v,min.freq=20)
