library(rvest)
library(stringr)
library("RWeka")
library("tm")
library("topicmodels")

e <- read_html("http://www.nber.org/papersbyprog/ED_archive.html")
links <- html_attr(html_nodes(e,"#mainTable a"), "href")

begin <- Sys.time()
titles <- lapply(links,FUN=function(x){html_text(html_nodes(read_html(x),".title"))})
Sys.time() - begin

titles <- unlist(titles)

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 2))

crp <- Corpus(VectorSource(titles))

crp <- tm_map(crp,tolower)
crp <- tm_map(crp,removePunctuation)
crp <- tm_map(crp,removeNumbers)
crp <- tm_map(crp,removeWords,stopwords("en"))
crp <- tm_map(crp,removeWords,c("education","schools","achievement","education","effect",
                                "evidence","school","effects","schooling","performance",
                                "student","students","teacher","teachers","outcomes",
                                "college"))

crp <- tm_map(crp,PlainTextDocument)

dtm <- DocumentTermMatrix(crp, control = list(tokenize = BigramTokenizer))

l <- LDA(dtm,3)
get_terms(l,10)
