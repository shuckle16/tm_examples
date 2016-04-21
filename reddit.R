# basic topic modeling on specified subreddit


library(rvest)
library(stringr)
library(tm)
library(topicmodels)

sr <- "programming"
url <- paste("http://www.reddit.com/r/",sr,"?limit=100",sep="")


pol <- read_html(url)
iterator <- 1

full <- data.frame(ups=numeric(),titles=character(),title_length=character(),dates=character(),weekday=factor())

while (iterator < 21) {
   ups <- as.numeric(html_text(html_nodes(pol, ".score.likes")))
   ups[is.na(ups)] <- 0

   titles <- html_text(html_nodes(pol,"p .title"))
   title_length <- as.numeric(sapply(titles,FUN=function(x){length(unlist(strsplit(x," ")))}))

   times <- html_attr(html_nodes(pol,".live-timestamp"),"datetime")
   dates <- as.Date(substr(times,1,10))
   weekday <- weekdays(dates)
   
   full <- rbind(full,data.frame(ups,titles,title_length,dates,weekday))
   
   next_page <- html_attr(html_nodes(pol,".nextprev a"),"href")
   pol <- read_html(next_page[length(next_page)])
   iterator <- iterator + 1
}


crp <- Corpus(VectorSource(full$titles))

crp <- tm_map(crp,removePunctuation)
crp <- tm_map(crp,tolower)
crp <- tm_map(crp,removeWords,stopwords("en"))
crp <- tm_map(crp,removeWords,c("the","are"))

crp <- tm_map(crp,PlainTextDocument)

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))

dtm <- DocumentTermMatrix(crp,control=list(tokenize=BigramTokenizer)

l <- LDA(dtm,3)

get_terms(l,5)


tbl <- colSums(as.matrix(DocumentTermMatrix(crp)))

barplot(head(tbl[order(tbl,decreasing=TRUE)],10),las=2)


