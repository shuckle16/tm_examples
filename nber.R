library("rvest")
library("stringr")
library("RWeka")
library("tm")
library("topicmodels")

e <- read_html("http://www.nber.org/papersbyprog/ED_archive.html")
links <- html_attr(html_nodes(e,"#mainTable a"), "href")

begin <- Sys.time()
titles <- lapply(links,FUN=function(x)
  {
    h <- read_html(x)
    t <- html_text(html_nodes(h,".title"))
    ps <- html_nodes(h,"p")
    p <- html_text(ps[grepl(x=html_attr(ps,"style"),pattern="justify")])
    d <- html_text(html_nodes(h,"p.bibtop"))
    return(list(t=t,p=p,d=d))
  })
Sys.time() - begin

paras <- unlist(titles)[grepl("p\\b|p1\\b",names(unlist(titles)))]
dates <- unlist(titles)[which(names(unlist(titles))=="d")]
titlez <- unlist(titles)[which(names(unlist(titles))=="t")]

df <- data.frame(paras,dates,titlez)

write.csv(x=df,file = "~/nber_education.csv",quote = TRUE)

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 2))

crp <- Corpus(VectorSource(paras))

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
