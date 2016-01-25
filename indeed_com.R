library(rvest)
library(stringr)

base_url <- "http://www.indeed.com/cmp/YOUR_FAVORITE_COMPANY/reviews?fcountry=ALL&fjobtitle=ALL&start="

#l <- as.numeric(sapply(reviews,function(x){length(unlist(strsplit(x," ")))}))

pages <- seq(0,520,20)
num_reviews_per_page <- numeric(length(pages))

review_text  <- character(600)
review_stars <- numeric(600)
review_title <- character(600)
review_date  <- character(600)

review_state <- character(600)


iterator <- 0
for(p in pages) {
  
  url <- paste(base_url,p,sep="")
  
  current_page <- read_html(url)
  num_reviews_current_page <-  length(html_text(html_nodes(current_page,"body div .cmp-review .cmp-review-description")))
  num_reviews_per_page[p/20 + 1] <- num_reviews_current_page
  
  for (r in 1:num_reviews_current_page) {
    iterator <- iterator + 1
  
    review_text[iterator]        <- html_text(html_nodes(current_page,"body div .cmp-review .cmp-review-description"))[r]
    review_stars[iterator]       <- as.numeric(html_attr(html_nodes(current_page,"body div .cmp-review .cmp-review-heading .cmp-rating-expandable span .cmp-value-title"),"title"))[r]
    review_title[iterator]       <- html_text(html_nodes(current_page,"body div .cmp-review .cmp-review-heading .cmp-review-title"))[r]
    review_date[iterator]        <- html_text(html_nodes(current_page,"body div .cmp-review .cmp-review-subtitle .cmp-review-date-created"))[r]
    
    review_state[iterator]    <- word(html_text(html_nodes(current_page,".cmp-review-subtitle .cmp-reviewer-job-location")),-1)[r]
    }
  
}


df <- data.frame(text=review_text,stars=review_stars,title=review_title,date=review_date,state=review_state)

df <- unique(df)

df <- df[-nrow(df),]

df$text <- as.character(df$text)
df$title <- as.character(df$title)
df$date <- as.character(df$date)


df$date <- as.Date(df$date,"%B %d, %Y")

df <- df[order(df$date),]

plot(aggregate(df$stars~as.numeric(format(df$date,"%Y%m")),FUN=median))
lines(lowess(aggregate(df$stars~as.numeric(format(df$date,"%Y%m")),FUN=median)))

aggregate(df$stars~as.numeric(format(df$date,"%Y")),FUN=mean)
