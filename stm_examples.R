# super cool topic modeling stuff

library(stm)

temp <- textProcessor(titles,stem = FALSE)

docs <- temp$documents
vocab <- temp$vocab

k <- c(3,4,8,16) 
s <- searchK(docs,vocab,K=k)
optimal_k <- s$results[which.min(s$results$residual),]$K
m <- stm(docs,vocab,optimal_k,max.em.its = 30)

plot(m)
plot(topicCorr(m))


findThoughts(m,texts = titles,topics=11)

toLDAvis(m,docs)


