library(devtools)
library(randomForest)
library(e1071)
library(AUC)

sp1 <- removeSparseTerms(dtm,.99)
sp2 <- removeSparseTerms(dtm_loves,.99)


ml <- data.frame(data.matrix(tm:::c.DocumentTermMatrix(sp1,sp2)))

row.names(ml) <- NULL

y <- as.factor(c(rep(1,sp1$nrow),rep(0,sp2$nrow)))

source_url("https://raw.githubusercontent.com/shuckle16/ml/master/train_test_split.R")
source_url("https://raw.githubusercontent.com/shuckle16/ml/master/oversample.R")


tts <- train_test_split(ml,y)

basetabletrain <- tts$basetabletrain
basetabletest  <- tts$basetabletest
ytrain         <- tts$ytrain
ytest          <- tts$ytest

os <- oversample(basetabletrain,ytrain)

basetabletrain <- os$basetabletrain
ytrain <- os$ytrain

rf <- randomForest(ytrain~.,data=basetabletrain,importance=TRUE)

varImpPlot(rf)

table(predict(rf,newdata=basetabletest),ytest)

AUC::auc(AUC::roc(predict(rf,newdata=basetabletest),ytest))


nb <- naiveBayes(ytrain~.,data=basetabletrain)

table(predict(nb,basetabletest),ytest)

AUC::auc(AUC::roc(predict(nb,basetabletest),ytest))
