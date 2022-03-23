setwd("C:\\Users\\Seow Shuen\\Documents\\GitHub\\TAE-data-challenge")
library(tm)
library(SnowballC)

source('utility.R')
trainraw <- read.csv('train.csv')
trainsparse <- get_dtm(trainraw)
str(trainsparse$sentiment)

library(caTools)
set.seed(123)
# Create train and test sets (with balanced response)
spl <- sample.split(trainsparse$sentiment,SplitRatio=0.7)
train <- subset(trainsparse,spl==TRUE)
test <- subset(trainsparse,spl==FALSE)

## logit
model1 <- glm(sentiment~.,data=train,family=binomial)
summary(model1)

predict1 <- predict(model1,newdata=test,type="response")
table(predict1>=0.5,test$sentiment)

## cart
library(rpart)
library(rpart.plot)
model2 <- rpart(sentiment~.,data=train)

prp(model2,type=4,extra=4)
predict2 <- predict(model2,newdata=test,type="class")
table(predict2,test$sentiment)
acc2 <- sum(diag(table(predict2,test$sentiment)))/sum(table(predict2,test$sentiment))
acc2 #0.7638519

# deeper then pruned
model2a <- rpart(sentiment~.,data=train,cp=10^-6) 
prp(model2a,type=4,extra=4) 
printcp(model2a)
model2b <- prune(model2a,cp=4.8924e-05) # To prune, we use the smallest value of xerror

prp(model2b,type=4,extra=4)
predict2b <- predict(model2b,newdata=test,type="class")
table(predict2b,test$sentiment)
acc2b <- sum(diag(table(predict2b,test$sentiment)))/sum(table(predict2b,test$sentiment))
acc2b #0.8247407

## random forest
library(randomForest)
# We use the default parameters to fit the model (500 trees)
model3 <- randomForest(sentiment~.,data=train) 
model3
varImpPlot(model3) #variable importance

predict3 <- predict(model3,newdata=test,type="class")
table(predict3,test$sentiment)
acc3 <- sum(diag(table(predict3,test$sentiment)))/sum(table(predict3,test$sentiment))
acc3 #0.850963

## naive bayes classifier
library(e1071)
model4 <- naiveBayes(as.factor(sentiment)~.,data=train)
model4$apriori
predict4 <- predict(model4,newdata = test,type = "class")
table(predict4,test$sentiment)
acc4 <- sum(diag(table(predict4,test$sentiment)))/sum(table(predict4,test$sentiment))
acc4 #0.7561481

## generate prediction
alldata <- read.csv('alldata.csv')
allsparse <- get_dtm(alldata)
trytrain <- subset(allsparse,is.na(allsparse$id))
trytest <- subset(allsparse,is.na(allsparse$sentiment))

trymodel <- randomForest(as.factor(sentiment)~.,data=trytrain) # change to best performing model
pred <- predict(trymodel,newdata=trytest,type="class")
predexport <- cbind(id=trytest$id,sentiment=pred)
# write.csv(predexport,'testpred1.csv',row.names = F)