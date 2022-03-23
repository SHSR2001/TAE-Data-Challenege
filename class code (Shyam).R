setwd("C:\\Users\\Shyam\\Documents\\GitHub\\TAE-data-challenge")
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

##Multi-nomial nb

df_nb = trainsparse
df_nb$ones <- 1
df_nb$sentiment <- (as.integer(df_nb$sentiment) - df_nb$ones)
df_nb$sentiment <- as.factor(df_nb$sentiment)

set.seed(123)
train_index <- sample(1:nrow(trainsparse), nrow(trainsparse)*0.70)

data_variables <- as.matrix(trainsparse[,-217])
data_label <- df_nb[,"sentiment"] 
data_matrix <- xgb.DMatrix(data = data_variables, label = data_label)


train_data   <- data_variables[train_index,]
train_label  <- data_label[train_index]
train_matrix <- xgb.DMatrix(data = train_data, label = train_label)


test_data  <- data_variables[-train_index,]
test_label <- data_label[-train_index]
test_matrix <- xgb.DMatrix(data = test_data, label = test_label)

laplace <- 1

install.packages('naivebayes')
library(naivebayes)

mnb <- multinomial_naive_bayes(x = train_data, y = train_label, laplace = laplace)
summary(mnb)

head(predict(mnb, newdata = test_data, type = "class")) 

prediction <- predict(mnb, newdata = test_data, type = "class")
prediction <- as.integer(prediction) 
prediction

table(prediction,as.integer(test_label))
acc3 <- sum(diag(table(prediction,as.integer(test_label))))/sum(table(prediction,as.integer(test_label)))
acc3

## generate prediction
alldata <- read.csv('alldata.csv')
allsparse <- get_dtm(alldata)
trytrain <- subset(allsparse,is.na(allsparse$id))
trytest <- subset(allsparse,is.na(allsparse$sentiment))

trymodel <- randomForest(as.factor(sentiment)~.,data=trytrain) # change to best performing model
pred <- predict(trymodel,newdata=trytest,type="class")
predexport <- cbind(id=trytest$id,sentiment=pred)
# write.csv(predexport,'testpred1.csv',row.names = F)