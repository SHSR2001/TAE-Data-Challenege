setwd("C:\\Users\\Seow Shuen\\Documents\\GitHub\\TAE-data-challenge")
library(tm)
library(SnowballC)

source('utility.R')
trainraw <- read.csv('train.csv')
trainsparse <- get_dtm(trainraw)

## pca
trainsparse$sentiment <- as.numeric(trainsparse$sentiment)
trainsparse <- subset(trainsparse, select=-sentiment)

pr.out.ns=prcomp(trainsparse, scale=F)
pr.out.ns$rotation     ### gives the weights for the PCAs (eigen vectors)
pr.out.ns$x[1:10,1:5]  ### gives PCA value for 1st 10 countries and first 5 PCAs

#plot scree: plot of variance of each PCA
plot(pr.out.ns, type="l", main="Scree plot")

#plot cumulative prop: proportional variance (cumulative)
pr.var.ns=pr.out.ns$sdev^2
pve.ns=pr.out.ns$sdev^2/sum(pr.out.ns$sdev^2)
plot(cumsum(pve.ns), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1),type='b')+abline(h=0.9,col="red")
# first PC explanation
pve.ns[1]
sum(pve.ns[1:150])

#get first 150 pcs into dataframe of features
pctrainsparse <- data.frame(pr.out.ns$x[,1:150])
pctrainsparse$sentiment <- trainraw$sentiment

library(caTools)
set.seed(123)
# Create train and test sets (with balanced response)
spl <- sample.split(pctrainsparse$sentiment,SplitRatio=0.7)
train <- subset(pctrainsparse,spl==TRUE)
test <- subset(pctrainsparse,spl==FALSE)

## logit
model1 <- glm(sentiment~.,data=train,family=binomial)
summary(model1)

predict1 <- predict(model1,newdata=test,type="response")
table(predict1>=0.5,test$sentiment)

## cart
library(rpart)
library(rpart.plot)
model2 <- rpart(sentiment~.,data=train,method="class")

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

allsparse$sentiment <- as.numeric(allsparse$sentiment)
allsparse.rmna <- subset(allsparse,select=-c(sentiment,id))
sum(is.na(allsparse.rmna))

# write.csv(allsparse.rmna,'PCAout.csv',row.names = F)

pr.out.ns=prcomp(allsparse.rmna, scale=F)
pr.out.ns$rotation     ### gives the weights for the PCAs (eigen vectors)
pr.out.ns$x[1:10,1:5]  ### gives PCA value for 1st 10 countries and first 5 PCAs

#plot scree: plot of variance of each PCA
plot(pr.out.ns, type="l", main="Scree plot")

#plot cumulative prop: proportional variance (cumulative)
pr.var.ns=pr.out.ns$sdev^2
pve.ns=pr.out.ns$sdev^2/sum(pr.out.ns$sdev^2)
plot(cumsum(pve.ns), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1),type='b')+abline(h=0.9,col="red")
# first PC explanation
pve.ns[1]
sum(pve.ns[1:150])

#get first 150 pcs into dataframe of features
pc.allsparse <- data.frame(pr.out.ns$x[,1:150])
pc.allsparse$sentiment <- allsparse$sentiment
pc.allsparse$id <- allsparse$id

trytrain <- subset(pc.allsparse,is.na(pc.allsparse$id))
trytest <- subset(pc.allsparse,is.na(pc.allsparse$sentiment))

trymodel <- rpart(as.factor(sentiment)~.,data=trytrain) # change to best performing model
pred <- predict(trymodel,newdata=trytest,type="class")
predexport <- cbind(id=trytest$id,sentiment=pred)
write.csv(predexport,'pca.cart.csv',row.names = F)

