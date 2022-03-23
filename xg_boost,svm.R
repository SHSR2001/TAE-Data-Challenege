install.packages('tm')
install.packages("caret")
install.packages('tidyverse')    
install.packages('kernlab')      
install.packages('e1071')        
install.packages('ISLR')         
install.packages('RColorBrewer') 
install.packages('xgboost')
update.packages('mgsub')
install.packages('textclean')

library(mgsub)
library(tidyverse)    
library(kernlab)      
library(e1071)        
library(ISLR)         
library(RColorBrewer) 
library(tm)
library(SnowballC)
library(caret)
library(textclean)

library(xgboost)
library(readr)
library(stringr)
library(car)

library(tm)
library(SnowballC)
library(textclean)

get_dtm <- function(data) {  
  # data prep
  corpus <- Corpus(VectorSource(data$tweet))
  
  replace_misc <- function(x) mgsub(x, c("&gt;","&lt;","><", ">.<","(;",";_"), c(">","<","squint","squint","wink","crying"))
  corpus <- tm_map(corpus,replace_misc)
  replace_frown <- function(x) mgsub(x, c("=(", " D:"," ):","-.-","-_",";(","==","=.="), "frown")
  corpus <- tm_map(corpus,replace_frown)
  replace_smiley <- function(x) mgsub(x, c("^.^", "^^", "^_", "B^)","B-)","=D",": )"," :3","(:","[:"), "smiley")
  corpus <- tm_map(corpus, replace_smiley)
  replace_underscore <- function(x) mgsub(x, c("o.o","o.0","o_","_o","0_","._","/:"),"frown")
  corpus <- tm_map(corpus,replace_underscore)
  remove <- function(y) mgsub(y, c(":3","http://","\n"), "")
  corpus <- tm_map(corpus, remove)
  corpus <- tm_map(corpus, replace_emoticon)
  
  corpus <- tm_map(corpus,content_transformer(tolower))
  corpus <- tm_map(corpus,removeWords,stopwords("english"))
  corpus <- tm_map(corpus,removePunctuation)
  corpus <- tm_map(corpus,removeNumbers) 
  corpus <- tm_map(corpus,stripWhitespace)
  corpus <- tm_map(corpus,stemDocument)
  
  # corpus <- tm_map(corpus,removeWords,c("back","look","can","got","come","degre","mention","weather","day","outsid","today","tonight","morn","may","get","time","will","now","week","summer","readi"))
  
  # document term matrix
  dtm <- DocumentTermMatrix(corpus)
  findFreqTerms(dtm,lowfreq=50)
  findFreqTerms(dtm,lowfreq=500)
  
  dtm <- removeSparseTerms(dtm,0.999)
  dtm
  
  datasparse <- as.data.frame(as.matrix(dtm))
  colnames(datasparse) <- make.names(colnames(datasparse))
  datasparse$sentiment <- as.factor(data$sentiment)
  datasparse$id <- data$id
  # str(datasparse)  
  
  findAssocs(dtm,terms="counti", corlimit=.25)
  
  length(findFreqTerms(dtm,lowfreq=50))
  for(i in 1:70) {
    mean <- mean(as.numeric(datasparse[datasparse[,i]!=0,'sentiment']),na.rm = T)
    var <- var(as.numeric(datasparse[datasparse[,i]!=0,'sentiment']),na.rm = T)
    if(1.5<mean && mean<2.5 && var>0.5) {
      print(c(colnames(datasparse[i]),mean,var))
      datasparse <- subset(datasparse, select=-i)
    }
  }
  
  mean(as.numeric(datasparse[datasparse$frown!=0,'sentiment']),na.rm = T)
  var(as.numeric(datasparse[datasparse$frown!=0,'sentiment']),na.rm = T)
  
  # datasparse <- subset(datasparse, select=-counti)
  
  # # word cloud
  # if(!require(wordcloud)){
  #   install.packages("wordcloud")
  #   library(wordcloud)
  # }
  # # Get word counts in decreasing order
  # word_freqs = sort(colSums(datasparse), decreasing=TRUE)
  # # Create data frame with words and their frequencies
  # dm = data.frame(word=names(word_freqs), freq=unname(word_freqs))
  # # Plot wordcloud (we limit the plot to 100 words)
  # wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"), max.words=100)
  
  return(datasparse)  
}

trainraw <- read.csv('train.csv')
trainsparse <- get_dtm(trainraw)
ncol(trainsparse)
str(trainsparse$sentiment)

testraw <- read.csv('test.csv')
testsparse <- get_dtm(testraw)
ncol(testsparse)
#str(trainsparse$sentiment)

alldata <- read.csv('alldata.csv')
alldata_sparse <- get_dtm(alldata)
ncol(alldata_sparse)

alldata_sparse

library(caTools)
set.seed(123)
# Create train and test sets (with balanced response)
spl <- sample.split(trainsparse$sentiment,SplitRatio=0.7)
train <- subset(trainsparse,spl==TRUE)
test <- subset(trainsparse,spl==FALSE)


# SVM

svm1 <- svm(sentiment~., data=train, 
              method="C-classification", kernal="radial", 
              gamma=0.1, cost=10)

summary(svm1)

prediction <- predict(svm1, test)
xtab <- table(test$sentiment, prediction)
xtab

## Accuracy

(839 + 1291 + 2165)/nrow(test) 

# XGBoost

trytrain <- subset(alldata_sparse,is.na(alldata_sparse$id))
trytest <- subset(alldata_sparse,is.na(alldata_sparse$sentiment))

df_xgboost = trytrain
df_xgboost$ones <- 1
df_xgboost$sentiment <- (as.integer(df_xgboost$sentiment) - df_xgboost$ones)
#df_xgboost$sentiment <- as.factor(df_xgboost$sentiment)

a <- trytrain[,-219]
b <- trytest[,-219]

train_data   <- as.matrix(a[,-219])
train_label  <- df_xgboost[,"sentiment"] 
train_matrix <- xgb.DMatrix(data = train_data, label = train_label)


test_data  <- as.matrix(b[,-219])
#test_label <- data_label[-train_index]
test_matrix <- xgb.DMatrix(data = test_data)

# ---------------------- LEAVE ----------------------------------------------#

set.seed(123)
train_index <- sample(1:nrow(trainsparse), nrow(trainsparse)*0.70)

data_variables <- as.matrix(trainsparse[,-221])
data_label <- df_xgboost[,"sentiment"] 
data_matrix <- xgb.DMatrix(data = data_variables, label = data_label)


#training_data <- as.data.frame(train_data)
#testing_data <- as.data.frame(test_data)

#training_data$sentiment = train_label
#testing_data$sentiment = test_label

#training_data$sentiment = as.factor(training_data$sentiment)
#testing_data$sentiment = as.factor(testing_data$sentiment)

#df_xgboost = testsparse
#df_xgboost$ones <- 1
#df_xgboost$sentiment <- (as.integer(df_xgboost$sentiment) - df_xgboost$ones)
#df_xgboost$sentiment <- as.factor(df_xgboost$sentiment)

test_variables <- as.matrix(testsparse)
#test_label <- df_xgboost[,"sentiment"] 
test_matrix <- xgb.DMatrix(data = test_variables)


numberOfClasses <- length(unique(trainsparse$sentiment))

#-------------------------------- LEAVE --------------------------------------#

xgb_params <- list( "objective" = "multi:softmax",
                   "eval_metric" = "mlogloss",
                   "num_class" = 3)
nround    <- 50 # number of XGBoost rounds
cv.nfold  <- 5

#To find best iteration
xgbcv <- xgb.cv(params = xgb_params, data = train_matrix, nrounds = 300, nfold = 5, showsd = T, stratified = T, print.every.n = 10, early.stop.round = 20, maximize = F)

#min(xgbcv$evaluation_log$test_mlogloss_mean)

# Fit cv.nfold * cv.nround XGB models and save OOF predictions
bst_model <- xgb.train(
                   params = xgb_params,
                   data = train_matrix, 
                   nrounds = 184,
                   #nfold = cv.nfold,
                   #verbose = FALSE,
                   #prediction = TRUE)
                   )

test_pred <- predict(bst_model, newdata = test_matrix)
test_pred <- test_pred + 1

predexport <- cbind(id=trytest$id,sentiment=test_pred)
write.csv(predexport,'please_work.csv',row.names = F)


test_prediction <- matrix(test_pred, nrow = numberOfClasses,
                          ncol=length(test_pred)/numberOfClasses) %>%
  t() %>%
  data.frame() %>%
  mutate(label = test_label + 1,
         max_prob = max.col(., "last"))
# confusion matrix of test set
confusionMatrix(factor(test_prediction$max_prob),
                factor(test_prediction$label),
                mode = "everything")

(1743 + 2062 + 1984)/nrow(test)

## ------------------------ TEST TRAIN ALL THAT ------------------- ##


set.seed(123)
train_index <- sample(1:nrow(trainsparse), nrow(trainsparse)*0.70)


df_xgboost = trainsparse
df_xgboost$ones <- 1
df_xgboost$sentiment <- (as.integer(df_xgboost$sentiment) - df_xgboost$ones)


typeof(trainsparse)
typeof(data_label)

data_variables <- as.matrix(trainsparse[,-881])
#data_variables <- as.integer(data_variables)
data_label <- as.matrix(df_xgboost[,"sentiment"])
data_matrix <- xgb.DMatrix(data = data_variables, label = data_label)

train_data   <- as.matrix(trainsparse[,-881])[train_index]
train_label  <- as.matrix(df_xgboost[,"sentiment"])[train_index]
train_matrix <- xgb.DMatrix(data = train_data, label = train_label)

test_data  <- data_variables[-train_index]
test_label <- data_label[-train_index]
test_matrix <- xgb.DMatrix(data = test_data, label = test_label)

train_index <- c(train_index)

training_data <- trainsparse[train_index,]
testing_data <- trainsparse[-train_index,]


training_data$sentiment = train_label
testing_data$sentiment = test_label

xgb_params <- list( "objective" = "multi:softmax",
                    "eval_metric" = "mlogloss",
                    "num_class" = 3)
nround    <- 50 # number of XGBoost rounds
cv.nfold  <- 5

#To find best iteration
xgbcv <- xgb.cv(params = xgb_params, data = train_matrix, nrounds = 300, nfold = 5, showsd = T, stratified = T, print.every.n = 10, early.stop.round = 20, maximize = F)

#min(xgbcv$evaluation_log$test_mlogloss_mean)

# Fit cv.nfold * cv.nround XGB models and save OOF predictions
bst_model <- xgb.train(
  params = xgb_params,
  data = train_matrix, 
  nrounds = 184,
  #nfold = cv.nfold,
  #verbose = FALSE,
  #prediction = TRUE)
)

test_pred <- predict(bst_model, newdata = test_matrix)
test_pred <- test_pred + 1

predexport <- cbind(id=trytest$id,sentiment=test_pred)
write.csv(predexport,'please_work.csv',row.names = F)


test_prediction <- matrix(test_pred, nrow = numberOfClasses,
                          ncol=length(test_pred)/numberOfClasses) %>%
  t() %>%
  data.frame() %>%
  mutate(label = test_label + 1,
         max_prob = max.col(., "last"))
# confusion matrix of test set
confusionMatrix(factor(test_prediction$max_prob),
                factor(test_prediction$label),
                mode = "everything")




## --- HYPER-P.M-TUNING-----------------------

mat <- xgb.importance (feature_names = colnames(train_data),model = bst_model)
xgb.plot.importance (importance_matrix = mat[1:20]) 

library(mlr)
library(parallel)
library(parallelMap) 
parallelStartSocket(cpus = detectCores())


fact_col <- colnames(train)[sapply(trytrain,is.character)]

for(i in fact_col) set(trytrain,j=i,value = factor(train[[i]]))
for (i in fact_col) set(trytest,j=i,value = factor(test[[i]]))

trytest$sentiment <- as.factor(0)

traintask <- makeClassifTask(data = trytrain,target = "sentiment")
testtask <- makeClassifTask(data = trytest, target = "sentiment")

traintask <- createDummyFeatures (obj = traintask) 
testtask <- createDummyFeatures (obj = testtask)

lrn <- makeLearner("classif.xgboost",predict.type = "response")
lrn$par.vals <- list( objective="multi:softmax", eval_metric="mlogloss", nrounds=100L, eta=0.1)

params <- makeParamSet( makeDiscreteParam("booster",values = c("gbtree","dart")), makeIntegerParam("max_depth",lower = 3L,upper = 10L), makeNumericParam("min_child_weight",lower = 1L,upper = 10L), makeNumericParam("subsample",lower = 0.5,upper = 1), makeNumericParam("colsample_bytree",lower = 0.5,upper = 1))

rdesc <- makeResampleDesc("CV",stratify = T,iters=5L)
ctrl <- makeTuneControlRandom(maxit = 10L)

mytune <- tuneParams(learner = lrn, task = traintask, resampling = rdesc, measures = acc, par.set = params, control = ctrl, show.info = T)
mytune$y 

#set hyperparameters
lrn_tune <- setHyperPars(lrn,par.vals = mytune$x)

#train model
xgmodel <- train(learner = lrn_tune,task = traintask)

#predict model
xgpred <- predict(xgmodel,testtask)
predexport <- data.frame(xgpred$data$id, xgpred$data$response)
write.csv(predexport,'please_please_work.csv',row.names = F)

table(xgpred$data$response,xgpred$data$truth)

(1768+2085+2039)/(1768+2085+2039 + 100 + 155 + 231 + 222 + 78 + 73)


## ADABoost

library(adabag)
library(caret)

summary(train)

model = boosting(sentiment~., data=train, boos=TRUE, mfinal=50)


pred = predict(model, test)
print(pred$confusion)


(1746 + 1799 + 1698)/nrow(test) 

model = boosting.cv(sentiment~., data=train, boos=TRUE, mfinal=50)


pred = predict(model, test)
print(pred$confusion)


## GBM

library(rsample)      # data splitting 
library(gbm)          # basic implementation
library(xgboost)      # a faster implementation of gbm
library(caret)        # an aggregator package for performing many machine learning models
library(h2o)          # a java-based platform
library(pdp)          # model visualization
library(ggplot2)      # model visualization
library(lime)         # model visualization

# for reproducibility
set.seed(123)

# train GBM model
gbm.fit <- gbm(
  formula = sentiment ~ .,
  distribution = "multinomial",
  data = train,
  n.trees = 10000,
  interaction.depth = 1,
  shrinkage = 0.001,
  cv.folds = 5,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  

# print results
print(gbm.fit)

pred <- predict(gbm.fit, n.trees = gbm.fit$n.trees, test)
table(pred, test$sentiment)

