get_dtm <- function(data) {  
  # data prep
  corpus <- Corpus(VectorSource(data$tweet))

  remove <- function(y) mgsub(y, c(":3","http://","\n"), "")
  corpus <- tm_map(corpus, remove)
  replace <- function(x) mgsub(x, c("&gt;","&lt;","><", ">.<", "=(", "T_T"," D:"), c(">","<","squint","squint","sad","sad","sad"))
  corpus <- tm_map(corpus,replace)
  replace_happy <- function(x) mgsub(x, c("^.^", "^^", "^_^", "B^)","B-)","=D"), "happy")
  corpus <- tm_map(corpus, replace_happy)
  corpus <- tm_map(corpus, replace_emoticon)
  
  corpus <- tm_map(corpus,content_transformer(tolower))
  corpus <- tm_map(corpus,removeWords,stopwords("english"))
  corpus <- tm_map(corpus,removePunctuation)
  corpus <- tm_map(corpus,removeNumbers) # do we need numbers for temperature?
  corpus <- tm_map(corpus,stemDocument)

  
  # remove words from words used >500 times in dtm but no sentiment implied
  corpus <- tm_map(corpus,removeWords,c("amp","rt","mention","link","weather","day","outsid","today","tonight","morn","may","get","time","will","mph","now","week","north","come","back",""))
  
  # document term matrix
  dtm <- DocumentTermMatrix(corpus)
  dtm
  findFreqTerms(dtm,lowfreq=50)
  freqterms <- findFreqTerms(dtm,lowfreq=500)
  
  table(alldata)
  
  dtm <- removeSparseTerms(dtm,0.995)
  dtm
  
  datasparse <- as.data.frame(as.matrix(dtm))
  colnames(datasparse) <- make.names(colnames(datasparse))
  datasparse$sentiment <- as.factor(data$sentiment)
  datasparse$id <- data$id
  str(datasparse)  

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

combine_traintest <- function() {
  train <- read.csv('train.csv')
  test <- read.csv('test.csv')
  train$id <- NA
  test$sentiment <- NA
  data <- rbind(train,test)
  write.csv(data, 'alldata.csv', row.names = F)
  # datasparse <- get_dtm(data)
  # write.csv(datasparse, 'testsparse.csv', row.names = F)
}