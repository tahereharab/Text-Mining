library(tm)
library(topicmodels)
library(readr)
library(fpc)
library(mclust)
library(cluster)
library(NbClust)
library(vegan)
library(readr)

options(header=FALSE, stringsAsFactors = FALSE, FileEncoding = "UTF-8")

media <- read_delim("E:/[Polimi Courses]/Thesis/R/instagram_user_cluster/media.csv", 
    ";", escape_double = FALSE, col_types = cols(m_date = col_datetime(format = "%Y/%m/%d %H:%M")), 
    na = "null", trim_ws = TRUE)
	
text <-as.vector(gsub(","," ",media$m_caption_norm))
username <- as.vector(media$m_user)
docs <- Corpus(VectorSource(text))
docs <-tm_map(docs,content_transformer(tolower))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, stripWhitespace)

myStopWords <- c("com", "new", "dai", "year", "che", "time", "thank", "just", "non","todai","del","sai","una","don","http","della","dell","sono"
, "love" , "good", "like", "statu", "happi", "world", "peopl", "come", "want", "make", "great", "look", "work", "follow", "merri", "life", "live",
"best", "beauti", "know", "wish", "thing", "need", "think", "night", "week", "let", "open","wai", "check", "help", "start", "natal", "post", "right", "person",
 "citi", "share" , "chang", "tonight", "latest", "final", "littl", "miss", "tutti", "stop", "dei", "realli", "die", "better", "big", "doe", "readi" ,
"tomorrow", "plai", "wait", "word", "real", "got", "sold", "speak", "begin" )
docs <- tm_map(docs, removeWords, myStopWords)

###DTM
dtm <- DocumentTermMatrix(docs, control = list(wordLenghts=c(3,15),bounds = list(global = c(100,Inf)))) 
rownames(dtm) <- username
rowTotals <- apply(dtm , 1, sum)
dtm   <- dtm[rowTotals> 0, ]
#creating ordered word frequency csv file
freq <- colSums(as.matrix(dtm))
ord <- order(freq,decreasing=TRUE)


###LDA
burnin <- 1000  
iter <- 1000     
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
verbose <- 1
best <- TRUE

#Number of topics
k <- 3
#Run LDA using Gibbs sampling
ldaOut <-LDA(dtm,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin, verbose=verbose))

ldaOut.topics <- as.matrix(topics(ldaOut))
write.csv(ldaOut.topics,file=paste("E:/[Polimi Courses]/Thesis/R/instagram_user_cluster/MediaLDAGibbs",k,"DocsToTopics.csv"))
   #top 10 terms in each topic
ldaOut.terms <- as.matrix(terms(ldaOut,15))
write.csv(ldaOut.terms,file=paste("E:/[Polimi Courses]/Thesis/R/instagram_user_cluster/MediaLDAGibbs",k,"TopicsToTerms.csv"))
   #probabilities associated with each topic assignment
topicProbabilities <- as.data.frame(ldaOut@gamma)
topicProbabilities <- cbind(ldaOut@documents,topicProbabilities)
colnames(topicProbabilities) <- c("username","topic1","topic2","topic3")
write.csv(topicProbabilities,file=paste("E:/[Polimi Courses]/Thesis/R/instagram_user_cluster/MediaLDAGibbs",k,"TopicProbabilities.csv"))