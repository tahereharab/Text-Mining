#Loading Libraries
library(tm)
library(topicmodels)
library(readr)
library(fpc)
library(mclust)
library(cluster)
library(NbClust)
library(vegan)

#Loading Users DataSet

users <- read_delim("E:/[Polimi Courses]/Thesis/R/instagram_user_cluster/users.csv", 
    ";", escape_double = FALSE, na = "null", 
    trim_ws = TRUE)
					 
#Extracting and cleaning the textual properties

text <-as.vector(gsub(","," ",users$u_media_hashtags))
username <- as.vector(users$u_username)
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


# creating document term matrix 
dtm <- DocumentTermMatrix(docs, control = list(wordLenghts=c(3,15),bounds = list(global = c(200,Inf)))) 
rownames(dtm) <- username
rowTotals <- apply(dtm , 1, sum)
dtm   <- dtm[rowTotals> 0, ]

#creating ordered word frequency csv file
freq <- colSums(as.matrix(dtm))
ord <- order(freq,decreasing=TRUE)
write.csv(freq[ord],"hashtag_word_freq.csv")
 

# Topic excraction
burnin <- 1000  
iter <- 1000     
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
verbose <- 1
best <- TRUE

#Number of topics
k <- 4

#Run LDA using Gibbs sampling
ldaOut <-LDA(dtm,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin, verbose=verbose))

#write out results
   #docs to topics
ldaOut.topics <- as.matrix(topics(ldaOut))
write.csv(ldaOut.topics,file=paste("htgLDAGibbs",k,"DocsToTopics.csv"))
   #top 10 terms in each topic
ldaOut.terms <- as.matrix(terms(ldaOut,10))
write.csv(ldaOut.terms,file=paste("htagLDAGibbs",k,"TopicsToTerms.csv"))
   #probabilities associated with each topic assignment
topicProbabilities <- as.data.frame(ldaOut@gamma)
topicProbabilities <- cbind(ldaOut@documents,topicProbabilities)
colnames(topicProbabilities) <- c("username","topic1","topic2","topic3","topic4")

write.csv(topicProbabilities,file=paste("htgLDAGibbs",k,"TopicProbabilities.csv"))

#K-means
set.seed(123456789)
grpuser <- kmeans(topicProbabilities[,2:5], centers=3, nstart=10)

plotcluster(topicProbabilities[,2:5], grpuser$cluster)
#clusplot(topicProbabilities[,-1], grpuser$cluster, main='2D representation of the Cluster solution', color=TRUE, shade=TRUE, labels=2, lines=0)

o=order(grpuser$cluster)
orderedclusters <- data.frame(topicProbabilities$username[o],grpuser$cluster[o])


# For each cluster 
c1user <- subset(orderedclusters,orderedclusters$grpuser.cluster.o. == 1)
c1 <- subset(users,users$u_username %in% c1user$topicProbabilities.username.o.)

c2user <- subset(orderedclusters,orderedclusters$grpuser.cluster.o. == 2)
c2 <- subset(users,users$u_username %in% c2user$topicProbabilities.username.o.)
 
c3user <- subset(orderedclusters,orderedclusters$grpuser.cluster.o. == 3)
c3 <- subset(users,users$u_username %in% c3user$topicProbabilities.username.o.)


# A vector with all cluster sizes
c_percent <- c(nrow(c1), nrow(c2),nrow(c3))

#Draw pie chart
piepercent<- round(100*c_percent/sum(c_percent), 1)
pie(c_percent,main = "Clusters Size", col = rainbow(length(c_percent)),labels = paste(piepercent,"%",sep=""), radius = 1.0)







 
 
 
