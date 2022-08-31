
library(cluster)
library(tm)

users <- read_delim("C:/data/users/users.csv", 
    ";", escape_double = FALSE, na = "NA", 
    trim_ws = TRUE)
    
cuser <- subset(orderedclusters,orderedclusters$grpuser.cluster.o. == 1)
c <- subset(users,users$u_username %in% cuser$topicProbabilities.username.o.)
text <-as.vector(gsub(","," ",c$u_bio_norm))
username <- as.vector(c$u_username)
docs <- Corpus(VectorSource(text))
docs <-tm_map(docs,content_transformer(tolower))
docs <- tm_map(docs, stripWhitespace)
myStopWords <- c("com", "new", "dai", "year", "che", "time", "thank", "just", "non","todai","del","sai","una","don","http","della","dell","sono"
, "love" , "good", "like", "statu", "happi", "world", "peopl", "come", "want", "make", "great", "look", "work", "follow", "merri", "life", "live",
"best", "beauti", "know", "wish", "thing", "need", "think", "night", "week", "let", "open","wai", "check", "help", "start", "natal", "post", "right", "person",
 "citi", "share" , "chang", "tonight", "latest", "final", "littl", "miss", "tutti", "stop", "dei", "realli", "die", "better", "big", "doe", "readi" ,
"tomorrow", "plai", "wait", "word", "real", "got", "sold", "speak", "begin" )
docs <- tm_map(docs, removeWords, myStopWords)

dtm <- DocumentTermMatrix(docs, control = list(wordLenghts=c(3,15),bounds = list(global = c(50,Inf))))
rownames(dtm) <- username
rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm   <- dtm[rowTotals> 0, ] 
freq <- colSums(as.matrix(dtm))
ord <- order(freq,decreasing=TRUE)
write.csv(freq[ord],"bio_cluster_3_word_freq.csv",)
######word cloud#######
#pal2 <- brewer.pal(8,"Dark2")
#wordcloud(documents, min.frequency = 200, max.words = Inf, random.order = FALSE , colors = pal2)
#######################