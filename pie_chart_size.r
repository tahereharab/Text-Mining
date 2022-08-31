

# For each cluster 
c1user <- subset(orderedclusters,orderedclusters$grpuser.cluster.o. == 1)
c1 <- subset(users,users$u_username %in% c1user$topicProbabilities.username.o.)

c2user <- subset(orderedclusters,orderedclusters$grpuser.cluster.o. == 2)
c2 <- subset(users,users$u_username %in% c1user$topicProbabilities.username.o.)
 
c3user <- subset(orderedclusters,orderedclusters$grpuser.cluster.o. == 3)
c3 <- subset(users,users$u_username %in% c1user$topicProbabilities.username.o.)

c4user <- subset(orderedclusters,orderedclusters$grpuser.cluster.o. == 4)
c4 <- subset(users,users$u_username %in% c1user$topicProbabilities.username.o.)

c5user <- subset(orderedclusters,orderedclusters$grpuser.cluster.o. == 5)
c5 <- subset(users,users$u_username %in% c1user$topicProbabilities.username.o.)
 
c6user <- subset(orderedclusters,orderedclusters$grpuser.cluster.o. == 6)
c6 <- subset(users,users$u_username %in% c1user$topicProbabilities.username.o.)

# A vector with all cluster sizes
c_percent <- c(nrow(c1), nrow(c2),nrow(c3),nrow(c4),nrow(c5),nrow(c6))

#Draw pie chart
piepercent<- round(100*c_percent/sum(c_percent), 1)
pie(c_percent,main = "Clusters Size", col = rainbow(length(c_percent)),labels = paste(piepercent,"%",sep=""), radius = 1.1)