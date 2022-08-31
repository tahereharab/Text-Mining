#Elbow Methode



 topic.rand <- topicProbabilities[,2:5]
 wss <- (nrow(topic.rand-1)*sum(apply(topic.rand,2,var)))
  for (i in 1:50) wss[i] <- sum(kmeans(topic.rand,centers=i,iter.max = 20)$withinss)
  plot(1:50, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
  dev.copy(png,filename="Elbow.jpg");
  dev.off ();