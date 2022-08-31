library(mclust)

#x <- 1:23626
#y <- sample(x, 1000)
topic.rand <- topicProbabilities[,2:6]

d_clust <- Mclust(as.matrix(topic.rand), G=3:30)
m.best <- dim(d_clust$z)[2]
cat("model-based optimal number of clusters:", m.best, "\n")
plot(d_clust)