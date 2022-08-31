

 library(fpc)
library(cluster)
 
#x <- 1:nrow(topicProbabilities)
#y <- sample(x, 1000)
topic.rand <- topicProbabilities[,2:11]

pamk.best <- pamk(topic.rand,krange = 3:100,usepam = FALSE,critout = TRUE)
#plot(clara(topic.rand, pamk.best$nc))
asw <- numeric(50)
for (k in 3:50)
     asw[[k]] <- clara(topic.rand, k) $ silinfo $ avg.width
 k.best <- which.max(asw)
 cat("silhouette-optimal number of clusters:", k.best, "\n")
 