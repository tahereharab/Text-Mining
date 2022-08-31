library(vegan)

x <- 1:nrow(topicProbabilities)
y <- sample(x, 1000)
topic.rand <- topicProbabilities[y,2:11]

#require(vegan)
fit <- cascadeKM(scale(topic.rand, center = TRUE,  scale = TRUE), 3, 50, iter = 200)
plot(fit, sortg = TRUE, grpmts.plot = TRUE)
calinski.best <- as.numeric(which.max(fit$results[2,]))
cat("Calinski criterion optimal number of clusters:", calinski.best, "\n")
