#install.packages("ldatuning")
library("ldatuning")
library("topicmodels")

#x <- 1:nrow(topicProbabilities)
#y <- sample(x,1000)
#dtm.rand <- dtm[y,]

result <- FindTopicsNumber(
     dtm,
     topics = seq(from = 2, to = 30, by = 1),
     metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
     method = "Gibbs",
     control = list(seed = 77),
     mc.cores = 4L,
     verbose = TRUE
 )
FindTopicsNumber_plot(result)