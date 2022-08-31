


for (i in 1:length(grpuser$withinss)) {
#For each cluster, this defines the documents in that cluster
inGroup <- which(grpuser$cluster==i)
within <- dtm[inGroup,]
if(length(inGroup)==1) within <- t(as.matrix(within))
out <- dtm[-inGroup,]
words <- apply(within,2,mean) - apply(out,2,mean) #Take the difference in means for each term
print(c("Cluster", i), quote=F)
labels <- order(words, decreasing=T)[1:20] #Take the top 20 Labels
print(names(words)[labels], quote=F) #From here down just labels
if(i==length(grpuser$withinss)) {
print("Cluster Membership")
print(table(grpuser$cluster))
print("Within cluster sum of squares by cluster")
print(grpuser$withinss)
}
}