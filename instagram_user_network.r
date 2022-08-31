library(readr)
library(igraph)

options(header=FALSE, stringsAsFactors = FALSE, FileEncoding = "UTF-8")
media <- read_delim("E:/[Polimi Courses]/Thesis/R/instagram_user_cluster/media.csv", 
    ";", escape_double = FALSE, col_types = cols(m_date = col_datetime(format = "%Y/%m/%d %H:%M")), 
    na = "null", trim_ws = TRUE)
	
	
contributers <- paste(media[1:100,]$m_user , media[1:100,]$m_likers, media[1:100,]$m_commenters, sep = ',')
contributers <- gsub("\\?","",contributers)
contributers <- contributers[contributers != ""]
contributers <- as.vector(strsplit(contributers,","))

instaNodes <- data.frame(nodes=character(),stringsAsFactors=FALSE)	
instaLinks <- data.frame(from=character() , to= character() , stringsAsFactors=FALSE)


n <- 0
while(n < length(contributers)){
    n <- n+1
    i <- 0
    while(i < length(contributers[[n]]))
    {
        i <- i+1
		contributer <- contributers[[n]][i]
		instaNodes <- rbind(instaNodes, contributer)
        if(i >  1){
		instaLinks <- rbind(instaLinks , list(contributers[[n]][1] , contributer))
        }
    }
    
}  

instaNodes <- unique(instaNodes)
colnames(instaLinks) <- c("from" , "to")
colnames(instaNodes) <- c("username")
 
plot(net , axes = FALSE, add = FALSE, xlim = c(-1, 1),ylim = c(-1, 1), vertex.color="blue", vertex.label = "*", edge.arrow.size=0.1, vertex.size=1)
plot(net)