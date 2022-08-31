library(tm)
library(topicmodels)
library(readr)
library(fpc)
library(mclust)
library(cluster)
library(NbClust)
library(vegan)

library(readr)
media <- read_delim("E:/[Polimi Courses]/Thesis/R/instagram_user_cluster/media.csv", 
    ";", escape_double = FALSE, col_types = cols(m_date = col_datetime(format = "%Y/%m/%d %H:%M")), 
    na = "null", trim_ws = TRUE)


# Media Count by Date	
tdate <- table(as.Date(media$m_date))
seqdate <- seq(as.Date("2016/06/10"), as.Date("2016/08/01"), by = "day")
plot(tdate, axes = FALSE, type = "l", col = "blue", ylim = range(0:5000),ylab = "media count", main = "Media Count by Date")
axis(1, at=seq(1,53,by=1) , labels= seqdate , las = 2)
axis(2, at = seq(0,5000,by=200) , labels = seq(0,5000,by=200))

#word cloud for text
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(tm)

m_data <- strsplit(as.vector(media$m_caption_norm),",")
myCorpus <- Corpus(VectorSource(m_data))
myCorpus <- tm_map(myCorpus , PlainTextDocument)
myCorpus <-tm_map(myCorpus,content_transformer(tolower))
myStopWords <- c("com","instagram","peopl","dai","todai")
myCorpus <- tm_map(myCorpus, removeWords, myStopWords)
pal2 <- brewer.pal(8,"Dark2")
wordcloud(myCorpus, min.freq = 50, max.words = Inf, random.order = FALSE , colors = pal2)

#word cloud for hashtag

m_data <- strsplit(as.vector(media$m_hashtags),",")
myCorpus <- Corpus(VectorSource(m_data))
myCorpus <- tm_map(myCorpus , PlainTextDocument)
myCorpus <-tm_map(myCorpus,content_transformer(tolower))
pal2 <- brewer.pal(8,"Dark2")
#windows()
wordcloud(myCorpus, min.freq = 50, max.words = Inf, random.order = FALSE , colors = pal2)
#dev.off


#active users
users <- media[media$u_user != "",]
u_user <- sort(table(users$u_user) , decreasing = TRUE)
users_names <- names(u_user)
names(u_user) <- ""
m <- barplot(u_user[1:30] , axes = FALSE , main = "Active Users" , ylab = "Media Count" , col = 4)
axis(1, at = m , labels = users_names[1:30] , las = 2, cex.axis = 0.7)
axis(2, at = seq(0,200,by=10) , labels = seq(0,200,by=10),cex.axis = 0.7 )


#active contributers
users <- read_delim("E:/[Polimi Courses]/Thesis/R/instagram_user_cluster/users.csv", 
    ";", escape_double = FALSE, na = "null", 
    trim_ws = TRUE)

con_names <- paste(media$m_likers, media$m_commenters, sep = ',')
con_names <- gsub("\\?","",con_names)
con_names <- con_names[con_names != ""]
con_names <- as.vector(strsplit(con_names,","))
con_names <- unlist(con_names)
con_names <- con_names[con_names %in% users$u_username]
con_count <- table(con_names)
con_count <- sort(con_count , decreasing = TRUE)
mp <- barplot(con_count[1:10],col = 13 , ylim = range(0:3000),  main = "Instagram Active Contributers" , ylab = "statuses Count", axes = FALSE, beside = TRUE, names.arg = "")
> text(mp, con_count[1:10], labels = con_count[1:10],pos = 3, cex = .60)
> axis(1,at = mp,labels = names(con_count[1:10]), las = 2 , cex.axis = 0.6)
> axis(2,at = seq(0,3000,by = 200),labels = seq(0,3000,by = 200) , cex.axis = 0.6)
	
	

#like count by Date
date_like <- aggregate(media$m_like~as.Date(media$m_date), media, sum)
seqdate <- seq(as.Date("2016/06/11"), as.Date("2016/07/30"), by = "day")
m <- plot(date_like[2:nrow(date_like),], axes = FALSE, type = "l", col = "blue", ylim = range(0:250000), xlab = "",ylab = "likes count" , main = " like Count During the exibithion")
axis(1, date_like$`as.Date(media$m_date)` , labels= substr(seqdate,6,10) , las = 2)
axis(2, at = seq(0,250000,by=5000) , labels = seq(0,250000,by=5000))


#comment count by date
date_comment <- aggregate(media$m_comment~as.Date(media$m_date), media, sum)
seqdate <- seq(as.Date("2016/06/11"), as.Date("2016/07/30"), by = "day")
m <- plot(date_comment[2:nrow(date_comment),], axes = FALSE, type = "l", col = "blue", ylim = range(0:7000),
xlab = " " , ylab = "comments count", main = " Comments Count During the exibithion")
axis(1, date_comment$`as.Date(media$m_date)` , labels= substr(seqdate,6,10) , las = 2)
axis(2, at = seq(0,7000,by=100) , labels = seq(0,7000,by=100))

#world simple geo Map
library(rworldmap)
media <- media[media$m_geo != '?',]
newmap <- getMap(resolution = "low")
plot(newmap, asp = 1)
lat <- c(substr(media$m_geo, 1 , (regexpr(",", media$m_geo) - 1)))
long <- c(substr(media$m_geo , (regexpr(",", media$m_geo) + 1) , 30))
points(long, lat, col = "red", cex = .6)

#world ggplot  
library(data.table)
library(ggplot2)
library(maps)
media <- media[media$m_geo != '?',]
mdat <- map_data('world')
str(mdat)
Lat <- c(as.numeric(substr(media$m_geo, 1 , (regexpr(",", media$m_geo) - 1)))) 
Lon <- c(as.numeric(substr(media$m_geo , (regexpr(",", media$m_geo) + 1) , 30)))
data <- as.data.table(Lon,Lat)  


#ggplot() + 
#     geom_polygon(dat=mdat, aes(long, lat, group=group), fill="grey50") +
#     geom_point(data=data, 
#                aes(x=Lon, y=Lat), col="red")		 

#colorful
g1 <- ggplot() + 
    geom_polygon(dat=mdat, aes(long, lat, group=group , fill = region)) +
    theme(legend.position="none") + geom_point(data=data, 
               aes(x=Lon, y=Lat), col="black")


			   
#density map
library(ggmap)	
library(data.table)	
media <- media[media$m_geo != '?',]	   
geofreq <- table(media$m_geo)			   
Loc <- strsplit(names(geofreq) , split = ',')  
   n <- 0
 Lon <- 0
 Lat <- 0
 while(n < length(Loc)){
     n <- n+1
     Lon[n] <- as.numeric(Loc[[n]][1])
     Lat[n] <- as.numeric(Loc[[n]][2])
 }
 
Density <- as.numeric(geofreq)
point_loc <- Lat
point_loc <- cbind(point_loc,Lon)
point_loc <- cbind(point_loc,Density)
colnames(point_loc) <- c("Lat", "Lon", "Den")
point_loc <- as.data.frame(point_loc)
Density <- point_loc$Den 

newmap <- get_map(location = "italy", zoom = 6)
map <- ggmap(newmap)
g2 <- map + geom_point(data=point_loc, aes(x=point_loc$Lat, y=point_loc$Lon , size = Density), col="red")
  
  
  
##grid map############
require(cowplot)
theme_set(theme_cowplot(font_size=12))
 
newmap3 <- get_map(location = "lombardy italy", zoom = 9)
map3 <- ggmap(newmap3)
g3 <- map3 + geom_point(data=point_loc, aes(x=point_loc$Lat, y=point_loc$Lon , size = Density), col="red")  

newmap4 <- get_map(location = "sulzano italy", zoom = 12)
map4 <- ggmap(newmap4)
g4 <- map4 + geom_point(data=point_loc, aes(x=point_loc$Lat, y=point_loc$Lon , size = Density), col="red")  

plot_grid(g1 , g2, g3 , g4 , ncol = 2 , labels = c('World', 'Italy' , 'Lomabrdy' , 'Sulzano')) 
#####################################











  