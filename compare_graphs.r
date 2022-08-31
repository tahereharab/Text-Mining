#compare graphs

#https://stat.ethz.ch/pipermail/r-help/2007-February/126226.html

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
	
statuses_with_geo <- read_delim("statuses.csv",
";", escape_double = FALSE, col_types = cols(s_date = col_datetime(format = "%Y/%m/%d %H:%M"),
s_id = col_character(), s_lat = col_character(),
s_lng = col_character(), s_username = col_character()),
na = "empty", trim_ws = TRUE)

######media in instagram & twitter########	
insta <- table(as.Date(media$m_date))
seqdate <- seq(as.Date("2016/06/11"), as.Date("2016/07/30"), by = "day")
date1 <- as.Date("2016-06-10")
date2 <- as.Date("2016-07-31")
twitterdate <- subset(statuses, as.Date(statuses$s_date) > date1 &
                      as.Date(statuses$s_date) < date2)
twitter <- table(as.Date(twitterdate$s_date))

df <- data.frame(seqdate,insta,twitter)
vinsta <- c(insta)
vtwitter <- c(twitter)
g <- ggplot(df, aes(seqdate))
g <- g + geom_line(aes(y=vinsta), colour="purple" , size = 1.3)
g <- g + geom_line(aes(y=vtwitter), colour="blue" , size = 1.3)
g <- g + ylab("Status Count") + xlab("Date")
g <- g + annotate("text", x = as.Date("2016-07-06"), y = 4500, label = "instagram" , colour = "purple") 
g <- g + annotate("text", x = as.Date("2016-06-15"), y = 1500, label = "twitter" , colour = "blue") 
g

####### instagram like and comments count #######
seqdate <- seq(as.Date("2016/06/11"), as.Date("2016/07/30"), by = "day")
date_like <- aggregate(media$m_like~as.Date(media$m_date), media, sum)
date_comment <- aggregate(media$m_comment~as.Date(media$m_date), media, sum)
df <- data.frame(seqdate,date_like$`media$m_like`,date_comment$`media$m_comment`)
likesANDcomments <- c(date_like$`media$m_like`)
vcomment <- c(date_comment$`media$m_comment`)
Date <- seqdate
g <- ggplot(df, aes(Date))
g <- g + geom_line(aes(y=likesANDcomments), colour="red" , size = 1.3)
g <- g + geom_line(aes(y=vcomment), colour="green" , size = 1.3)
g <- g + annotate("text", x = as.Date("2016-07-06"), y = 200000, label = "Likes" , colour = "red") 
g <- g + annotate("text", x = as.Date("2016-07-01"), y = 12000, label = "Comments" , colour = "green") 
g


####### twitter favorite and retweet count #######
seqdate <- seq(as.Date("2016/06/10"), as.Date("2016/07/31"), by = "day")
date_favorite <- aggregate(statuses$s_favorite_count~as.Date(statuses$s_date), statuses, sum)
date_favorite <- subset(date_favorite, as.Date(date_favorite$`as.Date(statuses$s_date)`) > as.Date("2016-05-28"))
date_retweet <- aggregate(statuses$s_retweet_count~as.Date(statuses$s_date), statuses, sum)
date_retweet <- subset(date_retweet, as.Date(date_retweet$`as.Date(statuses$s_date)`) > as.Date("2016-05-28"))
df <- data.frame(seqdate,date_favorite$`statuses$s_favorite_count`, date_retweet$`statuses$s_retweet_count`)
FavoritesANDretweets <- c(date_favorite$`statuses$s_favorite_count`)
vretweets <- c(date_retweet$`statuses$s_retweet_count`)
Date <- seqdate
g <- ggplot(df, aes(Date))
g <- g + geom_line(aes(y=FavoritesANDretweets), colour="blue" , size = 1.3)
g <- g + geom_line(aes(y=vretweets), colour="orange" , size = 1.3)
g <- g + annotate("text", x = as.Date("2016-06-20"), y = 9000, label = "Favorites" , colour = "blue") 
g <- g + annotate("text", x = as.Date("2016-06-15"), y = 4000, label = "Retweets" , colour = "orange") 
g




























