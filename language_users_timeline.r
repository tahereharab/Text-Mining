
options(header=FALSE, stringsAsFactors = FALSE, FileEncoding = "UTF-8")
dates <- table(as.Date(statuses$s_date))
date_names <- as.Date(statuses$s_date)


i <- 0

lan_data_frame <- data.frame(datename = character(),
                             it = integer(),
                             en = integer(),
                             others = integer(),
                             stringsAsFactors=FALSE)

while(i < length(dates)){
  i <- i+1
  date_users <- as.vector(statuses$s_username[as.Date(statuses$s_date) == as.Date(names(dates[i]))])
  lan_user <- users$u_language[users$u_username %in% date_users]
  
 lan_t <- table(lan_user)
 if(!is.na(lan_t["en-gb"])){
 lan_t["en"] <-lan_t["en"]+lan_t["en-gb"]
 }
 lan_t["others"] <- ((length(date_users))-(lan_t["en"]+lan_t["it"]))
 lan_data_frame <- rbind(lan_data_frame, list(names(dates[i]),lan_t["it"],lan_t["en"], lan_t["others"]))
}
colnames(lan_data_frame) <- c("date","it","en","others")
lan_data_frame[is.na(lan_data_frame)] <- 0
###lan_data_frame[48,4] <- 0

#chart
seqdate <- seq(as.Date("2016/06/10"), as.Date("2016/07/30"), by = "day")
date1 <- as.Date("2016-05-28")
date2 <- as.Date("2016-07-31")
lan_data_frame <- subset(lan_data_frame, as.Date(lan_data_frame$date) > date1 &
                          as.Date(lan_data_frame$date) < date2)
                          
df <- lan_data_frame
Italian <- c(df$it)
English <- c(df$en)
Others <- c(df$others)

g <- ggplot(df, aes(seqdate))
g <- g + geom_line(aes(y=Italian), colour="green" , size = 1.0)
g <- g + geom_line(aes(y=English), colour="navy" , size = 1.0)
g <- g + geom_line(aes(y=Others), colour="orange" , size = 1.0)
g <- g + ylab("Language Count") + xlab("Date")
g <- g + ggplot2::annotate("text", x = as.Date("2016-07-25"), y = 600, label = "- Italian    " , colour = "green" , size = 3)
g <- g + ggplot2::annotate("text", x = as.Date("2016-07-25"), y = 570, label = "- English  " , colour = "navy" , size = 3)
g <- g + ggplot2::annotate("text", x = as.Date("2016-07-25"), y = 540, label = "- Others " , colour = "orange" , size = 3)

g




