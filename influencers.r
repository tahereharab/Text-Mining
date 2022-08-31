norm_users <- users[users$u_follower_count > 0 ,]
norm_users <- norm_users[norm_users$u_following_count > 0 ,]
norm_users <- norm_users[norm_users$u_type %in% "Master" ,]

ffratio <- log10((norm_users$u_follower_count / norm_users$u_following_count) + 1)
ffratio <- as.vector(ffratio)
names(ffratio) <- norm_users$u_username
ffratio <- subset(ffratio, ffratio != Inf)
#ffratio <- sort(ffratio, decreasing = TRUE)
ffratio <- ffratio[order(names(ffratio))]
ffnames <- names(ffratio)



inf_count_c <- table(statuses$s_username[statuses$s_username %in% ffnames])
inf_stat <- statuses[statuses$s_username %in% ffnames,]
inf_stat_rt <- aggregate(inf_stat$s_retweet_count~inf_stat$s_username, inf_stat, sum)
inf_fv_c <- aggregate(inf_stat$s_favorite_count~inf_stat$s_username, inf_stat, sum)
inf_sum_c <- inf_fv_c$`inf_stat$s_favorite_count` + inf_stat_rt$`inf_stat$s_retweet_count`
names(inf_sum_c) <- inf_fv_c$`inf_stat$s_username`

#user status weight
usw_ratio <- log10((inf_sum_c / inf_count_c) + 1)
taraben_ratio <- usw_ratio * ffratio
taraben_ratio <- sort(taraben_ratio, decreasing = TRUE)


m <- barplot(taraben_ratio[1:10],axes = FALSE , ylim = range(0:10) , main = "Top 10 Influencers"
, ylab = "FFRatio" , col = 3, names.arg = "" )
axis(1, at = m , labels = names(taraben_ratio[1:10]) , las = 2, cex.axis = 0.7 )
axis(2, at = seq(0,10,by=0.5) , labels = seq(0,10,by=0.5),cex.axis = 0.7 )

