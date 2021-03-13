wsb <- read.csv("r_wallstreetbets_posts.csv")

# remove any data from before 2020
wsb_adj <- wsb[which(wsb$created_utc >= 1577836800),]

# convert UTC to readable time
wsb_adj$date_posted <- as.POSIXct(wsb_adj$created_utc, tz="UTC", origin="1970-01-01")
wsb_adj$day <- substr(wsb_adj$date_posted, start=1, stop=10)

(daily_posts <- table(wsb_adj$day))

# Entire timeline from 1/1/20 to 2/16/21
barplot(daily_posts, main="Timeline of r/wallstreetbets posts", ylab="Number of Posts", 
        cex.names=0.6, cex.axis=0.6, las=2,
        col="orange", border="orange", space=0)

# Day with most posts
(max_posts <- which.max(daily_posts)) # -> 2021-01-28, 120969 posts
# Create data frame of 1/28/21
wsb_max_posts <- wsb_adj[which(wsb_adj$day == "2021-01-28"),]

# Dataframe for pre-GME
wsb_pre_GME <- wsb_adj[which(wsb_adj$created_utc < 1610496000),]
barplot(table(wsb_pre_GME$day), main="Timeline of r/wallstreetbets posts pre-GME", ylab="Number of Posts", 
        cex.names=0.6, cex.axis=0.6, las=2,
        col="orange", border="orange", space=0)
# Dataframe for during GME
wsb_during_GME <- wsb_adj[which(wsb_adj$created_utc >= 1610496000 & wsb_adj$created_utc <= 1612396799),]
barplot(table(wsb_during_GME$day), main="Timeline of r/wallstreetbets posts during GME", ylab="Number of Posts", 
        cex.names=0.6, cex.axis=0.6, las=2,
        col="orange", border="orange", space=0)
# Dataframe for post-GME
wsb_post_GME <- wsb_adj[which(wsb_adj$created_utc > 1612396799),]
barplot(table(wsb_post_GME$day), main="Timeline of r/wallstreetbets posts post-GME", ylab="Number of Posts", 
        cex.names=0.6, cex.axis=0.6, las=2,
        col="orange", border="orange", space=0)
