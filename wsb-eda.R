require(ggplot2)
require(dplyr)

##################
### DATAFRAMES ###
##################

wsb <- read.csv("r_wallstreetbets_posts.csv")

# remove any data from before 2020
wsb_adj <- wsb[which(wsb$created_utc >= 1577836800),]

# convert UTC to readable time
wsb_adj$date_posted <- as.POSIXct(wsb_adj$created_utc, tz="UTC", origin="1970-01-01")

# separate day posted from time posted
wsb_adj$day <- substr(wsb_adj$date_posted, start=1, stop=10)
wsb_adj$time <- substr(wsb_adj$date_posted, start=12, stop=19)
wsb_adj$time_hour <- as.numeric(substr(wsb_adj$time, start=1, stop=2))

# create df of number of posts per day
daily_posts <- table(wsb_adj$day)
daily_posts_df <- data.frame(daily_posts)
names(daily_posts_df) <- c("day", "numberOfPosts")
daily_posts_df$day <- as.Date(daily_posts_df$day)

# create df of times of day posts are posted
post_times <- data.frame(table(wsb_adj$time_hour))
names(post_times) <- c("hour", "totalPosts")
for (i in 1:24) {
        post_times$avg_score[i] = mean(na.omit(wsb_adj[wsb_adj$time_hour==i-1, "score"]))
}

# Dataframe for pre-GME
wsb_pre_GME <- wsb_adj[which(wsb_adj$created_utc < 1610496000),]

# Dataframe for since GME (since the 50% stock surge on 1/13/21)
wsb_since_GME <- wsb_adj[which(wsb_adj$created_utc >= 1610496000),]

###############
### VISUALS ###
###############

options(scipen=999) # turn off scientific notation

# score vs. time scatterplot
ggplot(wsb_adj, aes(x=time_hour, y=score)) +
        geom_point(color="coral1") +
        labs(title="Post Score vs. Hour Posted", y="Score",
             x="Hour") +
        scale_x_continuous(breaks=0:23) +
        scale_y_continuous(breaks=c(0,25000,50000,75000,100000,125000)) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5))

# average scores of each hour
ggplot(data=post_times, aes(x=hour, y=avg_score, group=1)) +
        geom_line(color="coral1") +
        geom_point(color="coral1") +
        labs(title="Average scores of posts based on the time of day they were posted",
             x="Hour of the day (GMT)", y="Average score") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5))

# time series of number of posts between full time frame
ggplot(daily_posts_df, aes(x=day, y=numberOfPosts)) +
        geom_line(color="coral1") + 
        xlab("") +
        scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
        labs(title="Number of Daily Posts on r/wallstreetbets Over Time - Jan. 2020 to Feb. 2021",
             y="Daily Posts") +
        theme_minimal() +
        theme(axis.text.x=element_text(angle=60, hjust=1),
              plot.title = element_text(hjust = 0.5))

# time series for pre-GME
ggplot(daily_posts_df, aes(x=day, y=numberOfPosts)) +
        geom_line(color="coral1") + 
        xlab("") +
        scale_x_date(date_labels = "%b %Y", date_breaks = "1 month",
                     limit=c(as.Date("2020-01-01"),as.Date("2021-01-12"))) +
        ylim(0,3500) +
        labs(title="Number of Daily Posts on r/wallstreetbets Over Time - Pre-GME",
             y="Daily Posts") +
        theme_minimal() +
        theme(axis.text.x=element_text(angle=60, hjust=1),
              plot.title = element_text(hjust = 0.5))

# time series for since-GME
ggplot(daily_posts_df, aes(x=day, y=numberOfPosts)) +
        geom_line(color="coral1") + 
        xlab("") +
        scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 week",
                     limit=c(as.Date("2021-01-13"),as.Date("2021-02-16"))) +
        labs(title="Number of Daily Posts on r/wallstreetbets Over Time - Since GME",
             y="Daily Posts") +
        theme_minimal() +
        theme(axis.text.x=element_text(angle=60, hjust=1),
              plot.title = element_text(hjust = 0.5)) 

###################
### OTHER STATS ###
###################

# Day with most posts
(max_posts <- which.max(daily_posts)) # -> 2021-01-28, 120969 posts
# Create data frame of 1/28/21
wsb_max_posts <- wsb_adj[which(wsb_adj$day == "2021-01-28"),]

mean(wsb_pre_GME$score)
max(wsb_pre_GME$score) # most upvotes pre-GME -> 55907
max(daily_posts_df[daily_posts_df$day<"2021-01-13","numberOfPosts"])
mean(daily_posts_df[daily_posts_df$day<"2021-01-13","numberOfPosts"])

mean(wsb_since_GME$score)
max(wsb_since_GME$score) # most upvotes during GME -> 134840
max(daily_posts_df[daily_posts_df$day>="2021-01-13","numberOfPosts"])
mean(daily_posts_df[daily_posts_df$day>="2021-01-13","numberOfPosts"])
wsb_adj[which(wsb_adj$score == 134840),] # row 80425
wsb_adj[80425, "time_hour"] # 19


