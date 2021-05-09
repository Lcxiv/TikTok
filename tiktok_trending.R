library(readr)
library(ggplot2)
library(sqldf)
library(reshape2)
library(tidyverse)

trending_vids <- read_csv("Desktop/Projects/trending_vids.csv")
View(trending_vids)

sharing <- trending_vids$n_shares
views <- trending_vids$n_plays
likes <- trending_vids$n_likes
comments <- trending_vids$n_comments
length <- trending_vids$video_length
desc <- trending_vids$video_desc

tiktok <- sqldf('SELECT user_name, n_shares, n_plays, n_likes, n_comments, video_length
      FROM trending_vids
      WHERE n_likes > 10000000
      GROUP BY user_name
      ORDER BY n_likes')
tiktok <- drop_na(tiktok)


label_data <- tiktok

# calculate the ANGLE of the labels
number_of_bar <- nrow(label_data)
label_data$id <- seq(1, nrow(label_data))
angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
label_data$hjust<-ifelse( angle < -90, 1, 0)

# flip angle BY to make them readable
label_data$angle<-ifelse(angle < -90, angle+180, angle)
# ----- ------------------------------------------- ---- #


# Start the plot
p <- ggplot(tiktok, aes(x=as.factor(user_name), y= video_length)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  # This add the bars with a blue color
  geom_bar(stat="identity", fill=alpha("skyblue", 0.7)) +
  
  
  # Limits of the plot = very important. The negative value controls the size of the inner circle, the positive one is useful to add size over each bar
  ylim(-100,120) +
  
  # Custom the theme: no axis title and no cartesian grid
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm")      # Adjust the margin to make in sort labels are not truncated!
  ) +
  
  # This makes the coordinate polar instead of cartesian.
  coord_polar(start = 0) +
  
  # Add the labels, using the label_data dataframe that we have created before
  geom_text(data=label_data, aes(x=id, y=video_length, label=user_name, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) 

p


tiktok_words <- tiktok <- sqldf('SELECT user_name, video_desc, n_plays, n_likes, n_comments, video_length
      FROM trending_vids
      GROUP BY user_name
      ORDER BY n_likes')

library(wordcloud2) 
library(tidytext)
library(tm)
tiktok_words$video_desc <- sapply(tiktok_words$video_desc,function(row) iconv(row, "latin1", "ASCII", sub=""))
library(stringr)
tiktok_words$freq<-str_count(tiktok_words$video_desc,'\\w+')



word_frequ <- trending_vids$video_desc %>%
  na.omit() %>%
  tolower() %>%
  strsplit(split = " ") %>% # or strsplit(split = "\\W") 
  unlist() %>%
  table() %>%
  sort(decreasing = TRUE)

wordcloud2(word_frequ, color='random-light', backgroundColor="black")










