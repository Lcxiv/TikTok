library(readr)
library(ggplot2)
library(sqldf)
library(tidyverse)
library(wordcloud2) 
library(tidytext)
library(tm)
library(stringr)

setwd("C:/Users/Master/Desktop/TikTok")
trending_vids <- read.csv("trending_vids.csv")
#View(trending_vids)

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
#tiktok <- drop_na(tiktok)
################################################################################






data <- tiktok
data$group <- c( rep('Q1', 8), rep('Q2', 8), rep('Q3', 8), rep('Q4', 9))

empty_bar <- 10
to_add <- data.frame( matrix(NA, empty_bar, ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$group <- rep(levels(data$group), each=empty_bar)
data <- rbind(data, to_add)
data <- data %>% arrange(group)
data$id <- seq(1, nrow(data))

# Get the name and the y position of each label
label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- data %>% 
  group_by(group) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]


p <- ggplot(data, aes(x=as.factor(id), y=video_length)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  geom_bar(aes(x=as.factor(id), y=video_length, fill=n_shares), stat="identity", alpha=0.5) +

  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 60, xend = 32, yend = 60), colour = "grey", alpha=1, size=0.4 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 40, xend = 32, yend = 40), colour = "grey", alpha=1, size=0.4 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 20, xend = 32, yend = 20), colour = "grey", alpha=1, size=0.4 , inherit.aes = FALSE ) +
  ggplot2::annotate("text", x = rep(max(data$id),3), y = c(20, 40, 60), label = c("20", "40", "60") , color="grey", size=3 , angle=0, fontface="bold", hjust = -0.7 ) +
  
  # Add text showing the value of each 100/75/50/25 lines

  geom_bar(aes(x=as.factor(id), y=video_length, fill=n_shares), stat="identity", alpha=0.5) +
  theme_minimal() +
  theme(

    axis.text = element_blank(),
    panel.grid = element_blank(),

    
     
  ) +
  ylim(-50,150) +
  xlab("Top Trending Users") +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=video_length+20, label=user_name, hjust=hjust), color="black", fontface="bold",alpha=0.6,size=4, angle= label_data$angle, vjust = 1) +
  
  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end+3, yend = -5), colour = "black", alpha=0.8 )  +
  geom_text(data=base_data, aes(x = title, y = -6, label=group), hjust=c(1,1,0,0), colour = "black", alpha=0.8, fontface="bold")

p <- p + scale_fill_viridis_b(option= 'magma') 
p

ggsave("CirclePlot.png", p, height = 12 , width = 8)












######################################################################################################
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

empty_bar <- 3
base_data <- tiktok %>% 
  group_by(group) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

# Start the plot
p <- ggplot(tiktok, aes(x=as.factor(user_name), y= video_length)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  # This add the bars with a blue color
  geom_bar(stat="identity", fill=tiktok$n_shares) +
  
  
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
  #annotate("text", x = rep(max(tiktok$video_length),4), y = tiktok$video_length, label = c("30s", "60s", "120s", "180s") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  
  
  # This makes the coordinate polar instead of cartesian.
  coord_polar(start = 0) +
  
  # Add the labels, using the label_data dataframe that we have created before
  geom_text(data=label_data, aes(x=id, y=video_length, label=user_name, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE )


p



tiktok_words <- tiktok <- sqldf('SELECT user_name, video_desc, n_plays, n_likes, n_comments, video_length
      FROM trending_vids
      GROUP BY user_name
      ORDER BY n_likes')


tiktok_words$video_desc <- sapply(tiktok_words$video_desc,function(row) iconv(row, "latin1", "ASCII", sub=""))

tiktok_words$freq<-str_count(tiktok_words$video_desc,'\\w+')



word_frequ <- trending_vids$video_desc %>%
  na.omit() %>%
  tolower() %>%
  strsplit(split = " ") %>% 
  unlist() %>%
  table() %>%
  sort(decreasing = TRUE)

wordcloud2(word_frequ, color='random-light', backgroundColor="black")





install.packages('readr','sqldf','tidyverse','tidytext','tm','stringr')




