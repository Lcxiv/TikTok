library(readr)
library(ggplot2)
library(sqldf)
library(tidyverse)
library(wordcloud2) 
library(tidytext)
library(tm)
library(stringr)
library(webshot)
library("htmlwidgets")
library(plotly)
library(hrbrthemes)
library(viridis)
library(GGally)
require(stats)
#webshot::install_phantomjs()

#ALL OF THE WIDGETS AND GGSAVE HAVE BEEN COMMENTED SO THAT YOU WON'T HAVE TO GET MORE FOLDERS/IMAGES FOR NO REASONS
#IF YOU WANT TO TRY WITH A DIFFERENT DATASET, JUST RUN THE PYTHON SCRIPT(MAY TAKE SOME TIME), OTHER DATASETS WILL ALSO SHOW UP
#LASTLY, IF YOUR COMPUTER IS NOT GOOD ENOUGH, IT MAY CRASH THE SESSION BECAUSE OF THE INTERACTIVE PLOTS. IF THIS HAPPENS, JUST RUN CHUNKS OF CODES

###########################################################
#DATASETS


trending_vids <- read.csv("trending_vids.csv")
#View(trending_vids)

#DATASET1 with top 33 users

tiktok <- sqldf('SELECT user_name, n_shares, n_plays, n_likes, n_comments, video_length
      FROM trending_vids
      WHERE n_likes > 10000000
      GROUP BY user_name
      ORDER BY n_likes')

#DATASET2 with top 5000 users
tiktok2 <- sqldf('SELECT user_name, n_shares, n_plays, n_likes, n_comments, video_length
      FROM trending_vids
      GROUP BY user_name
      ORDER BY n_likes')
tiktok2$group <- c( rep('Q1', 1403), rep('Q2', 1403), rep('Q3', 1403), rep('Q4', 1403))

#Special DATASET for circle, but also a copy of TIKTOK with more variables
data <- tiktok

################################################################################

#Circle barplot

#How to make it


data <- tiktok
data$group <- c( rep('Q1', 8), rep('Q2', 8), rep('Q3', 8), rep('Q4', 9))
data$groupnum <- c( rep('0', 8), rep('25', 8), rep('50', 8), rep('75', 9))


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

#Actual plot

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
    axis.title = element_blank(),
    panel.grid = element_blank(),

    
     
  ) +
  ylim(-50,150) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=video_length+20, label=user_name, hjust=hjust), color="black", fontface="bold",alpha=0.6,size=4, angle= label_data$angle, vjust = 1) +
  
  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end+3, yend = -5), colour = "black", alpha=0.8 )  +
  geom_text(data=base_data, aes(x = title, y = -6, label=group), hjust=c(1,1,0,0), colour = "black", alpha=0.8, fontface="bold") +
  ggtitle("Top 33 users ordered by number of Likes")



#Plot and save 

p <- p + scale_fill_viridis_b(option= 'magma') 
p

#ggsave("Viz/CirclePlot.png", p, bg = "transparent")






######################################################################################################
#WORDCLOUD

#DATASET only for words

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

word_viz <- wordcloud2(word_frequ, color='random-light', backgroundColor="transparent")
word_viz

#Saving the wordcloud
#saveWidget(word_viz, file="Viz/word_viz.html", selfcontained = F,  background = "transparent")

#webshot("Viz/word_viz.html","Viz/word_viz.png", delay = 5)



###########################################################
#REGRESSION



reg<-lm(n_likes ~ n_plays, data = tiktok)
coeff=coefficients(reg)
# Equation of the line : 
eq = paste0("y = ", round(coeff[2],1), "*x + ", round(coeff[1],1))

s1 <- ggplot(data, aes(n_likes, n_plays, size = n_shares, fill = group)) + 
  geom_point(alpha=0.5, shape=21, color="black") +
  geom_abline(intercept = 8.209e+06, slope = 7.559e-0)+
  scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm"),
    
    
  ) +
  labs(title = "Relationship between Likes and Views", x = "Likes", y = "Views", 
       size = "Times Shared")+
  theme_ipsum() 

#Plotting normally and interactively
s1
ggplotly(s1)

#ggsave("Viz/scatterplot33.png", s1,  bg = "transparent")
#htmlwidgets::saveWidget(ggplotly(s1), "Viz/scatterplot33.html")



reg<-lm(n_likes ~ n_plays, data = tiktok)
coeff=coefficients(reg)

#GRAPH2

eq = paste0("y = ", round(coeff[2],1), "*x + ", round(coeff[1],1))

s2 <- ggplot(tiktok2, aes(n_likes, n_plays, size = n_shares, fill = user_name)) + 
  geom_point(alpha=0.5, shape=21, color="black") +
  geom_abline(intercept = 8.209e+06, slope = 7.559e-0)+
  scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm"),

    
  )+
  labs(title = "Relationship between Likes and Views", x = "Likes", y = "Views", 
       size = "Times Shared") +
  theme_ipsum() 
s2
ggplotly(p2)

#ggsave("Viz/scatterplot5000.png", s2, bg = "transparent")
#htmlwidgets::saveWidget(ggplotly(p2), "Viz/scatterplot5000.html")



################################################
#HEATMAP/CORR



corr <- ggcorr(tiktok2, method = c("everything", "pearson"), hjust = 0.75, label = T) +
  scale_fill_viridis(option = 'magma')

corr
ggsave("Viz/heatM.png", corr, bg = "transparent")

d1 <- ggplot(data, aes(x = n_likes, group = group, fill = group))+
  geom_density(alpha=.4) +
  theme_ipsum()+
  scale_fill_viridis(discrete=TRUE, option ="magma") +
  labs(title = "Distribution of Likes For Groups", x = "Likes", 
       legend = "Groups")
  
#Plots density with likes

d1 

#ggsave("Viz/likesDensity.png", d1,  bg = "transparent")


d2 <- ggplot(data, aes(x = n_shares, group = group, fill = group))+
  geom_density(alpha=.4) +
  theme_ipsum()  +
  scale_fill_viridis(discrete=TRUE, option ="magma")+
  labs(title = "Distribution of Times Shared For Groups", x = "Shared", 
       legend = "Groups")

#Plots density with shares

d2 

#ggsave("Viz/sharesDensity.png", d2,  bg = "transparent")
