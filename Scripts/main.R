setwd("/Users/Louis/Desktop/Projects/TikTok/Scripts")

source("functions.R")


trending_vids <- read.csv("Datasets/trending_vids.csv")
#View(trending_vids)

#DATASET1 with top 33 users
n <- 33
eq = paste0("SELECT user_name, n_shares, n_plays, n_likes, n_comments, video_length
      FROM trending_vids
      WHERE n_likes >", n, " GROUP BY user_name
      ORDER BY n_likes")

new_tik <- sqldf(eq)

tiktok <- sqldf('SELECT user_name, n_shares, n_plays, n_likes, n_comments, video_length
      FROM trending_vids
      WHERE n_likes > 10000000
      GROUP BY user_name
      ORDER BY n_likes')
tiktok$group <- c( rep('Q1', 8), rep('Q2', 8), rep('Q3', 8), rep('Q4', 9))

#DATASET2 with top 5000 users
tiktok2 <- sqldf('SELECT user_name, n_shares, n_plays, n_likes, n_comments, video_length
      FROM trending_vids
      GROUP BY user_name
      ORDER BY n_likes')
tiktok2$group <- c( rep('Q1', 1403), rep('Q2', 1403), rep('Q3', 1403), rep('Q4', 1403))

#Special DATASET for circle, but also a copy of TIKTOK with more variables
data <- tiktok
data$group <- c( rep('Q1', 8), rep('Q2', 8), rep('Q3', 8), rep('Q4', 9))
data$groupnum <- c( rep('0', 8), rep('25', 8), rep('50', 8), rep('75', 9))
################################################################################

#Circle barplot

#Shiny_Circle_Bar(tiktok)

#ggsave("Viz/CirclePlot.png", p, bg = "transparent")






######################################################################################################
#WORDCLOUD

#DATASET only for words

#shiny_Wordcloud(tiktok_words,color = 'random-light',backgroundColor = "black" )

#Saving the wordcloud
#saveWidget(word_viz, file="Viz/word_viz.html", selfcontained = F,  background = "transparent")

#webshot("Viz/word_viz.html","Viz/word_viz.png", delay = 5)



###########################################################
#REGRESSION


top33_reg <- shiny_reg(data, data$n_likes, data$n_plays, data$n_shares, data$user_name)
#top33_reg
#Plotting normally and interactively
top5000_reg <- shiny_reg(tiktok2, tiktok2$n_likes, tiktok2$n_plays, tiktok2$n_shares, tiktok2$user_name)
#top5000_reg

#ggsave("Viz/scatterplot33.png", s1,  bg = "transparent")
#htmlwidgets::saveWidget(ggplotly(s1), "Viz/scatterplot33.html")


#ggsave("Viz/scatterplot5000.png", s2, bg = "transparent")
#htmlwidgets::saveWidget(ggplotly(p2), "Viz/scatterplot5000.html")



################################################
#CORR/DENSITY

#ggcorr(tiktok)
#shiny_corr(data, data$n_likes, data$group)


