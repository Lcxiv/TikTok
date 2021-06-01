#Checking that all the necessary packages are installed
list.of.packages <- c("readr", "ggplot2", "sqldf", "tidyverse", "tidyverse", 
                      "wordcloud2", "tidytext","tm","stringr", "webshot", "sqldf",
                      "htmlwidgets","plotly", "hrbrthemes", "viridis", "GGally", "stats", "webshot")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages) 

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


#Circle Barplot

shiny_Circle_Bar <- function(data){
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
    ggtitle("Users ordered by number of Likes")
  return(p)
}

#Scatterplot of the data and regression line if available

shiny_reg <- function(data,X,y,size, fill){
  reg<-lm(X ~ y, data = data)
  coeff=coefficients(reg)
  # Equation of the line : 
  #eq = paste0("y = ", round(coeff[2],1), "*x + ", round(coeff[1],1))
  intercept <- coeff[1]
  slope <- coeff[2]
  reg_plot <- ggplot(data, aes(X, y, size = size, fill = fill)) + 
    geom_point(alpha=0.5, shape=21, color="black") +
    geom_abline(intercept = slope, slope = intercept) +
    #geom_smooth()+
    scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
    theme(
      legend.position = "none",
      legend.box = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.margin = unit(rep(-1,4), "cm"),
    ) +
    labs(title = "Relationship between Likes and Views", x = "Likes", y = "Views")+
    theme_ipsum() 
  interactive_plot <- ggplotly(reg_plot)
  return(interactive_plot)
}

#Wordcloud 

shiny_Wordcloud <- function(dataset, color, backgroundColor){
  dataset$video_desc <- sapply(dataset$video_desc,function(row) iconv(row, "latin1", "ASCII", sub=""))
  
  dataset$freq<-str_count(dataset$video_desc,'\\w+')
  
  
  
  word_frequ <- dataset$video_desc %>%
    na.omit() %>%
    tolower() %>%
    strsplit(split = " ") %>% 
    unlist() %>%
    table() %>%
    sort(decreasing = TRUE)
  
  return(wordcloud2(word_frequ, color=color, backgroundColor=backgroundColor))
}

shiny_corr <- function(data,X, group){
  corr <- ggcorr(data, method = c("everything", "pearson"), hjust = 0.75, label = T) +
    scale_fill_viridis(option = 'magma')

  corr_plot <- ggplot(data, aes(x = X, group = group, fill = group))+
    geom_density(alpha=.4) +
    theme_ipsum()+
    scale_fill_viridis(discrete=TRUE, option ="magma") +
    labs(title = "Distribution of Likes For Groups", x = "Likes", 
         legend = "Groups")
  interactive_plot <- ggplotly(corr_plot)
  return(interactive_plot)
}