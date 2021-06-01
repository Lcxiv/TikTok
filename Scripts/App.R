
source("main.R")




ui <- fluidPage(
  sliderInput("users", min = 1, max = 10000, value = 2, 
              label = "choose the number of users in the sampler" ),
  plotlyOutput("graph")
)

server <- function(input, output){

  #df$id <- seq(1, nrow(df))
  
  output$graph <- renderPlotly({
    slider <- input$users
    df = paste0("SELECT user_name, n_shares, n_plays, n_likes, n_comments, video_length, id AS COUNT(user_name)
      FROM trending_vids
      WHERE n_likes >", slider , " GROUP BY user_name
      ORDER BY n_likes")
    df <- sqldf(eq)
    #df <- reactive(df)
    shiny_reg(df, df$n_likes, df$n_plays, df$n_shares, df$user_name)
    
  })
} 
shinyApp(ui = ui, server = server)