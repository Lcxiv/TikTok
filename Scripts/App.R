
source("main.R")




ui <- fluidPage(
  sliderInput("users", min = 0, max = 5000, value = 500, dragRange = T,
              label = "Choose the number of users in the sample"),
  plotlyOutput("graph")
)

server <- function(input, output){

  #df$id <- seq(1, nrow(df))
  
  output$graph <- renderPlotly({
    slider <- input$users
    df <- sqldf(paste0("SELECT user_name, n_shares, n_plays, n_likes, n_comments, video_length, id 
    FROM tiktok2
    GROUP BY n_likes
    HAVING id <= ", slider)
    )
    #df <- subset(trending_vids, slider)
    #df <- reactive({df_copy})


    #df <- reactive({df})
    #top33_reg
    shiny_reg(df, df$n_likes, df$n_plays, df$n_shares, df$user_name)
    })
} 
shinyApp(ui = ui, server = server)