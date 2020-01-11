#app file
source("server_0_9.R")
library(shiny)
library(ggplot2)
ui <- fluidPage(
  titlePanel("Connect 4 Chess"),
  sidebarPanel(
    h3(textOutput("player_info_1"),
       style = "color:#1E9F94"),
    h3(textOutput("player_info_2"),
       style = "color:#E91773"),
    textOutput("add_info"),
    textOutput("win_info"),
    radioButtons("col_num", h3("Choose where to drop"),
                 choices = list("Column 1" = 1,
                                "Column 2" = 2,
                                "Column 3" = 3,
                                "Column 4" = 4,
                                "Column 5" = 5,
                                "Column 6" = 6,
                                "Column 7" = 7),
                 selected = 1),
    actionButton("submit", "Drop"),
    actionButton("restart", "Restart")
  ),
  mainPanel(
    fluidRow(
      column(width = 5, offset = 7,
        h3("A Ruoyan's Game", style = "color:#D68D36")
    )),
    plotOutput("main_map")
  )
)

server <- function(input, output){
  observeEvent(input$restart,{
    restart()
    output$main_map <- renderPlot({draw_map(current_map_temp = load_map())})
    output$player_info_1 <- renderText({paste("Player 1's Turn")})
    output$player_info_2 <- renderText({paste("------------------")})
    output$add_info <- renderText({paste("Please drop at the column you like.")})
  })

  observeEvent(input$submit,{
    add_dot(current_map_temp = load_map(), col_num_temp = as.numeric(input$col_num), color_temp = load_map()[12, 13])
    output$main_map <- renderPlot({draw_map(current_map_temp = load_map())})
    output$player_info_1 <- renderText({
      if(load_map()[12, 13] == -1){
        paste("Player 1's Turn")
      }else{
        paste("------------------")
      }
    })
    
    output$player_info_2 <- renderText({
      if(load_map()[12, 13] == -1){
        paste("------------------")
      }else{
        paste("Player 2's Turn")
      }
    })
    
    output$win_info <- renderText({
      if(load_map()[10,13] == 1){
        if(load_map()[12, 13] == -1){
          paste("Player 2 won the game !!")
        }else{
          paste("Player 1 won the game !!")
        }
      }else{
        paste("No one wins, game continumes.")
      }
    })
    
    output$add_info <- renderText({
      if(load_map()[11, 13] == 1){
        paste("Successfully Droped")
      }else{
        paste("This column is full. Please try another!")
      }
    })
  })
  
  output$main_map <- renderPlot({draw_map(current_map_temp = load_map())})
}
shinyApp(ui, server)