library(tidyverse)
library(fmsb)

# load data
path <- "./FPL_Dataset_2022-2023.csv"
fpl_raw_data <- read.csv(path) %>% as_tibble()

# transform data
fpl_data <- fpl_raw_data[c(
  "web_name",
  "team",
  "position",
  "total_points",
  "now_cost",
  "minutes",
  "goals_scored",
  "expected_goals",
  "assists",
  "expected_assists",
  "clean_sheets",
  "expected_goals_conceded"
)] %>%
  
  rename(all_of(
    c(
      player = "web_name",
      club = "team",
      points = "total_points",
      cost = "now_cost",
      goals = "goals_scored",
      xG = "expected_goals",
      xA = "expected_assists"
    )
    
  )) %>%
  
  mutate(cost = cost / 10)

# prepare data for select choices
positions <-  c("FWD", "MID", "DEF", "GKP")
clubs <- c(unique(fpl_data$club))
costs <- c(seq(from = 4, to = 13.5, by = 0.5))

# radar colnames
radarCols <- c("points",  "goals", "xG", "minutes", "assists", "xA")

# User interface ----
ui <- fluidPage(#theme = bslib::bs_theme(bootswatch = "darkly"),
  
  titlePanel("FPL_Analysis_2223"),
  
  sidebarLayout(
    sidebarPanel(
      # Radar chart to compare two players
      
      # player1
      selectInput(
        "club1",
        "Player 1 - Club",
        choices = clubs,
        selected = clubs[1]
      ),
      
      selectInput(
        "position1",
        "Player 1 - Position",
        choices = positions,
        selected = positions[1]
      ),
      
      selectInput("player1",
                  "Player 1 - Name",
                  choices = NULL),
      
      br(),
      br(),
      
      # player2
      selectInput(
        "club2",
        "Player 2 - Club",
        choices = clubs,
        selected = clubs[1]
      ),
      
      selectInput(
        "position2",
        "Player 2 - Position",
        choices = positions,
        selected = positions[1]
      ),
      
      selectInput("player2",
                  "Player 2 - Name",
                  choices = NULL),
      
    ),
    
    mainPanel(plotOutput("radarMap"))
    
  ))


# Server logic ----
server <- function(input, output, session) {
  # player 1 filter
  player1_filter <- reactive({
    fpl_data %>% filter(club == input$club1, position == input$position1) %>% arrange(desc(cost))
  })
  
  observeEvent(player1_filter(), {
    choices <- unique(player1_filter()$player)
    updateSelectInput(inputId = "player1", choices = choices)
  })
  
  # player 2 filter
  player2_filter <- reactive({
    fpl_data %>% filter(club == input$club2, position == input$position2) %>% arrange(desc(cost))
  })
  
  observeEvent(player2_filter(), {
    choices <- unique(player2_filter()$player)
    updateSelectInput(inputId = "player2", choices = choices)
  })
  
  # radar data
  radar_data_filter <- reactive({
    
    # organize data
    req(input$player1)
    req(input$player2)
    fpl_data %>% filter(player %in% c(input$player1, input$player2)) %>% select(radarCols)
  })
  
  output$radarMap <- renderPlot({
    
    plot_data <- radar_data_filter()
    plot_data <- rbind(c(250, 40, 40, 3420, 20, 20) , rep(0, 6) , plot_data)
    
    # color vector
    colors_border = c(rgb(0.2, 0.5, 0.5, 0.9),
                      rgb(0.8, 0.2, 0.5, 0.9) ,
                      rgb(0.7, 0.5, 0.1, 0.9))
    
    colors_in = c(rgb(0.2, 0.5, 0.5, 0.4),
                  rgb(0.8, 0.2, 0.5, 0.4) ,
                  rgb(0.7, 0.5, 0.1, 0.4))
    
    # plot with default options:
    radarchart(
      plot_data,
      axistype = 1 ,
      #custom polygon
      pcol = colors_border ,
      pfcol = colors_in ,
      plwd = 4 ,
      plty = 1,
      #custom the grid
      cglcol = "grey",
      cglty = 1,
      axislabcol = "grey",
      caxislabels = seq(0, 20, 5),
      cglwd = 0.8,
      #custom labels
      vlcex = 0.8
    )
    
    # add a legend
    legend(
      x = 0.7,
      y = 1,
      legend = rownames(plot_data[-c(1, 2),]),
      bty = "n",
      pch = 20 ,
      col = colors_in ,
      text.col = "grey",
      cex = 1.2,
      pt.cex = 3
    )
    
    radarchart( plot_data  , axistype=1 , 
                pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
                cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
                vlcex=0.8 
    )
    
  })
  
}

# Run app ----
shinyApp(ui, server)
