# 1. Radar Chart: FWD and MID compare G and A; DEF and GKP compare GC and

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
  "points_per_game",
  "now_cost",
  "minutes",
  "goals_scored",
  "expected_goals",
  "expected_goals_per_90",
  "assists",
  "expected_assists",
  "expected_assists_per_90",
  "goals_conceded",
  "expected_goals_conceded",
  "expected_goals_conceded_per_90",
  "saves",
  "saves_per_90",
  "clean_sheets",
  "clean_sheets_per_90"
)] %>%
  
  rename(all_of(
    c(
      Player = "web_name",
      Club = "team",
      Position = "position",
      Points = "total_points",
      Points90 = "points_per_game",
      Cost = "now_cost",
      Minutes = "minutes",
      Goals = "goals_scored",
      xG = "expected_goals",
      xG90 = "expected_goals_per_90",
      Assists = "assists",
      xA = "expected_assists",
      xA90 = "expected_assists_per_90",
      GC = "goals_conceded",
      xGc = "expected_goals_conceded",
      xGc90 = "expected_goals_conceded_per_90",
      Saves = "saves",
      Saves90 = "saves_per_90",
      CS = "clean_sheets",
      CS90 = "clean_sheets_per_90"
    )
    
  )) %>%
  
  mutate(Cost = Cost / 10)

# prepare data for select choices
Positions <-  c("FWD", "MID", "DEF", "GKP")
Clubs <- c(unique(fpl_data$Club))
Costs <- c(seq(from = 4, to = 13.5, by = 0.5))

# different position compare different data in radar chart
get_radarCols <- function(Position) {
  if (Position == 'FWD' | Position == 'MID') {
    return (c(
      "Points",
      "Assists",
      "xA",
      "xA90",
      "Minutes",
      "xG90",
      "xG",
      "Goals"
    ))
  } else if (Position == 'DEF') {
    return (c(
      "Points",
      "GC",
      "xGc",
      "xGc90",
      "Minutes",
      "CS90",
      "CS",
      "Points90"
    ))
  } else{
    return (c(
      "Points",
      "GC",
      "xGc",
      "xGc90",
      "Minutes",
      "Saves90",
      "Saves",
      "Points90"
    ))
  }
}

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
        choices = Clubs,
        selected = Clubs[1]
      ),
      
      selectInput(
        "position1",
        "Player 1 - Position",
        choices = Positions,
        selected = Positions[1]
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
        choices = Clubs,
        selected = Clubs[1]
      ),
      
      selectInput("position2",
                  "Player 2 - Position",
                  choices = NULL),
      
      
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
    fpl_data %>% filter(Club == input$club1, Position == input$position1) %>% arrange(desc(Cost))
  })
  
  observeEvent(player1_filter(), {
    choices <- unique(player1_filter()$Player)
    updateSelectInput(inputId = "player1", choices = choices)
  })
  
  # player 2 filter
  player2_filter <- reactive({
    fpl_data %>% filter(Club == input$club2,
                        Position == input$position2,
                        Player != input$player1) %>% arrange(desc(Cost))
  })
  
  observeEvent(player2_filter(), {
    choices <- unique(player2_filter()$Player)
    updateSelectInput(inputId = "player2", choices = choices)
  })
  
  # player 2 position depends on player 1
  observeEvent(input$position1, {
    updateSelectInput(inputId = "position2", choices = c(input$position1))
  })
  
  # # player 2 name cannot be same with player 1
  # player2_name_filter <- reactive({
  #   fpl_data %>% filter(Player != input$player1) %>% arrange(desc(Cost))
  # })
  #
  # observeEvent(player2_name_filter(), {
  #   choices <- unique(player2_name_filter()$Player)
  #   updateSelectInput(inputId = "player2", choices = choices)
  # })
  
  # radar data
  radar_data_filter <- reactive({
    # organize data
    req(input$player1)
    req(input$player2)
    radarCols <- get_radarCols(input$position1)
    fpl_data %>% filter(Player %in% c(input$player1, input$player2)) %>% select(radarCols)
  })
  
  output$radarMap <- renderPlot({
    radarCols <- get_radarCols(input$position1)
    
    plot_data <- radar_data_filter()
    plot_data <-
      rbind(c(
        max(fpl_data[radarCols[1]]),
        max(fpl_data[radarCols[2]]),
        max(fpl_data[radarCols[3]]),
        max(fpl_data[radarCols[4]]),
        max(fpl_data[radarCols[5]]),
        max(fpl_data[radarCols[6]]),
        max(fpl_data[radarCols[7]]),
        max(fpl_data[radarCols[8]])
      ),
      rep(0, 8),
      plot_data)
    
    # color vector
    colors_border = c(rgb(0.2, 0.5, 0.5, 0.9),
                      rgb(0.8, 0.2, 0.5, 0.9),
                      rgb(0.7, 0.5, 0.1, 0.9))
    
    colors_in = c(rgb(0.2, 0.5, 0.5, 0.4),
                  rgb(0.8, 0.2, 0.5, 0.4),
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
      x = 1.2,
      y = 1,
      legend = c(input$player1, input$player2),
      bty = "n",
      pch = 20 ,
      col = colors_in ,
      text.col = "grey",
      cex = 1.2,
      pt.cex = 3
    )
    
  })
  
}

# Run app ----
shinyApp(ui, server)
