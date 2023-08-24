library(tidyverse)
library(fmsb)
library(ggrepel)
library(patchwork)
library(hrbrthemes)

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
Positions_All <-  c("ALL", Positions)
Clubs <- c(unique(fpl_data$Club))
Costs <- c(seq(from = 4, to = 13.5, by = 0.5))
ShowDatas <- c("G & xG", "A &x A", "Gc & xGc") 

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
ui <- fluidPage(# theme = bslib::bs_theme(bootswatch = "darkly"),
  
  titlePanel("FPL_Analysis_2223"),
  
  tabsetPanel(
    tabPanel(
      "Compare Players by Value and Position",
      
      br(),
      br(),
      br(),
      
      # first tab
      sidebarLayout(sidebarPanel(
        sliderInput(
          "value_range",
          "Value Range",
          min = 3.5,
          max = 13.5,
          value = c(6, 10),
          step = 0.1
        ),
        
        br(),
        br(),
        br(),
        
        selectInput("dualY", "Show Data", choices = ShowDatas, selected = ShowDatas[0])
        
        
        
      ),
      mainPanel(plotOutput("scatter"), plotOutput("aaa")))
    ),
    
    # second tab
    tabPanel(
      "Compare Two Players in Same Positon",
      
      br(),
      br(),
      br(),
      
      sidebarLayout(sidebarPanel(tabsetPanel(
        tabPanel(
          "Player 1",
          br(),
          # player1
          selectInput("club1",
                      "Club",
                      choices = Clubs,
                      selected = Clubs[1]),
          
          selectInput(
            "position1",
            "Position",
            choices = Positions,
            selected = Positions[1]
          ),
          
          selectInput("player1",
                      "Name",
                      choices = NULL),
          
        ),
        
        tabPanel(
          "Player 2",
          br(),
          # player2
          selectInput("club2",
                      "Club",
                      choices = Clubs,
                      selected = Clubs[1]),
          
          selectInput("position2",
                      "Position",
                      choices = NULL),
          
          
          selectInput("player2",
                      "Name",
                      choices = NULL),
          
        )
        
      )),
      
      mainPanel(plotOutput("radar")),)
    ),
    
  ))


# Server logic ----
server <- function(input, output, session) {
  # tab 1 - scatterplot
  output$scatter <- renderPlot({
    plot_data <- fpl_data %>%
      filter(Points > 50) %>%
      filter(Cost >= input$value_range[1], Cost <= input$value_range[2])
    value_position <- input$value_position
    
    
    ggplot(plot_data, aes(x = Points, y = Cost)) +
      geom_point(aes(color = Position, shape = Position),
                 size = 1.5,
                 alpha = 0.8) +
      scale_color_manual(values = c("#386cb0", "#fdb462", "#7fc97f", "red")) +
      geom_text_repel(
        aes(label = Player),
        family = "Poppins",
        size = 3,
        min.segment.length = 0,
        seed = 42,
        box.padding = 0.5,
        max.overlaps = Inf,
        nudge_x = .15,
        nudge_y = .5,
        color = "grey50"
      ) +
      labs(
        title = "Points Positiopns",
        subtitle = "later",
        x = "total_points",
        y = "cost(m)"
      ) +
      theme(
        # The default font when not explicitly specified
        text = element_text(
          family = "Lobster Two",
          size = 8,
          color = "black"
        ),
        
        # Customize legend text, position, and background.
        legend.text = element_text(size = 9, family = "Roboto"),
        legend.position = c(1, 0),
        legend.justification = c(1, 0),
        legend.background = element_blank(),
        # This one removes the background behind each key in the legend
        legend.key = element_blank(),
        
        # Customize title and subtitle font/size/color
        plot.title = element_text(
          family = "Lobster Two",
          size = 20,
          face = "bold",
          color = "#2a475e"
        ),
        plot.subtitle = element_text(
          family = "Lobster Two",
          size = 15,
          face = "bold",
          color = "#1b2838"
        ),
        plot.title.position = "plot",
        
        # Adjust axis parameters such as size and color.
        axis.text = element_text(size = 10, color = "black"),
        axis.title = element_text(size = 12),
        axis.ticks = element_blank(),
        # Axis lines are now lighter than default
        axis.line = element_line(colour = "grey50"),
        
        # Only keep y-axis major grid lines, with a grey color and dashed type.
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "#b4aea9", linetype =
                                            "dashed"),
        
        # Use a light color for the background of the plot and the panel.
        panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
        plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4")
      )
  })
  
  # tab 1 - 
  output$aaa <- renderPlot({
    
    #Create Data
    x1 = rnorm(100)
    x2 = rnorm(100)+rep(2,100)
    par(mfrow=c(2,1))
    
    #Make the plot
    par(mar=c(0,5,3,3))
    hist(x1 , main="" , xlim=c(-2,5), ylab="Frequency for x1", xlab="", ylim=c(0,25) , xaxt="n", las=1 , col="slateblue1", breaks=10)
    par(mar=c(5,5,0,3))
    hist(x2 , main="" , xlim=c(-2,5), ylab="Frequency for x2", xlab="Value of my variable", ylim=c(25,0) , las=1 , col="tomato3"  , breaks=10)
    
    
    plot_data <- fpl_data %>%
      filter(Points > 50) %>%
      filter(Cost >= input$value_range[1], Cost <= input$value_range[2])
    value_position <- input$value_position
    
   
  })
 
  
  # tab 2 - radar
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
  
  # radar data
  radar_data_filter <- reactive({
    # organize data
    req(input$player1)
    req(input$player2)
    radarCols <- get_radarCols(input$position1)
    fpl_data %>% filter(Player %in% c(input$player1, input$player2)) %>% select(radarCols)
  })
  
  output$radar <- renderPlot({
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
