library(tidyverse)
library(shiny)
library(htmltools)
library(fmsb)
library(RColorBrewer)
library(hrbrthemes)
library(viridis)
library(ggrepel)
library(gridExtra)

# load data
path <- "./FPL_Dataset_2022-2023.csv"
fpl_raw_data <- read.csv(path)

# get level function
get_cost_level <- function(cost_bin) {
  level <- levels(cut_interval((fpl_raw_data$now_cost) / 10, 3))
  case_when(
    cost_bin == level[1] ~ "Low",
    cost_bin == level[2] ~ "Middle",
    cost_bin == level[3] ~ "Premium",
    .default = NA
  )
}

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
  # cost is 10 times larger because it is easier to store a integer in the database
  # now we can return it to the actual value by diving 10
  mutate(Cost = Cost / 10) %>%
  # separate the player into three group by their values
  mutate(Cost_bin = cut_interval(Cost, 3), .after = Cost) %>%
  # category player level by their values
  mutate(Level = get_cost_level(Cost_bin), .after = Cost_bin) %>%
  # generate per 90 stats and diff stats
  mutate(xG_diff = Goals - xG, .after = xG,) %>%
  mutate(xA_diff = Assists - xA, .after = xA) %>%
  mutate(xGc_diff = GC - xGc, .after = xGc) %>%
  mutate(Goals90 = Goals / (Minutes / 90), .before = xG90) %>%
  mutate(xG90_diff = Goals90 - xG90, .after = xG90) %>%
  mutate(Assists90 = Assists / (Minutes / 90), .before = xA90) %>%
  mutate(xA90_diff = Assists90 - xA90, .after = xA90) %>%
  mutate(GC90 = GC / (Minutes / 90), .before = xGc90) %>%
  mutate(xGc90_diff = GC90 - xGc90, .after = xGc90)

performance_data <- fpl_data[c(
  "Player",
  "Club",
  "Position",
  "Level",
  "Cost",
  "Points",
  "Minutes",
  "Points90",
  "Goals90",
  "xG90",
  "xG90_diff",
  "Assists90",
  "xA90",
  "xA90_diff",
  "GC90",
  "xGc90",
  "xGc90_diff",
  "CS90"
)] %>%
  mutate_all( ~ ifelse(Minutes < 90, NA, .)) %>%
  na.omit()

# remove data: fpl_raw_data to save memory
rm(fpl_raw_data)

# constant
Positions <-  c("FWD", "MID", "DEF", "GKP")
Levels <- unique(fpl_data$Level)
Clubs <- unique(fpl_data$Club)
Costs <- seq(from = 4, to = 13.5, by = 0.5)

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

# type checkbox choices
type_choices <- max(performance_data$Goals)
assists_max <- max(performance_data$Assists)
gc_max <- max(performance_data$GC)

# stats type
stats_checkbox <- list(
  "Points" = "Points",
  "Minutes" = "Minutes",
  "Goals" = "Goals",
  "Assists" = "Assists",
  "Goals Concede" = "GC",
  "Clean Sheets" = "CS"
)
stats_attack <- c("Points", "Minutes", "Goals", "Assists")
stats_defence <- c("Points", "Minutes", "GC", "CS")
stats_points <- c("Points")
stats_minutes <- c("Minutes")
stats_goals <- c("Goals90", "xG90", "xG90_diff")
stats_assists <- c("Assists90", "xA90", "xA90_diff")
stats_gc <- c("GC90", "xGc90", "xGc90_diff")
stats_cs <- c("CC90")
all_stats <-
  c(stats_points, stats_goals, stats_assists, stats_gc, stats_cs)

# ui
ui <-
  navbarPage(
    "FPL Data Explore",
    
    tabPanel(
      "Points Distribution",
      fluidPage(
        tags$head(tags$style(
          HTML(
            ".title{
                     text-align: center;
                     font-size: 24px;
                     font-weight: bold;
                     color: #3c8dbc;
                     padding: 10px;
                     }"
          )
        )), 
        div(class = "title", "Total points distribution across different levels"),
        
        sidebarLayout(
          sidebarPanel(width = 2,
                selectInput(
                  "distribution_type",
                  "Chart Type",
                  choices = list(
                    "boxplot" = 1,
                    "violin" = 2,
                    "donut" = 3
                  ),
                  selected = 1,
                ), 
            
            checkboxGroupInput(
              "distribution_positions",
              "Position",
              choices = list(
                "FWD" = "FWD",
                "MID" = "MID",
                "DEF" = "DEF",
                "GKP" = "GKP"
              ),
              selected = Positions
            ),
            
            checkboxGroupInput(
              "distribution_levels",
              "Levels",
              choices = list(
                "Premium" = "Premium",
                "Middle" = "Middle",
                "Low" = "Low"
              ),
              selected = Levels
            ),

          ),
          
          mainPanel(width = 10, plotOutput("multiple_distribution_plot"))
        )
      )
    ),
    
    navbarMenu(
      "Points, Positions and Values",
      tabPanel(
        "Points and Values",
        fluidPage(
          tags$head(tags$style(
            HTML(
              ".title{
           text-align: center;
           font-size: 24px;
           font-weight: bold;
           color: #3c8dbc;
           padding: 20px;
           }"
            )
          )),
          div(class = "title", "Compare Player Points by Level or Value"),
          
          sidebarLayout(
            sidebarPanel(
              selectInput("scatter_level",
                          "Level",
                          choices = Levels,
                          selected = Levels[2]),
              
              sliderInput(
                "scatter_value_range",
                "Value Range",
                min = 3.7,
                max = 13.1,
                value = c(6.83, 9.97),
                step = 0.1
              )
              
            ),
            mainPanel(plotOutput("scatter"))
          )
        )
      ),
      
      tabPanel(
        "Points and Position",
        fluidPage(
          tags$head(tags$style(
            HTML(
              ".title{
           text-align: center;
           font-size: 24px;
           font-weight: bold;
           color: #3c8dbc;
           padding: 10px;
           }"
            )
          )),
          
          div(class = "title", "Compare two different players in the same position"),
          
          sidebarLayout(
            sidebarPanel(width = 3, 
              tabsetPanel(
              tabPanel(
                "Player 1",
                selectInput("radar_club1",
                            "Club",
                            choices = Clubs,
                            selected = Clubs[1]),
                
                selectInput(
                  "radar_position1",
                  "Position",
                  choices = Positions,
                  selected = Positions[1]
                ),
                
                selectInput("radar_player1",
                            "Name",
                            choices = NULL),
                
                textInput("radar_value1",
                          "Value",
                          value = NULL)
                
              ),
              
              tabPanel(
                "Player 2",
                selectInput("radar_club2",
                            "Club",
                            choices = Clubs,
                            selected = Clubs[1]),
                
                selectInput("radar_position2",
                            "Position",
                            choices = NULL),
                
                
                selectInput("radar_player2",
                            "Name",
                            choices = NULL),
                
                textInput("radar_value2",
                          "Value",
                          value = NULL),
              )
            )),
            mainPanel(width = 7, plotOutput("radar_plot"), tableOutput("radar_table"))
          )
        )
      )
    ),
    
    tabPanel(
      "Actual and Expected Values",
      
      fluidPage(
        tags$head(tags$style(
          HTML(
            ".title{
           text-align: center;
           font-size: 24px;
           font-weight: bold;
           color: #3c8dbc;
           padding: 10px;
           }"
          )
        )),
        
        div(class = "title", "Compare players' actual values and expected values"),
        
        sidebarLayout(
          sidebarPanel(width = 2,
            selectInput(
              "grid_position",
              "Position",
              choices = Positions,
              selected = Positions[1]
            ),
            
            selectInput("grid_level",
                        "Level",
                        choices = Levels,
                        selected = Levels[3]),
            
            sliderInput(
              "grid_value_range",
              "Value Range",
              min = 3.7,
              max = 13.1,
              value = c(9.97, 13.1),
              step = 0.1
            ),
            
            checkboxGroupInput("grid_types",
                               "Show Stat",
                               choices = stats_checkbox,
                               selected = stats_attack),
            
          ),
          
          mainPanel(width = 10, plotOutput("grid_plot"))
        ),
        
        fluidRow(
          column(3),
          column(9, tableOutput("grid_table"))
          )
      )
    )
    
  )

server <- function(input, output) {
  # multiple_distribution_plot
  output$multiple_distribution_plot <- renderPlot({
    # all selected position
    positions <- input$distribution_positions
    # all selected levels
    levels <- input$distribution_levels
    # filter
    plot_data <-
      fpl_data %>% filter(Position %in% positions, Level %in% levels)
    # type
    if (nrow(plot_data) == 0) {
      return (NULL)
    }
    plot_type = input$distribution_type
    if (plot_type == 1) {
      blox(plot_data)
    } else if (plot_type == 2) {
      violin(plot_data)
    } else if (plot_type == 3) {
      donut(plot_data)
    }
  })
  
  # plot_functions
  blox <- function(plot_data) {
    plot_data %>%
      ggplot(aes(x = Level, y = Points, fill = Position)) +
      geom_boxplot() +
      stat_summary(
        fun = mean,
        geom = "point",
        shape = 20,
        size = 10,
        color = "#ec7014",
        fill = "#ec7014"
      ) +
      scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
      geom_jitter(color = "black",
                  size = 0.4,
                  alpha = 0.9) +
      ylim(0, 300) +
      xlab("Level") +
      theme_ipsum()
  }
  
  violin <- function(plot_data) {
    plot_data %>%
      ggplot(aes(x = Level, y = Points, fill = Position)) +
      geom_violin(alpha = .6) +
      xlab("Level") +
      theme_ipsum()
  }
  
  donut <- function(plot_data) {
    # the beauty of pipeline %>%
    aggregate(plot_data$Points,
              by = list(Position = plot_data$Position),
              FUN = sum) %>%
      mutate(
        percentage = x / sum(x),
        ymax = cumsum(percentage),
        ymin = c(0, head(ymax, n = -1)),
        labelPosition = (ymax + ymin) / 2,
        label = paste(Position,
                      "\n",
                      round(percentage, 2) * 100,
                      "%",
                      sep = "")
      ) %>%
      ggplot(aes(
        ymax = ymax,
        ymin = ymin,
        xmax = 4,
        xmin = 3,
        fill = Position
      )) +
      geom_rect() +
      coord_polar(theta = "y") +
      geom_label(x = 3.5,
                 aes(y = labelPosition, label = label),
                 size = 5) +
      scale_fill_brewer(palette = 4) +
      scale_color_brewer(palette = 3) +
      xlim(c(2, 4)) +
      theme_void() +
      theme(legend.position = "none")
  }
  
  # shiny_scatter
  # change the value slider depends on level
  observeEvent(input$scatterlevel, {
    new_level <- input$scatterlevel
    if (new_level == Levels[1]) {
      updateSliderInput(inputId = "scatter_value_range", value = c(3.7, 6.83))
    } else if (new_level == Levels[2]) {
      updateSliderInput(inputId = "scatter_value_range", value = c(6.83, 9.97))
    } else {
      updateSliderInput(inputId = "scatter_value_range", value = c(9.97, 13.1))
    }
  })
  
  output$scatter <- renderPlot({
    plot_data <- fpl_data %>%
      filter(Points > 50) %>%
      filter(Cost > input$scatter_value_range[1], Cost <= input$scatter_value_range[2])
    
    ggplot(plot_data, aes(x = Points, y = Cost)) +
      geom_point(aes(color = Position, shape = Position),
                 size = 1.5,
                 alpha = 0.8) +
      scale_color_manual(values = c("#386cb0", "#fdb462", "#7fc97f", "red")) +
      xlim(0, 300) +
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
      labs(x = "total points",
           y = "cost(m)") +
      theme(
        # The default font when not explicitly specified
        text = element_text(
          family = "Lobster Two",
          size = 8,
          color = "black"
        ),
        
        # Customize legend text, position, and background.
        legend.text = element_text(size = 9, family = "Roboto"),
        legend.position = c(1, 0.85),
        legend.justification = c(1, 0),
        legend.background = element_blank(),
        # This one removes the background behind each key in the legend
        legend.key = element_blank(),
        legend.title = element_blank(),
        
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
  
  # shiny_radar
  # player 1 filter
  player1_filter <- reactive({
    fpl_data %>%
      filter(Club == input$radar_club1, Position == input$radar_position1) %>%
      arrange(desc(Cost))
  })
  
  observeEvent(player1_filter(), {
    choices <- unique(player1_filter()$Player)
    updateSelectInput(inputId = "radar_player1", choices = choices)
  })
  
  # player 2 filter
  player2_filter <- reactive({
    fpl_data %>%
      filter(Club == input$radar_club2,
             Position == input$radar_position2,
             Player != input$radar_player1) %>%
      arrange(desc(Cost))
  })
  
  observeEvent(player2_filter(), {
    choices <- unique(player2_filter()$Player)
    updateSelectInput(inputId = "radar_player2", choices = choices)
  })
  
  # player 2 position depends on player 1
  observeEvent(input$radar_position1, {
    updateSelectInput(inputId = "radar_position2", choices = c(input$radar_position1))
  })
  
  # player 1 value depends on player 1 name
  player1_value_filter <- reactive({
    value_player <-  fpl_data %>%
      filter(Player == input$radar_player1)
    value_player$Cost
  })
  
  observeEvent(input$radar_player1, {
    player1_value_text <- paste0(player1_value_filter(), "m", seq = "")
    updateTextInput(inputId = "radar_value1", value = player1_value_text)
  })
  
  # player 2 value depends on player 2 name
  player2_value_filter <- reactive({
    value_player <-  fpl_data %>%
      filter(Player == input$radar_player2)
    value_player$Cost
  })
  
  observeEvent(input$radar_player2, {
    player2_value_text <- paste0(player2_value_filter(), "m", seq = "")
    updateTextInput(inputId = "radar_value2", value = player2_value_text)
  })
  
  # radar data
  radar_data_filter <- reactive({
    # organize data
    req(input$radar_player1)
    req(input$radar_player2)
    radarCols <- get_radarCols(input$radar_position1)
    fpl_data %>%
      filter(Player %in% c(input$radar_player1, input$radar_player2)) %>%
      select(all_of(radarCols))
  })
  
  output$radar_plot <- renderPlot({
    radarCols <- get_radarCols(input$radar_position1)
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
      legend = c(input$radar_player1, input$radar_player2),
      bty = "n",
      pch = 20 ,
      col = colors_in ,
      text.col = "grey",
      cex = 1.2,
      pt.cex = 3
    )
  })
  
  output$radar_table <- renderTable({
    radar_data_filter() %>%
      mutate(Player = c(input$radar_player1, input$radar_player2),
             .before = Points) %>% 
      mutate(Player = c(input$radar_value1, input$radar_value2),
             .after = Player)
  })
  
  # shiny_grid_extra
  # define a function to get show stats
  get_stats_by_type <- function(type) {
    case_when(
      type == "Points" ~ stats_points,
      type == "Minutes" ~ stats_minutes,
      type == "Goals" ~ stats_goals,
      type == "Assists" ~ stats_assists,
      type == "GC" ~ stats_gc,
      type == "CS" ~ stats_gc
    )
  }
  
  # change the checkbox default depends on position
  observeEvent(input$grid_position, {
    new_position <- input$grid_position
    if (new_position == Positions[1] |
        new_position == Positions[2]) {
      updateCheckboxGroupInput(inputId = "grid_types", selected = stats_attack)
    } else{
      updateCheckboxGroupInput(inputId = "grid_types", selected = stats_defence)
    }
  })
  
  # change the value slider depends on level
  observeEvent(input$grid_level, {
    new_level <- input$grid_level
    if (new_level == Levels[1]) {
      updateSliderInput(inputId = "grid_value_range", value = c(3.7, 6.83))
    } else if (new_level == Levels[2]) {
      updateSliderInput(inputId = "grid_value_range", value = c(6.83, 9.97))
    } else {
      updateSliderInput(inputId = "grid_value_range", value = c(9.97, 13.1))
    }
  })
  
  output$grid_plot <- renderPlot({
    # all show types
    types <- input$grid_types
    # get all the show stats name
    show_stats <- ""
    if (is.null(types)) {
      show_stats <- c(stats_points, stats_goals)
    } else{
      for (type in strsplit(types, ", ")) {
        show_stats <- unique(c(show_stats, get_stats_by_type(type)))
      }
    }
    show_stats <- show_stats[show_stats != ""]
    
    # plot_data
    plot_data <- performance_data %>%
      filter(Cost > input$grid_value_range[1],
             Cost <= input$grid_value_range[2],
             Position == input$grid_position)
    
    player_number <- length(plot_data)
    
    # plot
    p1 = NA
    p2 = NA
    p3 = NA
    p4 = NA
    p5 = NA
    p6 = NA
    
    if (nrow(plot_data) > 0) {
      if (!is.null(types)) {
        if ("Points" %in% types) {
          p1 <- points_plot(plot_data)
        }
        if ("Minutes" %in% types) {
          p2 <- minutes_plot(plot_data)
        }
        if ("Goals" %in% types) {
          p3 <- goals_plot(plot_data)
        }
        if ("Assists" %in% types) {
          p4 <- assists_plot(plot_data)
        }
        if ("GC" %in% types) {
          p5 <- gc_plot(plot_data)
        }
        if ("CS" %in% types) {
          p6 <- cs_plot(plot_data)
        }
      }
    }
    
    # grid
    plot_list <- list(p1, p2, p3, p4, p5, p6)
    # remove plot NAs
    plot_list <-  plot_list[!is.na(plot_list)]
    if (length(plot_list) > 0) {
      # arrange
      grid.arrange(grobs = plot_list, ncol = 2, )
    }
  })
  
  # plot for points
  points_plot <- function(plot_data) {
    plot_data %>%
      select(c("Cost", "Points90")) %>%
      mutate(Value = Points90, Type = "Points90") %>%
      plot()
  }
  
  # plot for minutes
  minutes_plot <- function(plot_data) {
    plot_data %>%
      select(c("Cost", "Minutes")) %>%
      mutate(Value = Minutes, Type = "Minutes") %>%
      plot()
  }
  
  # plot for goals
  goals_plot <- function(plot_data) {
    plot_data %>%
      select(c("Cost", "Goals90", "xG90", "xG90_diff")) %>%
      pivot_longer (2:4, names_to = "Type", values_to = "Value") %>%
      plot()
  }
  
  # plot for assists
  assists_plot <- function(plot_data) {
    plot_data %>%
      select(c("Cost", "Assists90", "xA90", "xA90_diff")) %>%
      pivot_longer (2:4, names_to = "Type", values_to = "Value") %>%
      plot()
  }
  
  # plot for goals concede
  gc_plot <- function(plot_data) {
    plot_data %>%
      select(c("Cost", "GC90", "xGc90", "xGc90_diff")) %>%
      pivot_longer (2:4, names_to = "Type", values_to = "Value") %>%
      plot()
  }
  
  # plot for clean sheets
  cs_plot <- function(plot_data) {
    plot_data %>%
      select(c("Cost", "CS90")) %>%
      mutate(Value = CS90, Type = "CS90") %>%
      plot()
  }
  
  # plot line chart
  plot <- function(data) {
    data %>%
      ggplot(aes(
        x = Cost,
        y = Value,
        group = Type,
        color = Type
      )) +
      geom_line() +
      scale_color_viridis(discrete = TRUE) +
      xlab("Cost(m)") +
      ylab("") +
      theme_ipsum() +
      theme(legend.title = element_blank())
  }
  
  output$grid_table <- renderTable({
    # all show types
    types <- input$grid_types
    # get all the show stats name
    show_stats <- ""
    if (is.null(types)) {
      show_stats <- c(stats_points, stats_goals)
    } else{
      for (type in strsplit(types, ", ")) {
        show_stats <- unique(c(show_stats, get_stats_by_type(type)))
      }
    }
    show_stats <- show_stats[show_stats != ""]
    performance_data %>%
      filter(Cost > input$grid_value_range[1],
             Cost <= input$grid_value_range[2],
             Position == input$grid_position) %>%
      arrange(desc(Cost)) %>%
      select(all_of(c("Player", "Level", "Cost", show_stats)))
  })
  
}

shinyApp(ui, server)