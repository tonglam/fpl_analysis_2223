## input: position, club, player_name;  xG, G; xA, A

library(shiny)
library(tidyverse)

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
      xA = "expected_assists",
      cs = "clean_sheets",
      xGc = "expected_goals_conceded"
    )
    
  )) %>%
  
  mutate(cost = cost / 10)

# prepare data for select choices
positions <-  c("ALL", "FWD", "MID", "DEF", "GKP")
clubs <- c("ALL", unique(fpl_data$club))
costs <- c("ALL", seq(from = 4, to = 13.5, by = 0.5))

# User interface ----
ui <- fluidPage(titlePanel("FPL_Analysis_2223"),
                
                sidebarLayout(
                  sidebarPanel(
                    selectInput(
                      "position",
                      label = "Position",
                      choices = positions,
                      selected = positions[1]
                    ),
                    selectInput(
                      "club",
                      label = "Club",
                      choices = clubs,
                      selected = clubs[1]
                    ),
                    selectInput(
                      "cost",
                      label = "Cost",
                      choices = costs,
                      selected = costs[1]
                    ),
                    # player is a reactive selectInput
                    selectInput("player", label = "Player", choices = "")
                    
                    
                  ),
                  
                  mainPanel("aaa")
                  
                ))


# Server logic ----
server <- function(input, output, session) {
  observe({
    old_players <- input$player
    print(old_players)
    players <- get_players(input$position, input$club, input$cost)
    updateSelectInput(session,
                      "player",
                      choices = players,
                      selected = players[1])
  })
  
  
  
  
  # functions
  get_players <- function(position_value, club_value, cost_value) {
    player_data <- fpl_data
    if (position_value != 'ALL') {
      player_data <- player_data %>% filter(position == position_value)
    }
    if (club_value != 'ALL') {
      player_data <- player_data %>% filter(club == club_value)
    }
    if (cost_value != 'ALL') {
   #   player_data <- player_data %>% filter(cost >= cost_value & cost < cost_value + 0.5)
    }
    players <- player_data %>% arrange(club, player) %>% pull(player)
    return (players)
  }
  
}

# Run app ----
shinyApp(ui, server)
