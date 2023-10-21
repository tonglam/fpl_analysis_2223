library(shiny)
library(tidyverse)
library(rpart)
library(gridExtra)
library(ROCR)
library(ROCit)
library(caret)
library(factoextra)
library(xgboost)

# constants
data_scale <- "small"

spilt_ratio <- 0.9

target <- 'player_type'
target_value <- 'player_type_value'
pos.label <- 'Offensive'
neg.label <- 'Defensive'

catVars <-
  c(
    "penalties_order",
    "direct_freekicks_order",
    "corners_and_indirect_freekicks_order"
  )
numericVars <-
  c(
    "clean_sheets_per_90",
    "expected_assists_per_90",
    "penalties_missed",
    "starts_per_90",
    "expected_goals_conceded_per_90",
    "red_cards_per_90",
    "threat_per_90",
    "influence_per_90",
    "penalties_saved",
    "creativity_per_90",
    "saves_per_90",
    "own_goals_per_90",
    "yellow_cards_per_90",
    "expected_goal_involvements_per_90",
    "ict_index",
    "minutes_per_90",
    "expected_goals_per_90"
  )

selected_catVars <- catVars
selected_numericVars <-
  c(
    "starts_per_90",
    "expected_goal_involvements_per_90",
    "expected_goals_per_90",
    "expected_assists_per_90"
  )

combined_features <-
  c(
    "penalties_order",
    "threat_per_90",
    "expected_goal_involvements_per_90",
    "creativity_per_90",
    "expected_goals_per_90",
    "expected_assists_per_90",
    "ict_index",
    "influence_per_90",
    "saves_per_90",
    "starts_per_90",
    "own_goals_per_90",
    "penalties_saved"
  )

rfe_features <-
  c(
    "creativity_per_90",
    "threat_per_90",
    "expected_goals_per_90",
    "expected_goal_involvements_per_90",
    "influence_per_90",
    "expected_assists_per_90",
    "starts_per_90",
    "ict_index",
    "yellow_cards_per_90",
    "minutes_per_90",
    "clean_sheets_per_90",
    "saves_per_90"
  )

xgb_params <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  lambda = 1,
  alpha = 0.1,
  max_depth = 3,
  min_child_weight = 5,
  eta = 0.1
)

k_means_center <- 4

# classification_preprocess
class_transform <- function(data) {
  data %>%
    select(
      -c(
        "id",
        "team",
        "name",
        "now_cost",
        "transfers_out",
        "value_form",
        "value_season",
        "cost_change_start",
        "news_added",
        "cost_change_start_fall",
        "ep_next",
        "event_points",
        "web_name",
        "status",
        "news",
        "chance_of_playing_next_round",
        "dreamteam_count",
        "chance_of_playing_this_round",
        "points_per_game",
        "total_points",
        "in_dreamteam",
        "form",
        "ep_this",
        "transfers_in",
        "selected_by_percent",
        "bps",
        "bonus"
      )
    ) %>%
    select(-ends_with("rank")) %>%
    select(-ends_with("rank_type")) %>%
    mutate(
      corners_and_indirect_freekicks_order = ifelse(
        is.na(corners_and_indirect_freekicks_order),
        "Non-Taker",
        "Taker"
      ),
      penalties_order = ifelse(is.na(penalties_order), "Non-Taker", "Taker"),
      direct_freekicks_order = ifelse(is.na(direct_freekicks_order), "Non-Taker", "Taker")
    ) %>%
    select(
      -c(
        "clean_sheets",
        "expected_assists",
        "starts",
        "expected_goals_conceded",
        "saves",
        "expected_goal_involvements",
        "expected_goals"
      )
    ) %>%
    mutate(minutes_per_90 = minutes / 90, .after = minutes) %>%
    mutate(assists_per_90 = assists / minutes * 90, .after = assists) %>%
    mutate(goals_per_90 = goals_scored / minutes * 90,
           .after = goals_scored) %>%
    mutate(goals_conceded_per_90 = goals_conceded / minutes * 90,
           .after = goals_conceded) %>%
    mutate(red_cards_per_90 = red_cards / minutes * 90, .after = red_cards) %>%
    mutate(threat_per_90 = threat / minutes * 90, .after = threat) %>%
    mutate(influence_per_90 = influence / minutes * 90, .after = influence) %>%
    mutate(creativity_per_90 = creativity / minutes * 90,
           .after = creativity) %>%
    mutate(own_goals_per_90 = own_goals / minutes * 90, .after = own_goals) %>%
    mutate(yellow_cards_per_90 = yellow_cards / minutes * 90,
           .after = yellow_cards) %>%
    select(
      -c(
        "minutes",
        "assists",
        "goals_scored",
        "goals_conceded",
        "red_cards",
        "threat",
        "influence",
        "creativity",
        "own_goals",
        "yellow_cards"
      )
    ) %>%
    mutate(
      minutes_per_90 = ifelse(is.na(minutes_per_90), 0, minutes_per_90),
      assists_per_90 = ifelse(is.na(assists_per_90), 0, assists_per_90),
      goals_per_90 = ifelse(is.na(goals_per_90), 0, goals_per_90),
      goals_conceded_per_90 = ifelse(is.na(goals_conceded_per_90), 0, goals_conceded_per_90),
      red_cards_per_90 = ifelse(is.na(red_cards_per_90), 0, red_cards_per_90),
      threat_per_90 = ifelse(is.na(threat_per_90), 0, threat_per_90),
      influence_per_90 = ifelse(is.na(influence_per_90), 0, influence_per_90),
      creativity_per_90 = ifelse(is.na(creativity_per_90), 0, creativity_per_90),
      own_goals_per_90 = ifelse(is.na(own_goals_per_90), 0, own_goals_per_90),
      yellow_cards_per_90 = ifelse(is.na(yellow_cards_per_90), 0, yellow_cards_per_90)
    ) %>%
    mutate(
      player_type = ifelse(position == 'GKP' |
                             position == 'DEF', "Defensive", "Offensive"),
      .before = position
    ) %>%
    mutate(player_type_value = ifelse(player_type == "Defensive", 2, 1),
           .after = player_type) %>%
    select(-position)
}

class_spilt <- function(data, ratio = spilt_ratio) {
  set.seed(500)
  fortrain <- runif(nrow(data)) < ratio
  train_data <- data[fortrain,]
  test_data <- data[!fortrain,]
  outCol <- names(train_data)[-c(1, 2)]
  vars <- setdiff(outCol, c('player_type', "player_type_value"))
  return(list(train = train_data, test = test_data))
}

class_prepossessing <- function(data) {
  list <- data %>%
    class_transform() %>%
    class_spilt()
  train_data <- list$train
  test_data <- list$test
  return(list(train = train_data, test = test_data))
}

class_mkPredC <- function(outCol, varCol, appCol, pos = pos.label) {
  pPos <- sum(outCol == pos) / length(outCol)
  naTab <- table(as.factor(outCol[is.na(varCol)]))
  pPosWna <- (naTab / sum(naTab))[pos]
  vTab <- table(as.factor(outCol), varCol)
  pPosWv <- (vTab[pos, ] + 1.0e-3 * pPos) / (colSums(vTab) + 1.0e-3)
  pred <- pPosWv[appCol]
  pred[is.na(appCol)] <- pPosWna
  pred[is.na(pred)] <- pPos
  return(pred)
}

class_cat_pred <- function(catVars, train_data, test_data) {
  for (v in catVars) {
    pi <- paste('pred_', v, sep = '')
    train_data[, pi] <-
      class_mkPredC(train_data[, "player_type"], train_data[, v], train_data[, v])
    test_data[, pi] <-
      class_mkPredC(train_data[, "player_type"], train_data[, v], test_data[, v])
  }
  return (list(train = train_data, test = test_data))
}

class_mkPredN <- function(outCol, varCol, appCol) {
  cuts <- unique(quantile(varCol, probs = seq(0, 1, 0.1), na.rm = T))
  varC <- cut(varCol, cuts)
  appC <- cut(appCol, cuts)
  class_mkPredC(outCol, varC, appC)
}

class_num_pred <- function(numVars, train_data, test_data) {
  for (v in numVars) {
    pi <- paste('pred_', v, sep = '')
    train_data[, pi] <-
      class_mkPredN(train_data[, 'player_type'], train_data[, v], train_data[, v])
    test_data[, pi] <-
      class_mkPredN(train_data[, 'player_type'], train_data[, v], test_data[, v])
  }
  return (list(train = train_data, test = test_data))
}

calcAUC <- function(predcol, outcol, pos = pos.label) {
  perf <- performance(prediction(predcol, outcol == pos), 'auc')
  as.numeric(perf@y.values)
}

class_single_performance <- function(train_data, test_data, vars) {
  result <- tribble(~ feature, ~ type, ~ pred, ~ trainAUC, ~ testAUC)
  categorical_vars <-
    vars[sapply(train_data[, vars], class) %in% c('factor', 'character')]
  numerical_vars <-
    vars[sapply(train_data[, vars], class) %in% c('numeric', 'integer')]
  # categorical
  for (v in categorical_vars) {
    pi <- paste('pred_', v, sep = '')
    train_data[, pi] <-
      mkPredC(train_data[, 'player_type'], train_data[, v], train_data[, v])
    aucTrain <-
      calcAUC(train_data[, pi], train_data[, "player_type"])
    aucTest <- calcAUC(test_data[, pi], test_data[, "player_type"])
    result <- sprintf("%s: trainAUC: %4.3f; TestAUC: %4.3f",
                      pi, aucTrain, aucTest)
    catResult <-
      add_row(
        catResult,
        feature = pi,
        type = "categorical",
        pred = result,
        trainAUC = aucTrain,
        testAUC = aucTest
      )
  }
  # numerical
  for (v in categorical_vars) {
    pi <- paste('pred_', v, sep = '')
    train_data[, pi] <-
      mkPredN(train_data[, 'player_type'], train_data[, v], train_data[, v])
    aucTrain <-
      calcAUC(train_data[, pi], train_data[, "player_type"])
    aucTest <- calcAUC(test_data[, pi], test_data[, "player_type"])
    result <- sprintf("%s: trainAUC: %4.3f; TestAUC: %4.3f",
                      pi, aucTrain, aucTest)
    catResult <-
      add_row(
        catResult,
        feature = pi,
        type = "numerical",
        pred = result,
        trainAUC = aucTrain,
        testAUC = aucTest
      )
  }
  
  return(result)
}

class_model_performance <- function(pred, truth, name = "model") {
  if (str_detect(name, "xgb")) {
    pred_class <- ifelse(pred > 0.5, pos.label, neg.label)
    truth_class <- ifelse(truth > 0.5, pos.label, neg.label)
  }
  factor_pred <- pred
  if (class(pred) != "factor") {
    if (str_detect(name, "xgb")) {
      factor_pred <- as.factor(pred_class)
    } else{
      factor_pred <- as.factor(pred)
    }
  }
  factor_truth <- truth
  if (class(truth) != "factor") {
    factor_truth <- as.factor(truth)
  }
  cm <- confusionMatrix(factor_pred, factor_truth)
  accuracy <- cm$overall['Accuracy']
  precision <- cm$byClass['Pos Pred Value']
  recall <- cm$byClass['Sensitivity']
  f1 <- cm$byClass['F1']
  if (str_detect(name, "tree")) {
    pred <- as.numeric(pred)
  }
  AUC <- calcAUC(pred, truth)
  type_truth <- ifelse(truth == "Offensive", 1, 2)
  likelihood <- logLikelihood(type_truth, pred)
  return(
    list(
      Model = name,
      Accuracy = accuracy,
      Precision = precision,
      Recall = recall,
      F1 = f1,
      AUC = AUC,
      Loglikelihood = likelihood
    )
  )
}

tree_model <- function(train_data, test_data, selected_features) {
  tree_model <-
    rpart(player_type ~ ., data = train_data, method = 'class')
  
  train_pred <-
    predict(tree_model, newdata = train_data)
  test_pred <-
    predict(tree_model, newdata = test_data)
  
  train_pred_class <-
    predict(tree_model, newdata = train_data, type = "class")
  test_pred_class <-
    predict(tree_model, newdata = test_data, type = "class")
  
  return (
    list(
      model = tree_model,
      model_name = 'Decision Tree',
      train_pred = train_pred,
      test_pred = test_pred,
      train_pred_class = train_pred_class,
      test_pred_class = test_pred_class
    )
  )
}

xgb_model <- function(train_data, test_data, selected_features) {
  xgb_train_data <- train_data %>%
    select(all_of(c(target, selected_features))) %>%
    mutate(type = as.numeric(player_type == 'Offensive'),
           .after = player_type)
  
  xgb_test_data <- test_data %>%
    select(all_of(c(target, selected_features))) %>%
    mutate(
      type = as.numeric(player_type == 'Offensive'),
      .after = player_type
    )
  
  if ("penalties_order" %in% selected_features) {
    xgb_train_data <-
      xgb_train_data %>% mutate(penalties_order = ifelse(penalties_order == 'Taker', 1, 0))
    xgb_test_data <-
      xgb_test_data %>% mutate(penalties_order = ifelse(penalties_order == 'Taker', 1, 0))
  }
  
  input <- as.matrix(xgb_train_data[-c(1, 2)])
  test_input <- as.matrix(xgb_test_data[-c(1, 2)])
  
  xgb_model <- xgboost(
    data = input,
    label = xgb_train_data$type,
    params = xgb_params,
    nrounds = 10,
    early_stopping_rounds = 10,
    verbose = 0
  )
  
  train_pred <- predict(xgb_model, input)
  train_pred_class <- ifelse(train_pred > 0.5, pos.label, neg.label)
  test_pred <- predict(xgb_model, test_input)
  test_pred_class <- ifelse(test_pred > 0.5, pos.label, neg.label)
  
  return (
    list(
      model = xgb_model,
      model_name = 'XGBoost',
      train_pred = train_pred,
      test_pred = test_pred,
      train_pred_class = train_pred_class,
      test_pred_class = test_pred_class
    )
  )
}

# clustering_preprocess
clustering_transform <- function(data) {
  data %>%
    select(
      -c(
        "id",
        "team",
        "name",
        "transfers_out",
        "value_form",
        "cost_change_start",
        "news_added",
        "cost_change_start_fall",
        "ep_next",
        "web_name",
        "status",
        "news",
        "chance_of_playing_next_round",
        "dreamteam_count",
        "chance_of_playing_this_round",
        "in_dreamteam",
        "form",
        "ep_this",
        "transfers_in"
      )
    ) %>%
    select(
      -c(
        "value_season",
        "points_per_game",
        "clean_sheets_per_90",
        "expected_goals_per_90",
        "expected_assists_per_90",
        "starts_per_90",
        "expected_goals_conceded_per_90",
        "saves_per_90",
        "expected_goal_involvements_per_90",
        "expected_goals_per_90",
        "goals_conceded_per_90",
      )
    ) %>%
    select(-ends_with("rank")) %>%
    select(-ends_with("rank_type")) %>%
    mutate(
      corners_and_indirect_freekicks_order = ifelse(
        is.na(corners_and_indirect_freekicks_order),
        "Non-Taker",
        "Taker"
      ),
      penalties_order = ifelse(is.na(penalties_order), "Non-Taker", "Taker"),
      direct_freekicks_order = ifelse(is.na(direct_freekicks_order), "Non-Taker", "Taker")
    ) %>%
    mutate(
      player_type = ifelse(position == 'GKP' |
                             position == 'DEF', "Defensive", "Offensive"),
      .before = position
    ) %>%
    mutate(player_type_value = ifelse(player_type == "Defensive", 2, 1),
           .after = player_type) %>%
    select(-position)
}

clustering_OHE <- function(data) {
  colname <- names(data)
  cat_col_char <-
    str_split(colname[sapply(data[, colname], class) %in% c('factor', 'character')], " ")
  cat_col <- c()
  for (i in 1:length(cat_col_char)) {
    value = cat_col_char[[i]]
    cat_col <- c(cat_col, value)
  }
  encoded_data <-
    dummyVars(paste("~", paste(cat_col, collapse = "+")), data = data) %>%
    predict(newdata = data)
  return_data <-
    cbind(encoded_data, data[, -which(names(data) %in% cat_col)])
  return(return_data)
}

clustering_scale <- function(data) {
  data %>% scale()
}

clustering_prepossessing <- function(data) {
  data %>%
    clustering_transform() %>%
    clustering_OHE() %>%
    clustering_scale()
}

# prepare raw data
merge_raw_data <- function(raw_data_1, raw_data_2) {
  slim_data = raw_data_1
  wide_data = raw_data_2
  if (ncol(raw_data_1) > ncol(raw_data_2)) {
    slim_data <- raw_data_2
    wide_data <- raw_data_1
  }
  diff_columns <- setdiff(names(wide_data), names(slim_data))
  wide_data <-
    wide_data[, -which(names(wide_data) %in% diff_columns)]
  return (union(slim_data, wide_data))
}

read_data <- function(mode = 'silm') {
  if (mode == 'wide') {
    return (merge_raw_data(
      read.csv("./FPL_Dataset_2022-2023.csv"),
      read.csv("./FPL_Dataset_2023-2024.csv")
    ))
  }  else {
    return (read.csv("./FPL_Dataset_2022-2023.csv"))
  }
}

# plot
plot_auc_density <- function(data, features) {
  count = length(features)
  p1 <- data[, features] %>%
    filter(player_type == pos.label) %>%
    pivot_longer(2:count, names_to = "Type", values_to = "Value") %>%
    ggplot(aes(x = Value, color = Type)) +
    geom_density() +
    xlab(pos.label) 
  p2 <-  data[, features] %>%
    filter(player_type == neg.label) %>%
    pivot_longer(2:count, names_to = "Type", values_to = "Value") %>%
    ggplot(aes(x = Value, color = Type)) +
    geom_density() +
    xlab(neg.label)
  
  grid.arrange(p1, p2, ncol = 1)
}


plot_roc_class <-
  function(model_names,
           data_list,
           predcol_list,title='') {
    colors <- c(
      "salmon",
      "lightblue",
      "navyblue",
      "seagreen",
      "orchid",
      "lightpink",
      "yellow",
      "grey"
    )
    

    
    for (i in 1:length(data_list)) {
      par(new = TRUE)
      data <- data_list[[i]]
      predcols <- predcol_list[[i]]
      ROCit_obj <-
        rocit(score = predcols, data[[target]] == pos.label)
      plot(
        ROCit_obj,
        col = c(colors[i], 1),
        legend = FALSE,
        YIndex = FALSE,
        values = FALSE
      )
    }

    legend("bottomright",
           legend = model_names,
           col = colors,
           lty = 1)
    title(title)
    

  }

plot_roc_sa <-
  function(data,
           features,
           title) {
    colors = c("salmon",
               "lightblue",
               "navyblue",
               "seagreen",
               "orchid",
               "lightpink")
    for (i in 1:length(features)) {
      par(new = T)
      feature = features[i]
      pred_feature = paste0("pred_", feature)
      ROCit_obj <-
        rocit(score = data[[pred_feature]], class = data[, target] == pos.label)
      plot(
        ROCit_obj,
        col = c(colors[i], 1),
        legend = FALSE,
        YIndex = FALSE,
        values = FALSE
      )
      title(title)
    }
    legend("bottomright",
           legend = features,
           col = colors,
           lty = 1,ncol=2)
  }

# ui
ui <-
  navbarPage(
    "FPL Data Modelling",
    
    tabPanel(
      "Single Variable Model Performance",
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
        div(class = "title", "Single Variable"),
        
        sidebarLayout(
          sidebarPanel(
            width = 3,
            
            selectInput(
              "single_graph",
              "Graph Type",
              choices = list('Double Density' = 'desity', 'ROC Curve' = 'roc'),
              selected = 'desity'
            ),
            
            checkboxGroupInput(
              "categorical",
              "Categorical Variables",
              choices = catVars,
              selected = selected_catVars
            ),
            
            checkboxGroupInput(
              "numerical",
              "Numerical Variables",
              choices = numericVars,
              selected = selected_numericVars
            ),
            
          ),
          
          mainPanel(fluidRow(
            column(6, plotOutput("single_left")),
            column(6, plotOutput("single_right"))
          ))
        )
      )
    ),
    
    tabPanel(
      "Classification Model Performance",
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
        div(class = "title", "ROC by comparing different models"),
        
        sidebarLayout(
          sidebarPanel(
            width = 3,
            
            checkboxGroupInput(
              "model",
              "Classification Model",
              choices = c("Decision Tree", "XGBoost"),
              selected = c("Decision Tree"),
            ),
            
            checkboxGroupInput(
              "feature",
              "Feature Selection",
              choices = c("Concatenation", "Recursive Feature Elimination"),
              selected = c("Concatenation"),
            ),
            
          ),
          mainPanel(width = 9, plotOutput("classification"))
        )
      )
    ),
    
    tabPanel(
      "Clustering Result",
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
        
        div(class = "title", "Clustering"),
        
        sidebarLayout(
          sidebarPanel(
            width = 2,
            
            selectInput(
              "groups",
              "Clustering Groups",
              choices = list(
                "3" = 3,
                "4" = 4,
                "5" = 5,
                "6" = 6
              ),
              selected = 4,
            ),
            
            selectInput(
              "cluster_graph",
              "Clustering Groups",
              choices = list("Cluster Plot" = 'cluster',
                             "ASW Plot" = "asw"),
              selected = 4,
            ),
            
          ),
          mainPanel(width = 8, plotOutput("clustering"))
        )
      )
    )
  )

# server
server <- function(input, output) {
  # read data
  fpl_raw_data <- read_data()
  
  # classification
  class_result_list <- class_prepossessing(fpl_raw_data)
  class_train_data <- class_result_list$train
  class_test_data <- class_result_list$test
  
  # clustering
  cluster_scale_data <- clustering_prepossessing(fpl_raw_data)
  
  # remove raw data
  rm(fpl_raw_data)
  
  output$single_left <- renderPlot({
    # category single variable
    selected_cat_input <- input$categorical
    selected_catVars <- c()
    for (i in 1:length(selected_cat_input)) {
      value = selected_cat_input[[i]]
      selected_catVars <- c(selected_catVars, value)
    }
    if (length(selected_catVars) == 0) {
      return (NULL)
    }
    cat_return_list <-
      class_cat_pred(selected_catVars, class_train_data, class_test_data)
    single_cat_test_data <- cat_return_list$test
    # plot
    graph <- input$single_graph
    if (graph == 'desity') {
      plot_auc_density(single_cat_test_data,
                       c(target, selected_catVars))
    } else if (graph == 'roc') {
      # categorical
      plot_roc_sa(single_cat_test_data,
                  selected_catVars,
                  "ROC for Categorical Single Variable")
    }
  })
  
  output$single_right <- renderPlot({
    # numerical single variable
    selected_num_input <- input$numerical
    selected_numericVars <- c()
    for (i in 1:length(selected_num_input)) {
      value = selected_num_input[[i]]
      selected_numericVars <- c(selected_numericVars, value)
    }
    if (length(selected_numericVars) == 0) {
      return (NULL)
    }

    
    num_return_list <-
      class_num_pred(selected_numericVars, class_train_data, class_test_data)
    single_num_test_data <- num_return_list$test
    # plot
    graph <- input$single_graph
    if (graph == 'desity') {
      plot_auc_density(single_num_test_data,
                       c(target, selected_numericVars))
    } else if (graph == 'roc') {
      plot_roc_sa(
        single_num_test_data,
        selected_numericVars,
        "ROC for Numerical Single Variable"
      )
    }
  })
  
  # classification performance
  # build decision tree models - filter out the features
  train_tree_data_cb1 <-
    class_train_data[, c(target, combined_features)]
  test_tree_data_cb1 <-
    class_test_data[, c(target, combined_features)]
  train_tree_data_cb2 <- class_train_data[, c(target, rfe_features)]
  test_tree_data_cb2 <- class_test_data[, c(target, rfe_features)]
  #tree model 1
  tmodel1_list <-
    tree_model(train_tree_data_cb1, test_tree_data_cb1, combined_features)
  pred_train_tmodel1 <- tmodel1_list$train_pred
  pred_test_tmodel1 <- tmodel1_list$test_pred
  #tree model 2
  tmodel2_list <-
    tree_model(train_tree_data_cb2, test_tree_data_cb2, rfe_features)
  pred_train_tmodel2 <- tmodel2_list$train_pred
  pred_test_tmodel2 <- tmodel2_list$test_pred


  #build xbgboost models - filter out the features
  train_xgb_data_cb1<-class_train_data[,c(target,combined_features)]
  test_xgb_data_cb1<-class_test_data[,c(target,combined_features)]
  train_xgb_data_cb2<-class_train_data[,c(target,rfe_features)]
  test_xgb_data_cb2<-class_test_data[,c(target,rfe_features)]
  #xgb model 1
  xgb1_list<-xgb_model(train_xgb_data_cb1,test_xgb_data_cb1,combined_features)
  pred_train_xgb1<-xgb1_list$train_pred
  pred_test_xgb1<-xgb1_list$test_pred
  #xgb model 2
  xgb2_list<-xgb_model(train_xgb_data_cb2,test_xgb_data_cb2,rfe_features)
  pred_train_xgb2<-xgb2_list$train_pred
  pred_test_xgb2<-xgb2_list$test_pred
 
  #Plotting
  output$classification <- renderPlot({
    selected_models_input <- input$model
    selected_features_input <- input$feature
    #create a empty list to store the data
    model_names <- c()
    dataset_list <- list()
    pred_list <- list()
    #set the checkbox condition
    if ("Decision Tree" %in% selected_models_input) {
      #add data into the list
      if ("Concatenation" %in% selected_features_input) {
        model_names <- c(model_names, c("tmodel1-train",
                                        "tmodel1-test"))
        dataset_list <-
          c(dataset_list,
            list(train_tree_data_cb1),
            list(test_tree_data_cb1))
        pred_list <- c(pred_list,
                       list(pred_train_tmodel1[, 2]),
                       list(pred_test_tmodel1[, 2]))
      }
      if ("Recursive Feature Elimination" %in% selected_features_input) {
        model_names <- c(model_names, c("tmodel2-train",
                                        "tmodel2-test"))
        dataset_list <-
          c(dataset_list,
            list(train_tree_data_cb2),
            list(test_tree_data_cb2))
        pred_list <-
          c(pred_list,
            list(pred_train_tmodel2[, 2]),
            list(pred_test_tmodel2[, 2]))
      }
      
    }
    
    if ("XGBoost" %in% selected_models_input) {
      if ("Concatenation" %in% selected_features_input) {
        model_names <- c(model_names, c("xgb1-train",
                                        "xgb1-test"))
        dataset_list <- c(dataset_list,
                          list(train_xgb_data_cb1),
                          list(test_xgb_data_cb1))
        pred_list <- c(pred_list, list(pred_train_xgb1),
                       list(pred_test_xgb1))
        }
      if ("Recursive Feature Elimination" %in% selected_features_input) {
          model_names <- c(model_names, c("xgb2-train",
                                          "xgb2-test"))
          dataset_list <-
            c(dataset_list,
              list(train_xgb_data_cb2),
              list(test_xgb_data_cb2))
          pred_list <- c(pred_list,
                         list(pred_train_xgb2),
                         list(pred_test_xgb2))
        }
      }
    
    #check the input condition
    if (length(model_names) != 0) {
      plot_roc_class(model_names, dataset_list, pred_list)
    }
  })


  
  # clustering
  # async({
  #   kmClustering.ch <-
  #     kmeansruns(cluster_scale_data,
  #                krange = 1:10,
  #                criterion = "ch")
  #   kmClustering.asw <-
  #     kmeansruns(cluster_scale_data,
  #                krange = 1:10,
  #                criterion = "asw")
  #   kmCritframe <- data.frame(k = 1:10,
  #                             ch = kmClustering.ch$crit,
  #                             asw = kmClustering.asw$crit)
  #
    # output$clustering <- renderPlot({
    #   groups <- input$groups
    #   graph <- input$cluster_graph
    # 
    #   kmClusters <-
    #     kmeans(
    #       cluster_scale_data,
    #       centers = groups,
    #       iter.max = 100,
    #       trace = T
    #     )
    # 
    #   if (graph == 'cluster') {
    #     clusters <- kmClusters$cluster
    #     fviz_cluster(list(data = cluster_scale_data, cluster = clusters))
    #   } else if (graph == 'asw') {
    #     fig1 <- ggplot(kmCritframe, aes(x = k, y = ch)) +
    #       geom_point() + geom_line(colour = "salmon") +
    #       scale_x_continuous(breaks = 1:10, labels = 1:10) +
    #       labs(y = "CH index") + theme(text = element_text(size = 20))
    #     fig2 <- ggplot(kmCritframe, aes(x = k, y = asw)) +
    #       geom_point() + geom_line(colour = "dodgerblue") +
    #       scale_x_continuous(breaks = 1:10, labels = 1:10) +
    #       labs(y = "ASW") + theme(text = element_text(size = 20))
    #     fig3 <-
    #       fviz_cluster(list(data = cluster_scale_data, cluster = kmClusters$cluster))
    #     grid.arrange(fig1, fig2, fig3, ncol = 2)
    #   }
    # })

  # })

}

# app
shinyApp(ui, server)