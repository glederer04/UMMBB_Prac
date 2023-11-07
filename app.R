#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(htmltools)
library(htmlwidgets)
library(xml2)
library(htmlTable)
library(reactable)
library(shinyWidgets)
library(rvest)
library(shinyjs)
library(gt)
library(webshot)
library(gtExtras)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggplot2)

DailyReactable <- function(data) {
  data <- data[,-1]
  
  rim_normalized <- (data$Rim_Per - min(data$Rim_Per)) / (max(data$Rim_Per) - min(data$Rim_Per))
  if (data$Rim_Per[16] == 0) {
    rim_normalized[is.na(rim_normalized)] <- 0.5
  } else {
    rim_normalized <- pmax(0, pmin(1, rim_normalized))
  }
  #rim_normalized <- pmax(0, pmin(1, rim_normalized))
  rim_colors <- rgb(colorRamp(c("#DCF8C7", "#3E8F00"))(rim_normalized), maxColorValue = 255)
  
  mid_normalized <- (data$Mid_Per - min(data$Mid_Per)) / (max(data$Mid_Per) - min(data$Mid_Per))
  if (data$Mid_Per[16] == 0) {
    mid_normalized[is.na(mid_normalized)] <- 0.5
  } else {
    mid_normalized <- pmax(0, pmin(1, mid_normalized))
  }
  #mid_normalized <- pmax(0, pmin(1, mid_normalized))
  mid_colors <- rgb(colorRamp(c("#DCF8C7", "#3E8F00"))(mid_normalized), maxColorValue = 255)
  
  three_normalized <- (data$Three_Per - min(data$Three_Per)) / (max(data$Three_Per) - min(data$Three_Per))
  three_normalized <- ifelse(data$Three_Per > 0, pmax(0, pmin(1, three_normalized)), 0.5)
  three_colors <- rgb(colorRamp(c("#DCF8C7", "#3E8F00"))(three_normalized), maxColorValue = 255)
  
  reactable_code <- reactable(
    data,
    striped = TRUE,
    highlight = TRUE,
    bordered = FALSE,
    theme = reactableTheme(
      stripedColor = '#f0f0f0',
      backgroundColor = '#E3E3E3',
      borderColor = 'black'
    ),
    columns = list(
      url = colDef(name = '', cell = function(value) {
        div(style = "text-align: center;", tags$img(src = value, width = "35px", height = "auto"))
      },minWidth = 50, sticky = 'left'),
      Player = colDef(name = 'Player', minWidth = 175, filterable = TRUE, sticky = 'left'), 
      Pts = colDef(name = 'Pts', minWidth = 60, align = 'center'), 
      Reb = colDef(name = 'Reb', minWidth = 60, align = 'center'), 
      Ast = colDef(name = 'Ast', minWidth = 60, align = 'center'), 
      Rim = colDef(name = 'Rim', minWidth = 65, align = 'right'), 
      Rim_Per = colDef(name = 'Rim%', minWidth = 80,align = 'left', style = JS("function(rowInfo, column, state) {
        const { showColors, rim_colors } = state.meta
        if (showColors) {
          return { backgroundColor: rim_colors[rowInfo.index] }
        }
      }")),
      Mid = colDef(name = 'Mid', minWidth = 65, align = 'right'), 
      Mid_Per = colDef(name = 'Mid%', minWidth = 80, align = 'left', style = JS("function(rowInfo, column, state) {
        const { showColors, mid_colors } = state.meta
        if (showColors) {
          return { backgroundColor: mid_colors[rowInfo.index] }
        }
      }")),
      Three = colDef(name = '3pt', minWidth = 60, align = 'right'),
      Three_Per = colDef(name = '3pt%', minWidth = 70, align = 'left', style = JS("function(rowInfo, column, state) {
        const { showColors, three_colors } = state.meta
        if (showColors) {
          return { backgroundColor: three_colors[rowInfo.index] }
        }
      }")),
      Off_Reb = colDef(name = 'OReb', minWidth = 75, align = 'center'),
      Def_Reb = colDef(name = 'DReb', minWidth = 75, align = 'center'),
      Stl = colDef(name = 'Stl', minWidth = 60, align = 'center'),
      Blk = colDef(name = 'Blk', minWidth = 60, align = 'center'),
      Tov = colDef(name = 'Tov', minWidth = 60, align = 'center'),
      Charges = colDef(name = 'Charges', minWidth = 90, align = 'center'),
      Fouls = colDef(name = 'Fouls', minWidth = 70, align = 'center')
    ),
    rowStyle = function(row) {
      list(height = '45px')
    },
    meta = list(
      rim_colors = rim_colors,
      mid_colors = mid_colors,
      three_colors = three_colors,
      showColors = TRUE
    ),
    defaultPageSize = 16
    #elementId = 'practice-tbl'
  )
  return(reactable_code)
}

# daily_reg_gt <- function(data) {
#   rownames(data) <- NULL
#   data <- data[,-1]
#   table <- data %>%
#     gt() %>%
#     cols_label(url = '', Player = md('**Player**'), Pts = md('**Pts**'), Reb = md('**Reb**'), Ast = md('**Ast**'), Rim = md('**Rim**'), Rim_Per = md('**Rim%**'), Mid = md('**Mid**'), Mid_Per = md('**Mid%**'), Three = md('**3pt**'), Three_Per = md('**3pt%**'), Off_Reb = md('**OReb**'), Def_Reb = md('**DReb**'), Stl = md('**Stl**'), Blk = md('**Blk**'), Tov = md('**Tov**'), Charges = md('**Charges**'), Fouls = md('**Fouls**')) %>%
#     gt_add_divider(Ast) %>%
#     gt_add_divider(Rim_Per) %>%
#     gt_add_divider(Mid_Per) %>%
#     gt_add_divider(Three_Per) %>%
#     text_transform(locations = cells_body(vars(url)), fn = function(x) {web_image(url = x, height = px(22.5))}) %>%
#     cols_align(align = c('center'), columns = vars(Pts, Reb, Ast, Rim, Rim_Per, Mid, Mid_Per, Three, Three_Per, Off_Reb, Def_Reb, Stl, Blk, Tov, Charges, Fouls))
#   return(table)
# }

AdvDailyReact <- function(data) {
  rownames(data) <- NULL
  data <- data[,-1]
  
  basic_totals <- reactable(
    data,
    striped = TRUE,
    highlight = TRUE,
    bordered = FALSE,
    theme = reactableTheme(
      stripedColor = '#f0f0f0',
      backgroundColor = '#E3E3E3',
      borderColor = 'black'
    ),
    columns = list(
      url = colDef(name = '', cell = function(value) {
        div(style = "text-align: center;", tags$img(src = value, width = "35px", height = "auto"))
      }, minWidth = 50, sticky = 'left'),
      Player = colDef(name = 'Player', minWidth = 175, filterable = TRUE, sticky = 'left'), 
      Pts = colDef(name = 'Pts', minWidth = 60, align = 'center'), 
      Reb = colDef(name = 'Reb', minWidth = 60, align = 'center'), 
      Ast = colDef(name = 'Ast', minWidth = 60, align = 'center'),
      FG. = colDef(name = 'FG%', minWidth = 80, align = 'center'),
      eFG = colDef(name = 'eFG%', minWidth = 80, align = 'center'),
      Two = colDef(name = '2pt', minWidth = 65, align = 'center'),
      Two_Per = colDef(name = '2pt%', minWidth = 80, align = 'center'),
      Three = colDef(name = '3pt', minWidth = 65, align = 'center'),
      Three_Per = colDef(name = '3pt%', minWidth = 80, align = 'center')
    ),
    rowStyle = function(row) {
      list(height = '45px')
    },
    defaultPageSize = 16
  )
  return(basic_totals)
}

reg_reactable <- function(data) {
  rownames(data) <- NULL
  data <- data[,-1]
  
  rim_normalized <- (data$Rim_Per - min(data$Rim_Per)) / 
    (max(data$Rim_Per) - min(data$Rim_Per))
  rim_normalized <- pmax(0, pmin(1, rim_normalized))
  rim_colors <- rgb(colorRamp(c("#DCF8C7", "#3E8F00"))(rim_normalized), maxColorValue = 255)
  
  mid_normalized <- (data$Mid_Per - min(data$Mid_Per)) / 
    (max(data$Mid_Per) - min(data$Mid_Per))
  mid_normalized <- pmax(0, pmin(1, mid_normalized))
  mid_colors <- rgb(colorRamp(c("#DCF8C7", "#3E8F00"))(mid_normalized), maxColorValue = 255)
  
  three_normalized <- (data$Three_Per - min(data$Three_Per)) / 
    (max(data$Three_Per) - min(data$Three_Per))
  three_normalized <- pmax(0, pmin(1, three_normalized))
  three_colors <- rgb(colorRamp(c("#DCF8C7", "#3E8F00"))(three_normalized), maxColorValue = 255)
  
  complete_totals <- reactable(
    data,
    striped = TRUE,
    highlight = TRUE,
    bordered = FALSE,
    theme = reactableTheme(
      stripedColor = '#f0f0f0',
      backgroundColor = '#E3E3E3',
      borderColor = 'black'
    ),
    columns = list(
      urls = colDef(name = '', cell = function(value) {
        div(style = "text-align: center;", tags$img(src = value, width = "35px", height = "auto"))
      }, minWidth = 50, sticky = 'left'),
      Player = colDef(name = 'Player', minWidth = 175, filterable = TRUE, sticky = 'left'), 
      Pts = colDef(name = 'Pts', minWidth = 60, align = 'center'), 
      Reb = colDef(name = 'Reb', minWidth = 60, align = 'center'), 
      Ast = colDef(name = 'Ast', minWidth = 60, align = 'center'), 
      Rim = colDef(name = 'Rim', minWidth = 65, align = 'right'), 
      Rim_Per = colDef(name = 'Rim%', minWidth = 80,align = 'left', style = JS("function(rowInfo, column, state) {
        const { showColors, rim_colors } = state.meta
        if (showColors) {
          return { backgroundColor: rim_colors[rowInfo.index] }
        }
      }")),
      Mid = colDef(name = 'Mid', minWidth = 65, align = 'right'), 
      Mid_Per = colDef(name = 'Mid%', minWidth = 80, align = 'left', style = JS("function(rowInfo, column, state) {
        const { showColors, mid_colors } = state.meta
        if (showColors) {
          return { backgroundColor: mid_colors[rowInfo.index] }
        }
      }")),
      Three = colDef(name = '3pt', minWidth = 60, align = 'right'),
      Three_Per = colDef(name = '3pt%', minWidth = 70, align = 'left', style = JS("function(rowInfo, column, state) {
        const { showColors, three_colors } = state.meta
        if (showColors) {
          return { backgroundColor: three_colors[rowInfo.index] }
        }
      }")),
      Off_Reb = colDef(name = 'OReb', minWidth = 75, align = 'center'),
      Def_Reb = colDef(name = 'DReb', minWidth = 75, align = 'center'),
      Stl = colDef(name = 'Stl', minWidth = 60, align = 'center'),
      Blk = colDef(name = 'Blk', minWidth = 60, align = 'center'),
      Tov = colDef(name = 'Tov', minWidth = 60, align = 'center'),
      Charges = colDef(name = 'Charges', minWidth = 90, align = 'center'),
      Fouls = colDef(name = 'Fouls', minWidth = 70, align = 'center')
    ),
    rowStyle = function(row) {
      list(height = '45px')
    },
    meta = list(
      rim_colors = rim_colors,
      mid_colors = mid_colors,
      three_colors = three_colors,
      showColors = TRUE
    ),
    defaultPageSize = 16,
    elementId = 'practice-tbl'
  )
  return(complete_totals)
}

adv_reactable <- function(data) {
  rownames(data) <- NULL
  data$FG. <- as.numeric(data$FG.)
  data <- data[,-1]
  
  fg_normalized <- (data$FG. - min(data$FG.)) / 
    (max(data$FG.) - min(data$FG.))
  fg_normalized <- pmax(0, pmin(1, fg_normalized))
  fg_colors <- rgb(colorRamp(c("#DCF8C7", "#3E8F00"))(fg_normalized), maxColorValue = 255)
  
  efg_normalized <- (data$eFG - min(data$eFG)) / 
    (max(data$eFG) - min(data$eFG))
  efg_normalized <- pmax(0, pmin(1, efg_normalized))
  efg_colors <- rgb(colorRamp(c("#DCF8C7", "#3E8F00"))(efg_normalized), maxColorValue = 255)
  
  advanced_tbl <- reactable(
    data,
    striped = TRUE,
    highlight = TRUE,
    bordered = FALSE,
    theme = reactableTheme(
      stripedColor = '#f0f0f0',
      backgroundColor = '#E3E3E3',
      borderColor = 'black'
    ),
    columns = list(
      urls = colDef(name = '', cell = function(value) {
        div(style = "text-align: center;", tags$img(src = value, width = "35px", height = "auto"))
      },minWidth = 50, sticky = 'left'),
      Player = colDef(name = 'Player', minWidth = 175, filterable = TRUE, sticky = 'left'), 
      Pts = colDef(name = 'Pts', minWidth = 60, align = 'center'),
      FG. = colDef(name = 'FG%', minWidth = 80,align = 'center', style = JS("function(rowInfo, column, state) {
          const { showColors, fg_colors } = state.meta
          if (showColors) {
            return { backgroundColor: fg_colors[rowInfo.index] }
          }
        }")),
      eFG = colDef(name = 'eFG%', minWidth = 80,align = 'center', style = JS("function(rowInfo, column, state) {
          const { showColors, efg_colors } = state.meta
          if (showColors) {
            return { backgroundColor: efg_colors[rowInfo.index] }
          }
        }")),
      PPS = colDef(name = 'PPS', minWidth = 75, align = 'center'),
      Rim = colDef(name = 'Rim', minWidth = 80, align = 'center'),
      Mid = colDef(name = 'Mid', minWidth = 80, align = 'center'),
      Three = colDef(name = '3pt', minWidth = 80, align = 'center'),
      Rim_par = colDef(name = 'Rim', minWidth = 75, align = 'center'),
      Mid_par = colDef(name = 'Mid', minWidth = 75, align = 'center'),
      Three_par = colDef(name = '3pt', minWidth = 75, align = 'center')
    ),
    rowStyle = function(row) {
      list(height = '45px')
    },
    meta = list(fg_colors = fg_colors,
                efg_colors = efg_colors,
                showColors = TRUE),
    defaultPageSize = 16,
    elementId = 'practice-tbl',
    columnGroups = list(
      colGroup(name = 'Attempts per 100 FGA', columns = c('Rim_par', 'Mid_par', 'Three_par'))
    )
  )
  return(advanced_tbl)
}

per70_reactable <- function(data) {
  rownames(data) <- NULL
  data <- data[,-1]
  
  rim_normalized <- (data$Rim_Per - min(data$Rim_Per)) / 
    (max(data$Rim_Per) - min(data$Rim_Per))
  rim_normalized <- pmax(0, pmin(1, rim_normalized))
  rim_colors <- rgb(colorRamp(c("#DCF8C7", "#3E8F00"))(rim_normalized), maxColorValue = 255)
  
  mid_normalized <- (data$Mid_Per - min(data$Mid_Per)) / 
    (max(data$Mid_Per) - min(data$Mid_Per))
  mid_normalized <- pmax(0, pmin(1, mid_normalized))
  mid_colors <- rgb(colorRamp(c("#DCF8C7", "#3E8F00"))(mid_normalized), maxColorValue = 255)
  
  three_normalized <- (data$Three_Per - min(data$Three_Per)) / 
    (max(data$Three_Per) - min(data$Three_Per))
  three_normalized <- pmax(0, pmin(1, three_normalized))
  three_colors <- rgb(colorRamp(c("#DCF8C7", "#3E8F00"))(three_normalized), maxColorValue = 255)
  
  per70_tbl <- reactable(
    data,
    striped = TRUE,
    highlight = TRUE,
    bordered = FALSE,
    theme = reactableTheme(
      stripedColor = '#f0f0f0',
      backgroundColor = '#E3E3E3',
      borderColor = 'black'
    ),
    columns = list(
      urls = colDef(name = '', cell = function(value) {
        div(style = "text-align: center;", tags$img(src = value, width = "35px", height = "auto"))
      },minWidth = 50, sticky = 'left'),
      Player = colDef(name = 'Player', minWidth = 175, filterable = TRUE, sticky = 'left'), 
      Pts = colDef(name = 'Pts', minWidth = 60, align = 'center'), 
      Reb = colDef(name = 'Reb', minWidth = 60, align = 'center'), 
      Ast = colDef(name = 'Ast', minWidth = 60, align = 'center'), 
      Rim = colDef(name = 'Rim', minWidth = 80, align = 'right'), 
      Rim_Per = colDef(name = 'Rim%', minWidth = 80,align = 'left', style = JS("function(rowInfo, column, state) {
          const { showColors, rim_colors } = state.meta
          if (showColors) {
            return { backgroundColor: rim_colors[rowInfo.index] }
          }
        }")),
      Mid = colDef(name = 'Mid', minWidth = 80, align = 'right'), 
      Mid_Per = colDef(name = 'Mid%', minWidth = 80, align = 'left', style = JS("function(rowInfo, column, state) {
          const { showColors, mid_colors } = state.meta
          if (showColors) {
            return { backgroundColor: mid_colors[rowInfo.index] }
          }
        }")),
      Three = colDef(name = '3pt', minWidth = 80, align = 'right'),
      Three_Per = colDef(name = '3pt%', minWidth = 70, align = 'left', style = JS("function(rowInfo, column, state) {
          const { showColors, three_colors } = state.meta
          if (showColors) {
            return { backgroundColor: three_colors[rowInfo.index] }
          }
        }")),
      Off_Reb = colDef(name = 'OReb', minWidth = 75, align = 'center'),
      Def_Reb = colDef(name = 'DReb', minWidth = 75, align = 'center'),
      Stl = colDef(name = 'Stl', minWidth = 60, align = 'center'),
      Blk = colDef(name = 'Blk', minWidth = 60, align = 'center'),
      Tov = colDef(name = 'Tov', minWidth = 60, align = 'center'),
      Charges = colDef(name = 'Charges', minWidth = 90, align = 'center'),
      Fouls = colDef(name = 'Fouls', minWidth = 70, align = 'center')
    ),
    rowStyle = function(row) {
      list(height = '45px')
    },
    meta = list(rim_colors = rim_colors,
                mid_colors = mid_colors,
                three_colors = three_colors,
                showColors = TRUE),
    defaultPageSize = 16,
    elementId = 'practice-tbl'
  )
  return(per70_tbl)
}

fix_line_chart <- function(df) {
  df <- df %>%
    rename(
      'Off. Reb' = 'Off..Reb',
      'Def. Reb' = 'Def..Reb',
      'Rim Makes' = 'Rim.Makes',
      'Mid Makes' = 'Mid.Makes',
      '3pt Makes' = 'X3pt.Makes',
      'Rim Attempts' = 'Rim.Attempts',
      'Mid Attempts' = 'Mid.Attempts',
      '3pt Attempts' = 'X3pt.Attempts',
      'FG%' = 'FG.',
      'eFG%' = 'eFG.',
      'Rim%' = 'Rim.',
      'Mid%' = 'Mid.',
      '3pt%' = 'X3pt.',
      'Rim Attempts per 100 FGA' = 'Rim.Attempts.per.100.FGA',
      'Mid Attempts per 100 FGA' = 'Mid.Attempts.per.100.FGA',
      '3pt Attempts per 100 FGA' = 'X3pt.Attempts.per.100.FGA',
      'Ast per 100 Tov' = 'Ast.per.100.Tov',
      'Ast per 100 FGA' = 'Ast.per.100.FGA',
      'Points per Shot' = 'Points.per.Shot'
    )
}

radar_data1 <- function(player, total) {
  total <- total[,-1]
  total <- total[rowSums(total == 0) != ncol(total),]
  player <- player[,-1:-2]
  
  eff_stats <- player[,c(4,7,10,14,16,19)]
  eff_stats_comp <- total[,c(4,7,10,14,16,19)]
  
  averages <- round(colMeans(eff_stats_comp),1)
  
  plot_data <- data.frame(Stat = c('FG%', 'eFG%', 'Rim%', 'Mid%', '2pt%', '3pt%'),
                          Value = t(tail(eff_stats, 1)),
                          Avg = averages)
  colnames(plot_data)[2] = 'Value'
  
  return(plot_data)
}

radar_data2 <- function(player, total) {
  total <- total[,-1]
  total <- total[rowSums(total == 0) != ncol(total),]
  player <- player[,-1:-2]
  
  total <- total[order(total$Points), ]
  total$Pts_rk <- rank(total$Points)
  total <- total[order(total$Rebounds), ]
  total$Reb_rk <- rank(total$Rebounds)
  total <- total[order(total$Assists), ]
  total$Ast_rk <- rank(total$Assists)
  total <- total[order(total$Off..Reb), ]
  total$OReb_rk <- rank(total$Off..Reb)
  total <- total[order(total$Def..Reb), ]
  total$DReb_rk <- rank(total$Def..Reb)
  total <- total[order(total$Turnovers), ]
  total$Tov_rk <- rank(-total$Turnovers)
  
  player <- tail(player,1)
  select_data <- total %>%
    filter(Points == player$Points, Rebounds == player$Rebounds, Assists == player$Assists)
  
  reg_stats <- select_data[,c(31,32,34,35,33,36)]
  plot_data <- data.frame(Stat = c('Pts', 'Reb', 'OReb', 'DReb', 'Ast', 'Tov'),
                          Value = t(tail(reg_stats, 1))
  )
  colnames(plot_data)[2] = 'Value'
  
  return(plot_data)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = "sandstone",
  titlePanel(
    HTML('<div style="text-align: center;"><img src="https://pbs.twimg.com/media/F-NNaSUXkAAfv0K?format=jpg&name=large" alt="Your Image" width="1400" height="150" /></div>')),
  tags$style(HTML('body { background-color: #E3E3E3; }
                  .nav-tabs > li > a {
                  font-family: "Archivo Black";
                  font-size: 16px;
                  font-weight: bold;
                  color: #a4042c;}
                  .btn-action {
                  background-color: #a4042c;
                  color: #FFFFFF;
                  border-color: #000000;
                  font-weight: bold;}
                  .custom-background {
                  background-color: #c0c0c0;
                  padding: 10px;}
                  .button-container {
                  display: flex;
                  justify-content: left;}
                  .custom-button {
                  background-color: #000000;
                  color: #FFFFFF;
                  border-color: #FFFFFF;
                  font-weight: bold;}
                  .custom-background-two {
                  background-color: #dbdbdb;
                  padding: 10px;
                  width: 100%;}
                  .vertical-align {
                  display: flex;
                  flex-direction: column;
                  align-items: left;}
                  .custom-button-two {
                  background-color: #dbdbdb;
                  color: #000000;
                  border-color: #000000;
                  font-weight: bold;}')),
  tabsetPanel(
    tabPanel(
      'Reactable Tables',
      fluidRow(
        column(4, align = 'center',class = "custom-background", actionButton('DailyReactButton', 'Daily Tables', class = "btn-action")),
        column(4, align = 'center',class = "custom-background", actionButton('WeeklyReactButton', 'Weekly Tables', class = "btn-action")),
        column(4, align = 'center',class = "custom-background", actionButton('TotalReactButton', 'Grand Total Tables', class = "btn-action"))
      ),
      uiOutput('ReactableInputUI'),
      uiOutput('ReactableButtonsUI'),
      uiOutput('ReactableTables')
    ),
    tabPanel(
      "Individual Player Stats",
      fluidRow(
        column(6, align = 'center', class = 'custom-background', actionButton("lineChartsButton", "Line Charts", class = 'btn-action')),
        column(6, align = 'center', class = 'custom-background', actionButton("radarChartsButton", "Radar Charts", class = 'btn-action')),
      ),
      uiOutput("playerStatsUI"),
      uiOutput('tableOutputs')
    ),
    tabPanel(
      'Regular Printing Tables',
      fluidRow(
        column(4, align = 'center', class = 'custom-background', actionButton("DailyPrintButton", "Daily Tables", class = 'btn-action')),
        column(4, align = 'center', class = 'custom-background', actionButton("WeeklyPrintButton", "Weekly Tables", class = 'btn-action')),
        column(4, align = 'center', class = 'custom-background', actionButton('TotalPrintButton', 'Grand Total Tables', class = 'btn-action')),
      ),
      uiOutput("RegularPrintUI"),
      uiOutput('RegularPrintTableOutputs')
    ),
    tabPanel(
      "Frank's Tables",
      fluidRow(
        column(4, align = 'center', class = 'custom-background', actionButton("DailyFrankButton", "Daily Tables", class = 'btn-action')),
        column(4, align = 'center', class = 'custom-background', actionButton("WeeklyFrankButton", "Weekly Tables", class = 'btn-action')),
        column(4, align = 'center', class = 'custom-background', actionButton('TotalFrankButton', 'Grand Total Tables', class = 'btn-action')),
      ),
      uiOutput("FrankPrintUI"),
      uiOutput('FrankTableOutputs')
    ),
  )
)

server <- function(input, output) {
#----Reactable Table Session----------------------------------------------------
  observeEvent(input$DailyReactButton, {
    output$ReactableInputUI <- renderUI({
      fluidRow(
        column(6, class = 'custom-background-two vertical-align',
               dateInput("selectedDate6", "Select Date:", value = Sys.Date(), format = "mm-dd-yyyy")
        )
      )
    })
    output$ReactableButtonsUI <- renderUI({
      fluidRow(
        column(3, class = 'button-container',
               actionButton("basicTotalsButton1", "Basic Totals", class = 'custom-button')
        ),
        column(3, class = 'button-container',
               actionButton("advancedTotalsButton1", "Advanced Totals", class = 'custom-button')
        ),
        column(12, class = 'button-container',
               downloadButton("downloadDailyTable1", "Download Table", class = 'custom-button-two')
        )
      )
    })
    output$ReactableTables <- renderUI({
      reactableOutput('daily_tableOutput1')
    })
  })
  observeEvent(input$WeeklyReactButton, {
    output$ReactableInputUI <- renderUI({
      fluidRow(
        column(6, class = 'custom-background-two vertical-align',
               selectInput("selectedWeek5", "Select Week:",
                           choices = c("9.11 - 9.17", '9.18 - 9.24', '9.25 - 10.1', '10.2 - 10.8', '10.9 - 10.15', '10.16 - 10.22', '10.23 - 10.29', '10.30 - 11.05'), selected = "10.30 - 11.05")
        )
      )
    })
    output$ReactableButtonsUI <- renderUI({
      fluidRow(
        column(4, class = 'button-container',
               actionButton("actionButton4", "Regular", class = 'custom-button')
        ),
        column(4, class = 'button-container',
               actionButton("actionButton5", "Advanced", class = 'custom-button')
        ),
        column(4, class = 'button-container',
               actionButton("actionButton6", "Per 70", class = 'custom-button')
        ),
        column(3, class = 'button-container',
               downloadButton("downloadWeeklyTable1", "Download Table", class = 'custom-button-two')
        )
      )
    })
    output$ReactableTables <- renderUI({
      reactableOutput("weekly_tableOutput1")
    })
  })
  observeEvent(input$TotalReactButton, {
    output$ReactableInputUI <- renderUI({})
    output$ReactableButtonsUI <- renderUI({
      fluidRow(
        column(4, class = 'button-container',
               actionButton("grandTotalButton4", "Regular", class = 'custom-button')
        ),
        column(4, class = 'button-container',
               actionButton("grandTotalButton5", "Advanced", class = 'custom-button')
        ),
        column(4, class = 'button-container',
               actionButton("grandTotalButton6", "Per 70", class = 'custom-button')
        ),
        column(3, class = 'button-container',
               downloadButton("downloadGrandTotalTable1", "Download Table", class = 'custom-button-two')
        )
      )
    })
    output$ReactableTables <- renderUI({
      reactableOutput("grand_totals_tableOutput1")
    })
  })
#------Reactable Table Outputs--------------------------------------------------
  observeEvent(input$selectedDate6, {
    selected_date6 <- input$selectedDate6
    file_name1r <- paste0(format(selected_date6, '%m-%d-%Y'), ' | Practice Stats.csv')
    file_name2r <- paste0(format(selected_date6, '%m-%d-%Y'), ' | Practice Stats2.csv')
    if (file.exists(file_name1r)) {
      observeEvent(input$basicTotalsButton1, {
        data1r <- read.csv(file_name1r)
        output$daily_tableOutput1 <- renderReactable({
          DailyReactable(data1r)
        })
        data1_1r <- data1r
        data1_1r <- data1_1r[,-1:-2]
        colnames(data1_1r)[6] = 'Rim%'
        colnames(data1_1r)[8] = 'Mid%'
        colnames(data1_1r)[9] = '3pt'
        colnames(data1_1r)[10] = '3pt%'
        colnames(data1_1r)[11] = 'OReb'
        colnames(data1_1r)[12] = 'DReb'
        temp_file <- tempfile(fileext = ".csv")
        write.csv(data1_1r, temp_file, row.names = FALSE)
        output$downloadDailyTable1 <- downloadHandler(
          filename = function() {
            paste("DailyTable1_", format(Sys.time(), "%m-%d-%Y"), ".csv", sep = "")
          },
          content = function(file) {
            file.copy(temp_file, file)
          }
        )
      }
      )
    } else {
      observeEvent(input$basicTotalsButton1, {
        NoData <- read.csv('No Table - Sheet1.csv')
        output$daily_tableOutput1 <- renderReactable({
          reactable(
            NoData,
            theme = reactableTheme(
              backgroundColor = '#E3E3E3'
            ),
            columns = list(img = colDef(name = '', cell = function(value) {
              div(style = "text-align: center;", tags$img(src = value, width = "auto", height = "auto"))
            },minWidth = 50, sticky = 'left'))
          )
        })
      })
    }
    if (file.exists(file_name2r)) {
      observeEvent(input$advancedTotalsButton1, {
        data8r <- read.csv(file_name2r)
        output$daily_tableOutput1 <- renderReactable({
          AdvDailyReact(data8r)
        })
        data1_2r <- data8r
        data1_2r <- data1_2r[,-1:-2]
        colnames(data1_2r)[5] = 'FG%'
        colnames(data1_2r)[6] = 'eFG%'
        colnames(data1_2r)[7] = '2pt'
        colnames(data1_2r)[8] = '2pt%'
        colnames(data1_2r)[9] = '3pt'
        colnames(data1_2r)[10] = '3pt%'
        temp_file <- tempfile(fileext = ".csv")
        write.csv(data1_2r, temp_file, row.names = FALSE)
        output$downloadDailyTable1 <- downloadHandler(
          filename = function() {
            paste("DailyTable2_", format(Sys.time(), "%m-%d-%Y"), ".csv", sep = "")
          },
          content = function(file) {
            file.copy(temp_file, file)
          }
        )
      })
    } else {
      observeEvent(input$advancedTotalsButton1, {
        NoData <- read.csv('No Table - Sheet1.csv')
        output$daily_tableOutput1 <- renderReactable({
          reactable(
            NoData,
            theme = reactableTheme(
              backgroundColor = '#E3E3E3'
            ),
            columns = list(img = colDef(name = '', cell = function(value) {
              div(style = "text-align: center;", tags$img(src = value, width = "auto", height = "auto"))
            },minWidth = 50, sticky = 'left'))
          )
        })
      })
    }
  })
  
  observeEvent(input$selectedWeek5, {
    selected_week5 <- input$selectedWeek5
    file_name2_reg_r <- paste0(selected_week5, ' | Practice Stats.csv')
    file_name2_adv_r <- paste0(selected_week5, ' | Advanced Stats.csv')
    file_name2_70_r <- paste0(selected_week5, ' | Per70 Stats.csv')
    if (file.exists(file_name2_reg_r) & file.exists(file_name2_adv_r) & file.exists(file_name2_70_r)) {
      observeEvent(input$actionButton4, {
        data2_r <- read.csv(file_name2_reg_r)
        output$weekly_tableOutput1 <- renderReactable({
          reg_reactable(data2_r)
        })
        data2_1r <- data2_r[,-1:-2]
        colnames(data2_1r)[6] = 'Rim%'
        colnames(data2_1r)[8] = 'Mid%'
        colnames(data2_1r)[9] = '3pt'
        colnames(data2_1r)[10] = '3pt%'
        colnames(data2_1r)[11] = 'OReb'
        colnames(data2_1r)[12] = 'DReb'
        temp_file <- tempfile(fileext = ".csv")
        write.csv(data2_1r, temp_file, row.names = FALSE)
        output$downloadWeeklyTable1 <- downloadHandler(
          filename = function() {
            paste("WeeklyTable_", selected_week5, ".csv", sep = "")
          },
          content = function(file) {
            file.copy(temp_file, file)
          }
        )
      })
      observeEvent(input$actionButton5, {
        data3_r <- read.csv(file_name2_adv_r)
        output$weekly_tableOutput1 <- renderReactable({
          adv_reactable(data3_r)
        })
        data3_1r <- data3_r[,-1:-2]
        colnames(data3_1r)[3] = 'FG%'
        colnames(data3_1r)[4] = 'eFG%'
        colnames(data3_1r)[8] = '3pt'
        colnames(data3_1r)[9] = 'Rim Att. per 100 FGA'
        colnames(data3_1r)[10] = 'Mid Att. per 100 FGA'
        colnames(data3_1r)[11] = '3pt Att. per 100 FGA'
        temp_file <- tempfile(fileext = ".csv")
        write.csv(data3_1r, temp_file, row.names = FALSE)
        output$downloadWeeklyTable1 <- downloadHandler(
          filename = function() {
            paste("WeeklyAdvTable_", selected_week5, ".csv", sep = "")
          },
          content = function(file) {
            file.copy(temp_file, file)
          }
        )
      })
      observeEvent(input$actionButton6, {
        data4_r <- read.csv(file_name2_70_r)
        output$weekly_tableOutput1 <- renderReactable({
          per70_reactable(data4_r)
        })
        data4_1r <- data4_r[,-1:-2]
        colnames(data4_1r)[6] = 'Rim%'
        colnames(data4_1r)[8] = 'Mid%'
        colnames(data4_1r)[9] = '3pt'
        colnames(data4_1r)[10] = '3pt%'
        colnames(data4_1r)[11] = 'OReb'
        colnames(data4_1r)[12] = 'DReb'
        temp_file <- tempfile(fileext = ".csv")
        write.csv(data4_1r, temp_file, row.names = FALSE)
        output$downloadWeeklyTable1 <- downloadHandler(
          filename = function() {
            paste("WeeklyPer70Table_", selected_week5, ".csv", sep = "")
          },
          content = function(file) {
            file.copy(temp_file, file)
          }
        )
      })
    }
  })
  
  observeEvent(input$grandTotalButton4, {
    data5_r <- read.csv('Grand Total | Practice Stats.csv')
    output$grand_totals_tableOutput1 <- renderReactable({
      reg_reactable(data5_r)
    })
    data5_1r <- data5_r[,-1:-2]
    colnames(data5_1r)[6] = 'Rim%'
    colnames(data5_1r)[8] = 'Mid%'
    colnames(data5_1r)[9] = '3pt'
    colnames(data5_1r)[10] = '3pt%'
    colnames(data5_1r)[11] = 'OReb'
    colnames(data5_1r)[12] = 'DReb'
    temp_file <- tempfile(fileext = ".csv")
    write.csv(data5_1r, temp_file, row.names = FALSE)
    output$downloadGrandTotalTable1 <- downloadHandler(
      filename = function() {
        paste("TotalPracticeTable.csv", sep = "")
      },
      content = function(file) {
        file.copy(temp_file, file)
      }
    )
  })
  observeEvent(input$grandTotalButton5, {
    data6_r <- read.csv('Grand Total | Advanced Stats.csv')
    output$grand_totals_tableOutput1 <- renderReactable({
      adv_reactable(data6_r)
    })
    data6_1r <- data6_r[,-1:-2]
    colnames(data6_1r)[3] = 'FG%'
    colnames(data6_1r)[4] = 'eFG%'
    colnames(data6_1r)[8] = '3pt'
    colnames(data6_1r)[9] = 'Rim Att. per 100 FGA'
    colnames(data6_1r)[10] = 'Mid Att. per 100 FGA'
    colnames(data6_1r)[11] = '3pt Att. per 100 FGA'
    temp_file <- tempfile(fileext = ".csv")
    write.csv(data6_1r, temp_file, row.names = FALSE)
    output$downloadGrandTotalTable1 <- downloadHandler(
      filename = function() {
        paste("TotalAdvTable.csv", sep = "")
      },
      content = function(file) {
        file.copy(temp_file, file)
      }
    )
  })
  observeEvent(input$grandTotalButton6, {
    data7_r <- read.csv('Grand Total | Per70 Stats.csv')
    output$grand_totals_tableOutput1 <- renderReactable({
      per70_reactable(data7_r)
    })
    data7_1r <- data7_r[,-1:-2]
    colnames(data7_1r)[6] = 'Rim%'
    colnames(data7_1r)[8] = 'Mid%'
    colnames(data7_1r)[9] = '3pt'
    colnames(data7_1r)[10] = '3pt%'
    colnames(data7_1r)[11] = 'OReb'
    colnames(data7_1r)[12] = 'DReb'
    temp_file <- tempfile(fileext = ".csv")
    write.csv(data7_1r, temp_file, row.names = FALSE)
    output$downloadGrandTotalTable1 <- downloadHandler(
      filename = function() {
        paste("TotalPer70Table.csv", sep = "")
      },
      content = function(file) {
        file.copy(temp_file, file)
      }
    )
  })
#-------Regular Print Session---------------------------------------------------
  observeEvent(input$DailyPrintButton, {
    output$RegularPrintUI <- renderUI({
      fluidRow(
        column(6, class = 'custom-background-two vertical-align',
               dateInput("selectedDate4", "Select Date:", value = Sys.Date())
        )
      )
    })
    output$RegularPrintTableOutputs <- renderUI({
      mainPanel(
        plotOutput("playerStatsTable3"),
        plotOutput("playerStatsTable4")
      )
    })
  })
  observeEvent(input$WeeklyPrintButton, {
    output$RegularPrintUI <- renderUI({
      fluidRow(
        column(6, class = 'custom-background-two vertical-align',
               selectInput("selectedWeek4", "Select Week:", 
                           choices = c('10.2 - 10.8', '10.9 - 10.15', '10.16 - 10.22', '10.23 - 10.29', '10.30 - 11.05'), selected = '10.30 - 11.05')
        )
      )
    })
    output$RegularPrintTableOutputs <- renderUI({
      mainPanel(
        plotOutput("WeeklyTable4"),
        plotOutput("WeeklyTable5"),
        plotOutput('WeeklyTable6')
      )
    })
  })
  observeEvent(input$TotalPrintButton, {
    output$RegularPrintUI <- renderUI({})
    output$RegularPrintTableOutputs <- renderUI({
      mainPanel(
        plotOutput("TotalTable4"),
        plotOutput("TotalTable5"),
        plotOutput('TotalTable6')
      )
    })
  })
  #--------Regular Print Tab------------------------------------------------------
  observeEvent(input$selectedDate4, {
    selected_date4 <- input$selectedDate4
    file_name_p3 <- paste0(format(selected_date4, "%Y-%m-%d"), ' | Complete Practice Totals.png')
    file_name_p4 <- paste0(format(selected_date4, "%Y-%m-%d"), ' | Basic Practice Totals.png')
    if (file.exists(file_name_p3)) {
      output$playerStatsTable3 <- renderImage({
        list(src = file_name_p3,
             contentType = 'image/png',
             width = '80%',
             height = '100%')
      }, deleteFile = FALSE)
    } else {
      output$playerStatsTable3 <- renderImage({
        list(src = 'No.png',
             contentType = 'image/png',
             width = '100%',
             height = '100%')
      }, deleteFile = FALSE)
    }
    if (file.exists(file_name_p4)) {
      output$playerStatsTable4 <- renderImage({
        list(src = file_name_p4,
             contentType = 'image/png',
             width = '75%',
             height = '100%')
      }, deleteFile = FALSE)
    } else {
      output$playerStatsTable4 <- renderImage({
        list(src = 'No.png',
             contentType = 'image/png',
             width = '100%',
             height = '0%')
      }, deleteFile = FALSE)
    }
  })
  #--
  observeEvent(input$selectedWeek4, {
    selected_week4 <- input$selectedWeek4
    file_name_p4 <- paste0(selected_week4, ' - Complete Totals Stat Sheet.png')
    file_name_p5 <- paste0(selected_week4, ' - Basic Totals Stat Sheet.png')
    file_name_p6 <- paste0(selected_week4, ' - Advanced Totals Stat Sheet.png')
    if (file.exists(file_name_p4)) {
      output$WeeklyTable4 <- renderImage({
        list(src = file_name_p4,
             contentType = 'image/png',
             width = '70%',
             height = '100%')
      }, deleteFile = FALSE)
    } else {
      output$WeeklyTable4 <- renderImage({
        list(src = 'No.png',
             contentType = 'image/png',
             width = '100%',
             height = '100%')
      }, deleteFile = FALSE)
    }
    if (file.exists(file_name_p5)) {
      output$WeeklyTable5 <- renderImage({
        list(src = file_name_p5,
             contentType = 'image/png',
             width = '70%',
             height = '100%')
      }, deleteFile = FALSE)
    } else {
      output$WeeklyTable5 <- renderImage({
        list(src = 'No.png',
             contentType = 'image/png',
             width = '0%',
             height = '100%')
      }, deleteFile = FALSE)
    }
    if (file.exists(file_name_p6)) {
      output$WeeklyTable6 <- renderImage({
        list(src = file_name_p6,
             contentType = 'image/png',
             width = '70%',
             height = '100%')
      }, deleteFile = FALSE)
    } else {
      output$WeeklyTable6 <- renderImage({
        list(src = 'No.png',
             contentType = 'image/png',
             width = '0%',
             height = '100%')
      }, deleteFile = FALSE)
    }
  })
  #--
  output$TotalTable4 <- renderImage({
    list(src = 'Full Season - Complete Totals Stat Sheet.png',
         contentType = 'image/png',
         width = '60%',
         height = '100%')
  }, deleteFile = FALSE)
  output$TotalTable5 <- renderImage({
    list(src = 'Full Season - Basic Totals Stat Sheet.png',
         contentType = 'image/png',
         width = '70%',
         height = '100%')
  }, deleteFile = FALSE)
  output$TotalTable6 <- renderImage({
    list(src = 'Full Season - Advanced Totals Stat Sheet.png',
         contentType = 'image/png',
         width = '60%',
         height = '100%')
  }, deleteFile = FALSE)
#------Frank Tables Session-----------------------------------------------------
  observeEvent(input$DailyFrankButton, {
    output$FrankPrintUI <- renderUI({
      fluidRow(
        column(6, class = 'custom-background-two vertical-align',
               dateInput("selectedDate5", "Select Date:", value = Sys.Date())
        )
      )
    })
    output$FrankTableOutputs <- renderUI({
      plotOutput('FrankTable1', height = '500px', width = '750px')
    })
  })
  observeEvent(input$WeeklyFrankButton, {
    output$FrankPrintUI <- renderUI({
      fluidRow(
        column(6, class = 'custom-background-two vertical-align',
               selectInput("selectedWeek4", "Select Week:",
                           choices = c('10.16 - 10.22', '10.23 - 10.29', '10.30 - 11.05'), selected = '10.30 - 11.05')
        )
      )
    })
    output$FrankTableOutputs <- renderUI({
      plotOutput('WeeklyFrank1', height = '500px', width = '750px')
    })
  })
  observeEvent(input$TotalFrankButton, {
    output$FrankPrintUI <- renderUI({})
    output$FrankTableOutputs <- renderUI({
      plotOutput('TotalFrank1', height = '500px', width = '750px')
    })
  })
#-------Frank Tables Tab--------------------------------------------------------
  observeEvent(input$selectedDate5, {
    selected_date5 <- input$selectedDate5
    file_name_frank1 <- paste0(format(selected_date5, "%Y-%m-%d"), ' | Frank Totals.png')
    if (file.exists(file_name_frank1)) {
      output$FrankTable1 <- renderImage({
        list(src = file_name_frank1,
             contentType = 'image/png',
             width = '65%',
             height = '100%')
      }, deleteFile = FALSE)
    } else {
      output$FrankTable1 <- renderImage({
        list(src = 'No.png',
             contentType = 'image/png',
             width = '100%',
             height = '100%')
      }, deleteFile = FALSE)
    }
  })
  
  observeEvent(input$selectedWeek4, {
    selected_week4 <- input$selectedWeek4
    weekly_frank_file1 <- paste0(selected_week4, ' - Frank Weekly Stat Sheet.png')
    if (file.exists(weekly_frank_file1)) {
      output$WeeklyFrank1 <- renderImage({
        list(src = weekly_frank_file1,
             contentType = 'image/png',
             width = '65%',
             height = '100%')
      }, deleteFile = FALSE)
    } else {
      output$WeeklyFrank1 <- renderImage({
        list(src = 'No.png',
             contentType = 'image/png',
             width = '100%',
             height = '100%')
      }, deleteFile = FALSE)
    }
  })
  
  output$TotalFrank1 <- renderImage({
    list(src = 'Full Season - Frank Total Stat Sheet.png',
         contentType = 'image/png',
         width = '65%',
         height = '100%')
  }, deleteFile = FALSE)
  #---------Player Stats Session--------------------------------------------------
  observeEvent(input$lineChartsButton, {
    output$playerStatsUI <- renderUI({
      fluidRow(
        column(6, class = 'custom-background-two vertical-align button-container',
               selectInput("playerName", "Select Player:", choices = c('Full Team', 'Rollie Castineyra', 'Josh Cohen', 'Jackson Cronin', 'Matt Cross', 'Jaylen Curry', 'Robert Davis Jr.', 'Rahsool Diggins', 'Tarique Foster', 'Daniel Hankins-Sanford', 'Mathok Majok', 'Ryan Marcus', 'Sawyer Mayhugh', 'Jayden Ndjigue', 'Keon Thompson', 'Marqui Worthy'), selected = 'Full Team')
        ),
        column(6, class = 'custom-background-two vertical-align button-container',
               selectInput("statType", "Select Stat to Display:", choices = c('Points', 'Rebounds', 'Off. Reb', 'Def. Reb', 'Assists', 'FG%', 'FGA', 'FGM', 'eFG%', 'Rim%', 'Rim Makes', 'Rim Attempts', 'Rim Attempts per 100 FGA', 'Mid%', 'Mid Makes', 'Mid Attempts', 'Mid Attempts per 100 FGA', '2pt%', '3pt%', '3pt Makes', '3pt Attempts', '3pt Attempts per 100 FGA', 'Steals', 'Blocks', 'Turnovers', 'Charges', 'Fouls', 'Ast per 100 Tov', 'Ast per 100 FGA', 'Points per Shot'), selected = 'Points')
        )
      )
    })
    output$tableOutputs <- renderUI({
      plotOutput('playerstats_line_chart')
    })
  })
  
  observeEvent(input$radarChartsButton, {
    output$playerStatsUI <- renderUI({
      fluidRow(
        column(6, class = 'custom-background-two vertical-align',
               selectInput("playerNameRadar", "Select Player:", choices = c('Rollie Castineyra', 'Josh Cohen', 'Jackson Cronin', 'Matt Cross', 'Jaylen Curry', 'Robert Davis Jr.', 'Rahsool Diggins', 'Tarique Foster', 'Daniel Hankins-Sanford', 'Mathok Majok', 'Ryan Marcus', 'Sawyer Mayhugh', 'Jayden Ndjigue', 'Keon Thompson', 'Marqui Worthy'), selected = 'Rollie Castineyra')
        )
      )
    })
    output$tableOutputs <- renderUI({
      fluidRow(
        column(6, plotOutput('radar3', height = "500px", width = '441px')),
        column(6, plotOutput('radar4', height = "487px", width = '441px'))
      )
    })
  })
  #------Player Stats Graphs------------------------------------------------------
  observeEvent(input$playerName, {
    player_selected_n <- input$playerName
    file_name_n <- paste0(player_selected_n, ' | Practice Progress.csv')
    data_line_n <- read.csv(file_name_n)
    data_line_n <- fix_line_chart(data_line_n)
    output$playerstats_line_chart <- renderPlot({
      selected_stat_n <- input$statType
      if (!is.null(selected_stat_n)) {
        df_n <- data_line_n %>%
          select(Practice, all_of(selected_stat_n))
        ggplot(df_n, aes(x = Practice)) +
          geom_line(aes(y = .data[[selected_stat_n]])) +
          geom_point(aes(y = .data[[selected_stat_n]]), color = "black", size = 2) +
          labs(title = paste0(player_selected_n, ' | Cumulative Practice Progress'), y = 'Value', x = "Practice #", subtitle = selected_stat_n) +
          scale_color_manual(values = "black") +
          theme_minimal() +
          theme(
            text = element_text(size = 12.5),
            axis.text = element_text(size = 14),
            axis.title.x = element_text(size = 15, face = 'bold'),
            axis.title.y = element_text(size = 15, face = 'bold'),
            axis.line = element_line(size = 1),
            plot.title = element_text(size = 17.5, face = 'bold', hjust = 0.5),
            plot.subtitle = element_text(size = 17.5, face = 'bold', hjust = 0.5),
            panel.grid = element_line(colour = 'gray80', linewidth = 0.5)
          )
      }
    })
  })
  
  observeEvent(input$playerNameRadar, {
    radar_player_n <- input$playerNameRadar
    full_radar_n <- read.csv('Players | Practice Totals.csv')
    radar_file_n <- paste0(radar_player_n, ' | Practice Progress.csv')
    radar_data_n <- read.csv(radar_file_n)
    output$radar3 <- renderPlot({
      plot_data3 <- radar_data2(radar_data_n, full_radar_n)
      radar_chart3 <- ggplot(plot_data3, aes(x = Stat, y = Value)) +
        geom_col(aes(fill = Stat), width = 1, alpha = 0.9, color = 'white') +
        coord_polar(start = -pi/2, clip = "off")
      radar_chart3 <- radar_chart3 +
        theme_minimal() +
        theme(panel.grid.major = element_line(color = "white", size = 0.5),
              plot.background = element_rect(fill = "gray89", color = 'gray89'),
              legend.position = "none",
              axis.text.x = element_text(size = 12, face = "bold", color = "black"),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.y = element_blank()
        ) + 
        labs(title = paste0(radar_player_n, ' | Total Stats'),
             subtitle = paste0("Shaded areas are in comparison to rest of team. \nLarger the area, better they are performing compared to rest of team.")
        ) + 
        theme(
          plot.title = element_text(hjust = 0.5, size = 20, face = "bold", color = 'black'),
          plot.subtitle = element_text(color = 'black', size = 12, hjust = 0.5)
        ) +
        scale_fill_manual(values = c('#971B2F', '#971B2F', '#971B2F', '#971B2F', '#971B2F', '#971B2F'), guide = 'none')
      print(radar_chart3)
    })
    output$radar4 <- renderPlot({
      plot_data4 <- radar_data1(radar_data_n, full_radar_n)
      radar_chart4 <- ggplot(plot_data4, aes(x = Stat, y = Value)) +
        geom_col(aes(fill = Stat), width = 1, alpha = 0.9, color = 'white') +
        geom_point(aes(y = Avg), size = 3, color = "black") +
        geom_segment(aes(x = Stat, y = 0, xend = Stat, yend = Avg), linetype = "dashed", color = "black") +
        coord_polar(start = -pi/2, clip = "off")
      radar_chart4 <- radar_chart4 +
        theme_minimal() +
        theme(
          panel.grid.major = element_line(color = "white", size = 0.5),
          plot.background = element_rect(fill = "gray89", color = 'gray89'),
          legend.position = "none",
          axis.text.x = element_text(size = 12, face = "bold", color = "black"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank()
        ) + 
        labs(title = paste0(radar_player_n, ' | Shooting Efficiency'),
             subtitle = paste0("Dotted line represents average amongst team throughout practice.")
        ) + 
        theme(
          plot.title = element_text(hjust = 0.5, size = 20, face = "bold", color = 'black'),
          plot.subtitle = element_text(color = 'black', size = 12, hjust = 0.5)
        ) +
        scale_fill_manual(values = c('#971B2F', '#971B2F', '#971B2F', '#971B2F', '#971B2F', '#971B2F'), guide = 'none')
      print(radar_chart4)
    })
  })
}

shinyApp(ui, server)