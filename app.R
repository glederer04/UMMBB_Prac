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
  titlePanel("UMass MBB Practice"),
  tags$style(HTML("body { background-color: #E3E3E3; }")),
  tabsetPanel(
    tabPanel("Daily Tables",
             fluidRow(
               column(6,
                      dateInput("selectedDate", "Select Date:", value = Sys.Date(), format = "mm-dd-yyyy")
               ),
               column(3,
                      actionButton("basicTotalsButton", "Basic Totals")
               ),
               column(3,
                      actionButton("advancedTotalsButton", "Advanced Totals")
               ),
               #--
               column(3,
                      downloadButton("downloadDailyTable", "Download Table")
               )
               #--
             ),
             reactableOutput('daily_tableOutput')
    ),
    tabPanel("Weekly Tables",
             selectInput("selectedWeek", "Select Week:",
                         choices = c("9.11 - 9.17", '9.18 - 9.24', '9.25 - 10.1', '10.2 - 10.8', '10.9 - 10.15'), selected = "10.9 - 10.15"
             ),
             fluidRow(
               column(4,
                      actionButton("actionButton1", "Regular")
               ),
               column(4,
                      actionButton("actionButton2", "Advanced")
               ),
               column(4,
                      actionButton("actionButton3", "Per 70")
               ),
               #--
               column(3,
                      downloadButton("downloadWeeklyTable", "Download Table")
                      )
               #--
             ),
             reactableOutput("weekly_tableOutput")
    ),
    tabPanel("Grand Totals",
             fluidRow(
               column(4,
                      actionButton("grandTotalButton1", "Regular")
               ),
               column(4,
                      actionButton("grandTotalButton2", "Advanced")
               ),
               column(4,
                      actionButton("grandTotalButton3", "Per 70")
               ),
               #--
               column(3,
                      downloadButton("downloadGrandTotalTable", "Download Table")
                      )
               #--
             ),
             reactableOutput("grand_totals_tableOutput")
    ),
    tabPanel(
      "Daily Printing Tables",
      sidebarLayout(
        sidebarPanel(
          div(
            dateInput("selectedDate2", "Select Date:", value = Sys.Date())
          )
        ),
        mainPanel(
          plotOutput("playerStatsTable"),
          plotOutput("playerStatsTable2")
        )
      )
    ),
    tabPanel(
      "Weekly Printing Tables",
      sidebarLayout(
        sidebarPanel(
          div(
            selectInput("selectedWeek2", "Select Week:", 
                        choices = c('10.2 - 10.8', '10.9 - 10.15'), selected = '10.9 - 10.15')
          )
        ),
        mainPanel(
          plotOutput("WeeklyTable1"),
          plotOutput("WeeklyTable2"),
          plotOutput('WeeklyTable3')
        )
      )
    ),
    tabPanel(
      "Total Printing Tables",
      mainPanel(
        plotOutput("TotalTable1"),
        plotOutput("TotalTable2"),
        plotOutput('TotalTable3')
      )
    ),
    tabPanel("Player Line Graphs",
             fluidRow(
               column(6,
                      selectInput("player", "Select a Player:", 
                                  choices = c('Full Team', 'Rollie Castineyra', 'Josh Cohen', 'Jackson Cronin', 'Matt Cross', 'Jaylen Curry', 'Robert Davis Jr.', 'Rahsool Diggins', 'Tarique Foster', 'Daniel Hankins-Sanford', 'Mathok Majok', 'Ryan Marcus', 'Sawyer Mayhugh', 'Jayden Ndjigue', 'Keon Thompson', 'Marqui Worthy'), selected = 'Full Team'),
               ),
               column(6,
                      selectInput("selected_stat", "Select Stat to Display:",
                                  choices = c('Points', 'Rebounds', 'Off. Reb', 'Def. Reb', 'Assists', 'FG%', 'FGA', 'FGM', 'eFG%', 'Rim%', 'Rim Makes', 'Rim Attempts', 'Rim Attempts per 100 FGA', 'Mid%', 'Mid Makes', 'Mid Attempts', 'Mid Attempts per 100 FGA', '2pt%', '3pt%', '3pt Makes', '3pt Attempts', '3pt Attempts per 100 FGA', 'Steals', 'Blocks', 'Turnovers', 'Charges', 'Fouls', 'Ast per 100 Tov', 'Ast per 100 FGA', 'Points per Shot'), selected = 'Points')
               )
             ),
             fluidRow(
               column(12,
                      plotOutput("line_chart")
                      )
               )
             ),
    tabPanel('Radar Charts',
             selectInput("radar_player", "Select a Player:", 
                         choices = c('Rollie Castineyra', 'Josh Cohen', 'Jackson Cronin', 'Matt Cross', 'Jaylen Curry', 'Robert Davis Jr.', 'Rahsool Diggins', 'Tarique Foster', 'Daniel Hankins-Sanford', 'Mathok Majok', 'Ryan Marcus', 'Sawyer Mayhugh', 'Jayden Ndjigue', 'Keon Thompson', 'Marqui Worthy'), selected = 'Rollie Castineyra'),
             mainPanel(
               fluidRow(
                 column(6, plotOutput('radar1', height = "500px", width = '441px')),
                 column(6, plotOutput('radar2', height = "487px", width = '441px'))
               )
             )
    ),
    tabPanel(
      "Frank Daily Tables",
      sidebarLayout(
        sidebarPanel(
          div(
            dateInput("selectedDate3", "Select Date:", value = Sys.Date())
          )
        ),
        mainPanel(
          plotOutput("FrankTable")
        )
      )
    ),
    # tabPanel(
    #   "Frank Weekly Tables",
    #   sidebarLayout(
    #     sidebarPanel(
    #       div(
    #         selectInput("selectedWeek3", "Select Week:", 
    #                     choices = c('10.16 - 10.22'), selected = '10.16 - 10.22')
    #       )
    #     ),
    #     mainPanel(
    #       plotOutput("WeeklyFrank")
    #     )
    #   )
    # ),
    tabPanel(
      "Frank Total Tables",
      mainPanel(
        plotOutput("TotalFrank")
      )
    )
  )
)

server <- function(input, output) {
  #------daily--------------------------------------------------------------------
  observeEvent(input$selectedDate, {
    selected_date <- input$selectedDate
    file_name1 <- paste0(format(selected_date, '%m-%d-%Y'), ' | Practice Stats.csv')
    file_name2 <- paste0(format(selected_date, '%m-%d-%Y'), ' | Practice Stats2.csv')
    if (file.exists(file_name1)) {
      observeEvent(input$basicTotalsButton, {
        data1 <- read.csv(file_name1)
        output$daily_tableOutput <- renderReactable({
          DailyReactable(data1)
        })
        #--
        data1_1 <- data1
        data1_1 <- data1_1[,-1:-2]
        colnames(data1_1)[6] = 'Rim%'
        colnames(data1_1)[8] = 'Mid%'
        colnames(data1_1)[9] = '3pt'
        colnames(data1_1)[10] = '3pt%'
        colnames(data1_1)[11] = 'OReb'
        colnames(data1_1)[12] = 'DReb'
        temp_file <- tempfile(fileext = ".csv")
        write.csv(data1_1, temp_file, row.names = FALSE)
        output$downloadDailyTable <- downloadHandler(
          filename = function() {
            paste("DailyTable1_", format(Sys.time(), "%m-%d-%Y"), ".csv", sep = "")
          },
          content = function(file) {
            file.copy(temp_file, file)
          }
        )
        #--
      }
      )
    } else {
      observeEvent(input$basicTotalsButton, {
        NoData <- read.csv('No Table - Sheet1.csv')
        output$daily_tableOutput <- renderReactable({
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
    if (file.exists(file_name2)) {
      observeEvent(input$advancedTotalsButton, {
        data8 <- read.csv(file_name2)
        output$daily_tableOutput <- renderReactable({
          AdvDailyReact(data8)
        })
        #--
        data1_2 <- data8
        data1_2 <- data1_2[,-1:-2]
        colnames(data1_2)[5] = 'FG%'
        colnames(data1_2)[6] = 'eFG%'
        colnames(data1_2)[7] = '2pt'
        colnames(data1_2)[8] = '2pt%'
        colnames(data1_2)[9] = '3pt'
        colnames(data1_2)[10] = '3pt%'
        temp_file <- tempfile(fileext = ".csv")
        write.csv(data1_2, temp_file, row.names = FALSE)
        output$downloadDailyTable <- downloadHandler(
          filename = function() {
            paste("DailyTable2_", format(Sys.time(), "%m-%d-%Y"), ".csv", sep = "")
          },
          content = function(file) {
            file.copy(temp_file, file)
          }
        )
        #--
      })
    } else {
      observeEvent(input$advancedTotalsButton, {
        NoData <- read.csv('No Table - Sheet1.csv')
        output$daily_tableOutput <- renderReactable({
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
  #---------Weekly----------------------------------------------------------------
  observeEvent(input$selectedWeek, {
    selected_week <- input$selectedWeek
    file_name2_reg <- paste0(selected_week, ' | Practice Stats.csv')
    file_name2_adv <- paste0(selected_week, ' | Advanced Stats.csv')
    file_name2_70 <- paste0(selected_week, ' | Per70 Stats.csv')
    if (file.exists(file_name2_reg) & file.exists(file_name2_adv) & file.exists(file_name2_70)) {
      observeEvent(input$actionButton1, {
        data2 <- read.csv(file_name2_reg)
        output$weekly_tableOutput <- renderReactable({
          reg_reactable(data2)
        })
        #--
        data2_1 <- data2[,-1:-2]
        colnames(data2_1)[6] = 'Rim%'
        colnames(data2_1)[8] = 'Mid%'
        colnames(data2_1)[9] = '3pt'
        colnames(data2_1)[10] = '3pt%'
        colnames(data2_1)[11] = 'OReb'
        colnames(data2_1)[12] = 'DReb'
        temp_file <- tempfile(fileext = ".csv")
        write.csv(data2_1, temp_file, row.names = FALSE)
        output$downloadWeeklyTable <- downloadHandler(
          filename = function() {
            paste("WeeklyTable_", selected_week, ".csv", sep = "")
          },
          content = function(file) {
            file.copy(temp_file, file)
          }
        )
        #--
      })
      observeEvent(input$actionButton2, {
        data3 <- read.csv(file_name2_adv)
        output$weekly_tableOutput <- renderReactable({
          adv_reactable(data3)
        })
        #--
        data3_1 <- data3[,-1:-2]
        colnames(data3_1)[3] = 'FG%'
        colnames(data3_1)[4] = 'eFG%'
        colnames(data3_1)[8] = '3pt'
        colnames(data3_1)[9] = 'Rim Att. per 100 FGA'
        colnames(data3_1)[10] = 'Mid Att. per 100 FGA'
        colnames(data3_1)[11] = '3pt Att. per 100 FGA'
        temp_file <- tempfile(fileext = ".csv")
        write.csv(data3_1, temp_file, row.names = FALSE)
        output$downloadWeeklyTable <- downloadHandler(
          filename = function() {
            paste("WeeklyAdvTable_", selected_week, ".csv", sep = "")
          },
          content = function(file) {
            file.copy(temp_file, file)
          }
        )
        #--
      })
      observeEvent(input$actionButton3, {
        data4 <- read.csv(file_name2_70)
        output$weekly_tableOutput <- renderReactable({
          per70_reactable(data4)
        })
        #--
        data4_1 <- data4[,-1:-2]
        colnames(data4_1)[6] = 'Rim%'
        colnames(data4_1)[8] = 'Mid%'
        colnames(data4_1)[9] = '3pt'
        colnames(data4_1)[10] = '3pt%'
        colnames(data4_1)[11] = 'OReb'
        colnames(data4_1)[12] = 'DReb'
        temp_file <- tempfile(fileext = ".csv")
        write.csv(data4_1, temp_file, row.names = FALSE)
        output$downloadWeeklyTable <- downloadHandler(
          filename = function() {
            paste("WeeklyPer70Table_", selected_week, ".csv", sep = "")
          },
          content = function(file) {
            file.copy(temp_file, file)
          }
        )
        #--
      })
    }
  })
  #-----Grand Totals--------------------------------------------------------------
  observeEvent(input$grandTotalButton1, {
    data5 <- read.csv('Grand Total | Practice Stats.csv')
    output$grand_totals_tableOutput <- renderReactable({
      reg_reactable(data5)
    })
    #--
    data5_1 <- data5[,-1:-2]
    colnames(data5_1)[6] = 'Rim%'
    colnames(data5_1)[8] = 'Mid%'
    colnames(data5_1)[9] = '3pt'
    colnames(data5_1)[10] = '3pt%'
    colnames(data5_1)[11] = 'OReb'
    colnames(data5_1)[12] = 'DReb'
    temp_file <- tempfile(fileext = ".csv")
    write.csv(data5_1, temp_file, row.names = FALSE)
    output$downloadGrandTotalTable <- downloadHandler(
      filename = function() {
        paste("TotalPracticeTable.csv", sep = "")
      },
      content = function(file) {
        file.copy(temp_file, file)
      }
    )
    #--
  })
  observeEvent(input$grandTotalButton2, {
    data6 <- read.csv('Grand Total | Advanced Stats.csv')
    output$grand_totals_tableOutput <- renderReactable({
      adv_reactable(data6)
    })
    #--
    data6_1 <- data6[,-1:-2]
    colnames(data6_1)[3] = 'FG%'
    colnames(data6_1)[4] = 'eFG%'
    colnames(data6_1)[8] = '3pt'
    colnames(data6_1)[9] = 'Rim Att. per 100 FGA'
    colnames(data6_1)[10] = 'Mid Att. per 100 FGA'
    colnames(data6_1)[11] = '3pt Att. per 100 FGA'
    temp_file <- tempfile(fileext = ".csv")
    write.csv(data6_1, temp_file, row.names = FALSE)
    output$downloadGrandTotalTable <- downloadHandler(
      filename = function() {
        paste("TotalAdvTable.csv", sep = "")
      },
      content = function(file) {
        file.copy(temp_file, file)
      }
    )
    #--
  })
  observeEvent(input$grandTotalButton3, {
    data7 <- read.csv('Grand Total | Per70 Stats.csv')
    output$grand_totals_tableOutput <- renderReactable({
      per70_reactable(data7)
    })
    #--
    data7_1 <- data7[,-1:-2]
    colnames(data7_1)[6] = 'Rim%'
    colnames(data7_1)[8] = 'Mid%'
    colnames(data7_1)[9] = '3pt'
    colnames(data7_1)[10] = '3pt%'
    colnames(data7_1)[11] = 'OReb'
    colnames(data7_1)[12] = 'DReb'
    temp_file <- tempfile(fileext = ".csv")
    write.csv(data7_1, temp_file, row.names = FALSE)
    output$downloadGrandTotalTable <- downloadHandler(
      filename = function() {
        paste("TotalPer70Table.csv", sep = "")
      },
      content = function(file) {
        file.copy(temp_file, file)
      }
    )
    #--
  })
  #-------Print Daily-----------------------------------------------------------
  observeEvent(input$selectedDate2, {
    selected_date2 <- input$selectedDate2
    file_name_p1 <- paste0(format(selected_date2, "%Y-%m-%d"), ' | Complete Practice Totals.png')
    file_name_p2 <- paste0(format(selected_date2, "%Y-%m-%d"), ' | Basic Practice Totals.png')
    if (file.exists(file_name_p1)) {
      output$playerStatsTable <- renderImage({
        list(src = file_name_p1,
             contentType = 'image/png',
             width = '80%',
             height = '100%')
      }, deleteFile = FALSE)
    } else {
      output$playerStatsTable <- renderImage({
        list(src = 'No.png',
             contentType = 'image/png',
             width = '100%',
             height = '100%')
      }, deleteFile = FALSE)
    }
    if (file.exists(file_name_p2)) {
      output$playerStatsTable2 <- renderImage({
        list(src = file_name_p2,
             contentType = 'image/png',
             width = '75%',
             height = '100%')
      }, deleteFile = FALSE)
    } else {
      output$playerStatsTable2 <- renderImage({
        list(src = 'No.png',
             contentType = 'image/png',
             width = '100%',
             height = '0%')
      }, deleteFile = FALSE)
    }
  })
  #--------Weekly Print---------------------------------------------------------
  observeEvent(input$selectedWeek2, {
    selected_week2 <- input$selectedWeek2
    file_name_p3 <- paste0(selected_week2, ' - Complete Totals Stat Sheet.png')
    file_name_p4 <- paste0(selected_week2, ' - Basic Totals Stat Sheet.png')
    file_name_p5 <- paste0(selected_week2, ' - Advanced Totals Stat Sheet.png')
    if (file.exists(file_name_p3)) {
      output$WeeklyTable1 <- renderImage({
        list(src = file_name_p3,
             contentType = 'image/png',
             width = '70%',
             height = '100%')
      }, deleteFile = FALSE)
    } else {
      output$WeeklyTable1 <- renderImage({
        list(src = 'No.png',
             contentType = 'image/png',
             width = '100%',
             height = '100%')
      }, deleteFile = FALSE)
    }
    if (file.exists(file_name_p4)) {
      output$WeeklyTable2 <- renderImage({
        list(src = file_name_p4,
             contentType = 'image/png',
             width = '70%',
             height = '100%')
      }, deleteFile = FALSE)
    } else {
      output$WeeklyTable2 <- renderImage({
        list(src = 'No.png',
             contentType = 'image/png',
             width = '0%',
             height = '100%')
      }, deleteFile = FALSE)
    }
    if (file.exists(file_name_p5)) {
      output$WeeklyTable3 <- renderImage({
        list(src = file_name_p5,
             contentType = 'image/png',
             width = '70%',
             height = '100%')
      }, deleteFile = FALSE)
    } else {
      output$WeeklyTable3 <- renderImage({
        list(src = 'No.png',
             contentType = 'image/png',
             width = '0%',
             height = '100%')
      }, deleteFile = FALSE)
    }
  })
  #-------Total Print-----------------------------------------------------------
  output$TotalTable1 <- renderImage({
    list(src = 'Full Season - Complete Totals Stat Sheet.png',
         contentType = 'image/png',
         width = '60%',
         height = '100%')
  }, deleteFile = FALSE)
  output$TotalTable2 <- renderImage({
    list(src = 'Full Season - Basic Totals Stat Sheet.png',
         contentType = 'image/png',
         width = '70%',
         height = '100%')
  }, deleteFile = FALSE)
  output$TotalTable3 <- renderImage({
    list(src = 'Full Season - Advanced Totals Stat Sheet.png',
         contentType = 'image/png',
         width = '60%',
         height = '100%')
  }, deleteFile = FALSE)
  #------Player Line Graph------------------------------------------------------
  observeEvent(input$player, {
    player_selected <- input$player
    file_name <- paste0(player_selected, ' | Practice Progress.csv')
    data_line <- read.csv(file_name)
    data_line <- fix_line_chart(data_line)
    output$line_chart <- renderPlot({
      selected_stat <- input$selected_stat
      if (!is.null(selected_stat)) {
        df <- data_line %>%
          select(Practice, all_of(selected_stat))
        ggplot(df, aes(x = Practice)) +
          geom_line(aes(y = .data[[selected_stat]])) +
          geom_point(aes(y = .data[[selected_stat]]), color = "black", size = 2) +
          labs(title = paste0(player_selected, ' | Cumulative Practice Progress'), y = 'Value', x = "Practice #", subtitle = paste0(selected_stat)) +
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
#-------Radar Charts------------------------------------------------------------
  observeEvent(input$radar_player, {
    radar_player <- input$radar_player
    full_radar <- read.csv('Players | Practice Totals.csv')
    radar_file <- paste0(radar_player, ' | Practice Progress.csv')
    radar_data <- read.csv(radar_file)
    output$radar1 <- renderPlot({
      plot_data1 <- radar_data2(radar_data, full_radar)
      radar_chart1 <- ggplot(plot_data1, aes(x = Stat, y = Value)) +
        geom_col(aes(fill = Stat), width = 1, alpha = 0.9, color = 'white') +
        coord_polar(start = -pi/2, clip = "off")
      radar_chart1 <- radar_chart1 +
        theme_minimal() +
        theme(panel.grid.major = element_line(color = "white", size = 0.5),
              plot.background = element_rect(fill = "gray89", color = 'gray89'),
              legend.position = "none",
              axis.text.x = element_text(size = 12, face = "bold", color = "black"),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.y = element_blank()
        ) + 
        labs(title = paste0(radar_player, ' | Total Stats'),
             subtitle = paste0("Shaded areas are in comparison to rest of team. \nLarger the area, better they are performing compared to rest of team.")
        ) + 
        theme(
          plot.title = element_text(hjust = 0.5, size = 20, face = "bold", color = 'black'),
          plot.subtitle = element_text(color = 'black', size = 12, hjust = 0.5)
        ) +
        scale_fill_manual(values = c('#971B2F', '#971B2F', '#971B2F', '#971B2F', '#971B2F', '#971B2F'), guide = 'none')
      print(radar_chart1)
    })
    output$radar2 <- renderPlot({
      plot_data2 <- radar_data1(radar_data, full_radar)
      radar_chart2 <- ggplot(plot_data2, aes(x = Stat, y = Value)) +
        geom_col(aes(fill = Stat), width = 1, alpha = 0.9, color = 'white') +
        geom_point(aes(y = Avg), size = 3, color = "black") +
        geom_segment(aes(x = Stat, y = 0, xend = Stat, yend = Avg), linetype = "dashed", color = "black") +
        coord_polar(start = -pi/2, clip = "off")
      radar_chart2 <- radar_chart2 +
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
        labs(title = paste0(radar_player, ' | Shooting Efficiency'),
             subtitle = paste0("Dotted line represents average amongst team throughout practice.")
        ) + 
        theme(
          plot.title = element_text(hjust = 0.5, size = 20, face = "bold", color = 'black'),
          plot.subtitle = element_text(color = 'black', size = 12, hjust = 0.5)
        ) +
        scale_fill_manual(values = c('#971B2F', '#971B2F', '#971B2F', '#971B2F', '#971B2F', '#971B2F'), guide = 'none')
      print(radar_chart2)
    })
  })
#--------Frank Daily------------------------------------------------------------
  observeEvent(input$selectedDate3, {
    selected_date3 <- input$selectedDate3
    file_name_frank <- paste0(format(selected_date3, "%Y-%m-%d"), ' | Frank Totals.png')
    if (file.exists(file_name_frank)) {
      output$FrankTable <- renderImage({
        list(src = file_name_frank,
             contentType = 'image/png',
             width = '65%',
             height = '100%')
      }, deleteFile = FALSE)
    } else {
      output$FrankTable <- renderImage({
        list(src = 'No.png',
             contentType = 'image/png',
             width = '100%',
             height = '100%')
      }, deleteFile = FALSE)
    }
  })
#--------Frank Weekly-----------------------------------------------------------
  # observeEvent(input$selectedWeek3, {
  #   selected_week3 <- input$selectedWeek3
  #   weekly_frank_file <- paste0(selected_week3, ' - Frank Weekly Stat Sheet.png')
  #   if (file.exists(weekly_frank_file)) {
  #     output$WeeklyFrank <- renderImage({
  #       list(src = weekly_frank_file,
  #            contentType = 'image/png',
  #            width = '65%',
  #            height = '100%')
  #     }, deleteFile = FALSE)
  #   } else {
  #     output$WeeklyFrank <- renderImage({
  #       list(src = 'No.png',
  #            contentType = 'image/png',
  #            width = '100%',
  #            height = '100%')
  #     }, deleteFile = FALSE)
  #   }
  # })
#---------Frank Total-----------------------------------------------------------
  output$TotalFrank <- renderImage({
    list(src = 'Full Season - Frank Total Stat Sheet.png',
         contentType = 'image/png',
         width = '65%',
         height = '100%')
  }, deleteFile = FALSE)
}

shinyApp(ui, server)
