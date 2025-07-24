#app.R
library(shiny)
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(tools)
library(DT)
library(RColorBrewer)
library(svglite)
library(zip)
library(shinyWidgets)
source("global.R")
source("tds_server.R")
source("preprocess_server.R")
source("differences.R")
source("trajectory.R")

ui <- fluidPage(
  titlePanel("TDSカーブ描画ツール"),
  tabsetPanel(
    tabPanel("TDSカーブ解析",
             sidebarLayout(
               sidebarPanel(
                 fileInput("files", "属性ごとのCSVを複数選択:", multiple = TRUE, accept = ".csv"),
                 splitLayout(cellWidths = c("50%","50%"),
                             numericInput("num_panelists", "評価者数:", value = 10, min = 1),
                             numericInput("num_repeats", "繰り返し回数:", value = 1, min = 1)),
                 checkboxInput("smooth", "スムージングを適用（loess）", value = FALSE),
                 checkboxInput("manual_col","任意の色",F),
                 conditionalPanel(condition = "input.manual_col == true",
                                  textInput("col_txt","任意の色を入力",value = "blue,green,#00704a,#FFC800,red,lightgreen")),
                 tags$div(
                   style = "display: flex; align-items: center; gap: 8px; margin-bottom: 6px;",
                   tags$div(
                     style = "flex: 0 0 70%;",
                     pickerInput(inputId = "font_family", label   = "フォントファミリー", choices = font_names, selected= font_names[2],
                                 options = list(
                                   `live-search` = FALSE,
                                   size          = 10,
                                   container     = "body"),
                                 width = "100%")),
                   tags$div(
                     style = "flex: 0 0 30%;",
                     numericInput(inputId = "font_size",label   = "サイズ", value   = 14, width   = "100%")
                   )),
                 actionButton("go", "描画実行"),
                 tags$hr(),
                 selectInput("fig_format", "図の保存形式", choices = c("png", "svg")),
                 downloadButton("downloadMetrics", "Metrics"),
                 downloadButton("downloadPlot", "Figure"),
                 downloadButton("download_legend", "Legend"),
                 downloadButton("DL_csv", "DP")
               ),
               mainPanel(
                 tags$div(style = "height: 25px;"), 
                 plotOutput("tdsPlot"),
                 tags$div(style = "height: 75px;"), 
                 splitLayout(cellWidths = c("50%","50%"),
                             numericInput("fig_width", "幅", value = 6, min = 1, step = 0.5),
                             numericInput("fig_height", "高さ", value = 4, min = 1, step = 0.5))
                 # dataTableOutput("metricsTable")
               )
             )
    ),
    tabPanel("差分カーブ",
             sidebarLayout(
               sidebarPanel(
                 # A/B フォルダ選択ボタン
                 tags$p("A-B間のTDS差分を求める。",
                 tags$br(),"各属性のCSVが",tags$br(),"格納されたフォルダを選択："),
                 tags$table(
                   style = "width: 100%; border-spacing: 0; margin-bottom: 10px;",
                   tags$tr(
                     tags$td(style = "width:45%; padding:0;",
                             shinyDirButton("dirA", "A フォルダ", "フォルダを選択", style = "width:100%;")),
                     tags$td(style = "width:10%; text-align: center; vertical-align: middle; padding:0;",
                             tags$span("—", style = "font-size: 24px; font-weight: bold;")),
                     tags$td(style = "width:45%; padding:0; text-align: right;",
                             shinyDirButton("dirB", "B フォルダ", "フォルダを選択", style = "width:100%;"))
                   )),
                 checkboxInput("manual_col_diff","任意の色",F),
                 conditionalPanel(condition = "input.manual_col_diff == true",
                                  textInput("col_txt_diff","任意の色を入力",value = "blue,green,#00704a,#FFC800,red,lightgreen")),
                 tags$div(style = "display: flex; align-items: center; gap: 8px; margin-bottom: 6px;", # Flexコンテナ：この中は横並びという枠組み
                   tags$div( # Flexアイテム：各アイテムの占める割合や配置を決める
                     style = "flex: 0 0 70%;",
                     pickerInput(inputId = "font_family_diff", label   = "フォントファミリー", choices = font_names, selected= font_names[1],
                                 options = list(
                                   `live-search` = FALSE,
                                   size          = 10,
                                   container     = "body"),
                                 width = "100%")),
                   tags$div(
                     style = "flex: 0 0 30%;",
                     numericInput(inputId = "font_size_diff",label   = "サイズ", value   = 14, width   = "100%")
                   )),
                 splitLayout(cellWidths = c("33%","33%","33%"),
                             numericInput("max_y","max:", value = 1, step = 0.25),
                             numericInput("min_y","min:", value = -1, step = 0.25),
                             numericInput("by_y","by value:", value = 10)),
                 actionButton("go_d", "描画実行"),
                 tags$hr(),
                 selectInput("fig_format_diff", "図の保存形式", choices = c("png", "svg")),
                 downloadButton("downloadMetrics", "Metrics"),
                 downloadButton("downloadPlot_diff", "Figure"),
                 downloadButton("download_legend", "Legend"),
                 downloadButton("DL_csv", "DP")
               ),
               mainPanel(
                 tags$div(style = "height: 25px;"),
                 plotOutput("diffPlot"),
                 tags$div(style = "height: 75px;"), 
                 splitLayout(cellWidths = c("50%","50%"),
                             numericInput("fig_width_diff", "幅", value = 6, min = 1, step = 0.5),
                             numericInput("fig_height_diff", "高さ", value = 4, min = 1, step = 0.5))
               )
             )),
    tabPanel("TCATA", # Temporal Check-All-That-Apply (TCATA) data
             sidebarLayout(
               sidebarPanel(
                 fileInput("files_traj", "ProductごとのCSVを複数選択:", multiple = TRUE, accept = ".csv"),
                 sliderInput("time_num","軌跡のプロット数（0.01 = 1%）:",min = 0.01,max = 0.2,step = .01,value = 0.05),
                 sliderInput("span_num","平滑化の度合い(loess):",min = 0.1,max = 1,step = .01,value = 0.6),
                 checkboxInput("manual_col_traj","任意の色",F),
                 conditionalPanel(condition = "input.manual_col_traj == true",
                                  textInput("col_txt_traj","任意の色を入力",value = "blue,#00704a,#FFC800,red")),
                 tags$div(style = "display: flex; align-items: center; gap: 8px; margin-bottom: 6px;",
                   tags$div(style = "flex: 0 0 70%;",
                     pickerInput(inputId = "font_family_traj", label   = "フォントファミリー", choices = font_names, selected= font_names[2],
                                 options = list(
                                   `live-search` = FALSE,
                                   size          = 10,
                                   container     = "body"),
                                 width = "100%")),
                   tags$div(style = "flex: 0 0 30%;",
                     numericInput(inputId = "font_size_traj",label   = "サイズ", value   = 14, width   = "100%")
                   )),
                 splitLayout(cellWidths = c("33%", "33%", "33%"),
                             numericInput("xmax_input_traj", "x軸 最大値", value = 0, min = -Inf),
                             numericInput("xmin_input_traj", "最小値", value = 0, min = -Inf),
                             numericInput("xstep_input_traj", "目盛間隔", value = 0.1, min = 0.1)),
                 splitLayout(cellWidths = c("33%", "33%", "33%"),
                             numericInput("ymax_input_traj", "y軸 最大値", value = 0, min = -Inf),
                             numericInput("ymin_input_traj", "最小値", value = 0, min = -Inf),
                             numericInput("ystep_input_traj", "目盛間隔", value = 0.1, min = 0.1)),
                 actionButton("go_t", "描画実行"),
                 tags$hr(),
                 selectInput("fig_format_traj", "図の保存形式", choices = c("png", "svg")),
                 downloadButton("downloadPlot_traj", "Figure"),
               ),
               mainPanel(
                 tags$div(style = "height: 25px;"),
                 plotOutput("trajectoryPlot"),
                 tags$div(style = "height: 75px;"), 
                 splitLayout(cellWidths = c("50%","50%"),
                             numericInput("fig_width_traj", "幅", value = 6, min = 1, step = 0.5),
                             numericInput("fig_height_traj", "高さ", value = 4, min = 1, step = 0.5))
                 
               )
             )),
    tabPanel("データ前処理",
             sidebarLayout(
               sidebarPanel(
                 fileInput("raw_file", "前処理対象のCSVファイルをアップロード:", accept = ".csv"),
                 actionButton("process", "前処理実行"),
                 downloadButton("downloadZip", "整形済CSVを一括ダウンロード")
               ),
               mainPanel(
                 verbatimTextOutput("processLog")
               )
             )
    )
  )
)

server <- function(input, output, session) {
  # 描画サイズをリアクティブに計算 (96px/inch 換算)
  reactiveWidth <- reactive({ paste0(input$fig_width * 96, "px") })
  reactiveHeight <- reactive({ paste0(input$fig_height * 96, "px") })
  
  # Windows のユーザープロファイルを優先取得
  win_home <- Sys.getenv("USERPROFILE")
  user_home <- if (nzchar(win_home)) win_home else path.expand("~")
  
  # デスクトップフォルダを優先ルートに設定
  desktop_paths <- file.path(user_home, c("Desktop", "デスクトップ"))
  desktop_root  <- desktop_paths[vapply(desktop_paths, dir.exists, logical(1))]
  roots <- if (length(desktop_root) > 0) {
    c(Desktop = desktop_root,
      shinyFiles::getVolumes()())
  } else {
    shinyFiles::getVolumes()()
  }
  
  # フォルダ選択UI
  shinyDirChoose(input, "dirA", roots = roots, session = session)
  shinyDirChoose(input, "dirB", roots = roots, session = session)
  
  
  
  # 各モジュールを呼び出し
  tds_server(input, output, session)
  preprocess_server(input, output, session)
  trajectory(input, output, session)
  differences(input, output, session)
}

shinyApp(ui = ui, server = server)
