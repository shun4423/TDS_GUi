library(shiny)
library(shinyFiles)
library(readr)
library(dplyr)
library(tidyr)
library(tempR)
library(ggplot2)
library(tools)

library(scales)
# A/B フォルダの差分解析とプロット描画モジュール
differences <- function(input, output, session) {
  # UIの設定：Windows のユーザープロファイルを優先取得 ####
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
  # Finish ####
  
  
  
  # Fig plotのため初期設定
  Re <- reactiveValues()
  plot_obj <- reactiveVal(NULL)
  
  # 実行ボタンで差分データを計算
  diffData <- eventReactive(input$go_d, {
    req(input$dirA, input$dirB)
    dirA <- parseDirPath(roots, input$dirA)
    dirB <- parseDirPath(roots, input$dirB)
    
    csv_A <- list.files(dirA, pattern = "\\.csv$", full.names = TRUE)
    csv_B <- list.files(dirB, pattern = "\\.csv$", full.names = TRUE)
    if (length(csv_A) == 0 || length(csv_B) == 0)
      stop("CSV が見つかりません")
    
    attrs <- intersect(
      file_path_sans_ext(basename(csv_A)),
      file_path_sans_ext(basename(csv_B))
    )
    if (!setequal(attrs, file_path_sans_ext(basename(csv_A))))
      stop("A/B の属性ファイルが一致しません")
    
    diff_long <- data.frame()
    for (attr in sort(attrs)) {
      df_A <- read_csv(file.path(dirA, paste0(attr, ".csv")), show_col_types = FALSE)
      df_B <- read_csv(file.path(dirB, paste0(attr, ".csv")), show_col_types = FALSE)
      time_vec <- df_A[[1]]
      if (!all(time_vec == df_B[[1]]))
        stop(paste("時間軸が一致しません:", attr))
      
      mat_A    <- t(as.matrix(df_A[ , -1]))
      mat_B    <- t(as.matrix(df_B[ , -1]))
      diff_vec <- get.differences(mat_A, mat_B)
      lsd_vec  <- get.significance.diff(mat_A, mat_B)
      sig_flag <- abs(diff_vec) > lsd_vec
      
      diff_df <- data.frame(
        Time      = time_vec,
        Difference= as.numeric(diff_vec),
        LSD       = as.numeric(lsd_vec),
        Sig       = sig_flag,
        PlotVal   = ifelse(sig_flag, diff_vec, NA),
        Attribute = attr
      )
      diff_long <- bind_rows(diff_long, diff_df)
    }
    diff_long
  })
  
  # プロット描画
  output$diffPlot <- renderPlot({
    df <- diffData()
    req(nrow(df) > 0)
    title_txt <- paste0(
      "TDS Difference Curve  (",
      basename(parseDirPath(roots, input$dirA)),
      " – ",
      basename(parseDirPath(roots, input$dirB)),
      ")"
    )
    
    p <- ggplot(df, aes(x = Time, y = PlotVal, colour = Attribute, group = Attribute)) +
         theme_cowplot(font_size = input$font_size_diff,
                       font_family = input$font_family_diff,
                       line_size = 0.5,
                       rel_small = 14/input$font_size_diff,
                       rel_tiny = 11/input$font_size_diff,
                       rel_large = 16/input$font_size_diff) +
         theme(legend.text=element_text(size=10)) +
         geom_line(linewidth = 1.2, na.rm = TRUE) +
         geom_hline(yintercept = 0, linetype = "dashed") +
         labs(title = title_txt,
              x = "Time (sec)",
              y = "TDS difference curve", color = "Attribute")
    
    
    # カラーパレット設定（属性 + 基準線用色を明示）
    all_attributes <- unique(df$Attribute)
    if (input$manual_col_diff) {
      # 入力された文字列をカンマ区切りで分割して色とする
      entered_colors <- strsplit(input$col_txt_diff, ",")[[1]]
      entered_colors <- trimws(entered_colors)  # 前後の空白を除く
      # 属性数と一致しない場合の補完（リサイクル）
      if (length(entered_colors) < length(all_attributes)) {
        entered_colors <- rep(entered_colors, length.out = length(all_attributes))}
      attr_colors <- entered_colors
    } else {
      attr_colors <- RColorBrewer::brewer.pal(n = max(3, length(all_attributes)), name = "Set1")}
    names(attr_colors) <- all_attributes
    manual_colors <- attr_colors
    
    
    pb <- ggplot_build(p)
    x_max <- max(pb$layout$panel_params[[1]]$x$breaks)
    y_max <- max(pb$layout$panel_params[[1]]$y$breaks)
    p <- p + scale_color_manual(values = manual_colors) + 
             scale_x_continuous(expand = c(0, 0)) + 
             scale_y_continuous(expand = c(0, 0),labels = label_number(drop0trailing = TRUE,  # 末尾のゼロを落とす
                                                                       trim          = TRUE)) + # 不要なスペースを落とす 
             coord_cartesian(ylim = c(input$min_y, input$max_y), xlim = c(0, x_max))
    
    plot_obj(p)
    
    
    showtext_begin()
    p 
    }, width = reactive(input$fig_width_diff * 96),   # px換算
    height = reactive(input$fig_height_diff * 96))

  
  output$downloadPlot_diff <- downloadHandler(
    filename = function() paste0("tds_diff_", Sys.Date(), ".", input$fig_format_diff),
    content = function(file) {
      showtext_auto(FALSE)# definitely need
      if (input$fig_format_diff == "png") {
        ggsave2(file, plot = plot_obj(), device = "png", width = input$fig_width_diff, height = input$fig_height_diff, dpi = 600)
      } else {
        ggsave2(file, plot = plot_obj(), device = "svg", width = input$fig_width_diff, height = input$fig_height_diff)
      }
      showtext_auto(TRUE)# definitely need
    })
    

  
  

  
  
  
  
  
  
  
}
