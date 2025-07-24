tds_server <- function(input, output, session) {
  plot_obj <- reactiveVal(NULL)
  
  observeEvent(input$files, {
    req(input$files)
    first_file <- input$files$datapath[1]
    df <- read.csv(first_file)
    n_subjects <- ncol(df) - 1  # 1列目はTime列
    updateNumericInput(session, "num_panelists", value = n_subjects)
  })
  
  calculate_Ps <- function(P0, num_panelists, num_repeats) {
    z_value <- qnorm(0.95)
    n <- num_panelists * num_repeats
    Ps <- min(P0 + z_value * sqrt(P0 * (1 - P0) / n), 1)
    return(Ps)
  }
  
  calculate_tds_metrics <- function(tds_df, Ps, time_vector = NULL) {
    if (is.null(time_vector)) {
      time_vector <- seq(0, 1, length.out = nrow(tds_df))
    }
    
    results <- data.frame(
      Attribute = character(),
      MaxValue = numeric(),
      TimeAtMax = numeric(),
      DurationAbovePs = numeric(),
      AUC = numeric(),
      stringsAsFactors = FALSE
    )
    
    for (attr in names(tds_df)) {
      values <- tds_df[[attr]]
      max_val <- max(values)
      time_at_max <- time_vector[which.max(values)]
      above_idx <- which(values > Ps)
      duration <- if (length(above_idx) > 0) length(above_idx) * mean(diff(time_vector)) else 0
      auc <- if (length(above_idx) > 0) sum((values[above_idx] - Ps) * mean(diff(time_vector))) else 0
      
      results <- rbind(results, data.frame(
        Attribute = attr,
        MaxValue = max_val,
        TimeAtMax = time_at_max,
        DurationAbovePs = duration,
        AUC = auc
      ))
    }
    return(results)
  }
  
  # 1. ファイル読み込み＆初期処理は一度だけ行う
  tds_function <- reactive({
    req(input$files)
    files <- input$files
    dr_list <- list()
    tds_wide <- list()
    time_vec <- NULL
    
    for (i in 1:nrow(files)) {
      file <- files$datapath[i]
      df <- read.csv(file)
      time <- df[[1]]
      if (is.null(time_vec)) time_vec <- time
      n_subj <- ncol(df) - 1
      dr <- rowSums(df[, -1], na.rm = TRUE) / n_subj
      attr_name <- tools::file_path_sans_ext(basename(files$name[i]))
      dr_list[[i]] <- data.frame(Time = time, Attribute = attr_name, DP = dr)
      tds_wide[[attr_name]] <- dr
    }
    
    dr_data <- dplyr::bind_rows(dr_list)
    tds_df <- as.data.frame(tds_wide)
    # この関数を呼び出したときに利用する項目でリスト化
    list(
      dr_data = dr_data,
      tds_df = tds_df,
      time_vector = time_vec,
      num_attr = length(unique(dr_data$Attribute)),
      dr_list = dr_list
    )
  })
  
  
  
  observeEvent(input$go, {
    Re <- tds_function()
    P0 <- 1 / Re$num_attr
    Ps <- calculate_Ps(P0, input$num_panelists, input$num_repeats)
    
    reference_lines <- data.frame(
      Time = rep(unique(Re$dr_data$Time), 2),
      Value = c(rep(P0, length(unique(Re$dr_data$Time))), rep(Ps, length(unique(Re$dr_data$Time)))),
      Reference = rep(c("Chance Level", "Significance Level"), each = length(unique(Re$dr_data$Time)))
    )
    
    p <- ggplot() +
         theme_cowplot(font_size = input$font_size,
                       font_family = input$font_family,
                       line_size = 0.5,
                       rel_small = 14/input$font_size,
                       rel_tiny = 11/input$font_size,
                       rel_large = 16/input$font_size) +
          theme(legend.text=element_text(size=10)) +
         (if (input$smooth) {
           geom_smooth(data = Re$dr_data, aes(x = Time, y = DP, color = Attribute), method = "loess", se = FALSE, span = 0.2)
         } else {
           geom_line(data = Re$dr_data, aes(x = Time, y = DP, color = Attribute), linewidth = 1.2)
         }) +
         geom_line(data = reference_lines, aes(x = Time, y = Value, linetype = Reference), color = "black", linewidth = 1)
      
    
    
    # カラーパレット設定（属性 + 基準線用色を明示）
    all_attributes <- unique(Re$dr_data$Attribute)
    if (input$manual_col) {
      # 入力された文字列をカンマ区切りで分割して色とする
      entered_colors <- strsplit(input$col_txt, ",")[[1]]
      entered_colors <- trimws(entered_colors)  # 前後の空白を除く
      # 属性数と一致しない場合の補完（リサイクル）
      if (length(entered_colors) < length(all_attributes)) {
        entered_colors <- rep(entered_colors, length.out = length(all_attributes))}
      attr_colors <- entered_colors
    } else {
      attr_colors <- RColorBrewer::brewer.pal(n = max(3, length(all_attributes)), name = "Set1")}
    names(attr_colors) <- all_attributes
    manual_colors <- c(attr_colors,
                       "Chance Level" = "black",
                       "Significance Level" = "gray40")
    
    p <- p + scale_color_manual(values = manual_colors) +
        scale_linetype_manual(values = c("Chance Level" = "dashed", "Significance Level" = "dotted")) +
        labs(x = "Time (sec)", y = "Dominance proportion", color = "Attribute", linetype = "Reference") +
        theme(text = element_text(family = input$font_family, size = 14))
    
    pb <- ggplot_build(p)
    x_max <- max(pb$layout$panel_params[[1]]$x$breaks)
    p <- p + scale_x_continuous(expand = c(0, 0)) +
             scale_y_continuous(expand = c(0, 0)) + coord_cartesian(ylim = c(0, 1), xlim = c(0, x_max))
    
    plot_obj(p)
    
    
    output$tdsPlot <- renderPlot({
      showtext_begin()
      p 
      }, width = reactive(input$fig_width * 96),   # px換算
      height = reactive(input$fig_height * 96))
    
    # output$metricsTable <- renderDataTable({ datatable(metrics, options = list(pageLength = 5)) })
    
    output$downloadMetrics <- downloadHandler(
      filename = function() paste0("tds_metrics_", Sys.Date(), ".csv"),
      content = function(file) {
        metrics <- calculate_tds_metrics(Re$tds_df, Ps, time_vector = Re$dr_list[[1]]$Time)
        readr::write_excel_csv(metrics, file)
      })
    
    
    output$downloadPlot <- downloadHandler(
      filename = function() paste0("tds_plot_", Sys.Date(), ".", input$fig_format),
      content = function(file) {
        showtext_auto(FALSE)# definitely need
        if (input$fig_format == "png") {
          ggsave2(file, plot = plot_obj(), device = "png", width = input$fig_width, height = input$fig_height, dpi = 600)
        } else {
          ggsave2(file, plot = plot_obj(), device = "svg", width = input$fig_width, height = input$fig_height)
        }
        showtext_auto(TRUE)# definitely need
      })
    
    output$download_legend <- downloadHandler(
      filename = function() {
        paste0("tds_plot_legend_", Sys.Date(), ".", input$fig_format)
      },
      content = function(file) {
        legend_grob <- get_legend_plot(plot_obj())
        
        # レジェンドのサイズを unit で取得
        width_unit <- grobWidth(legend_grob)
        height_unit <- grobHeight(legend_grob)
        
        # 単位（inches）に変換（相対単位のままだと使えないので cm → inch に変換）
        width_in  <- convertWidth(width_unit, "inches", valueOnly = TRUE)
        height_in <- convertHeight(height_unit, "inches", valueOnly = TRUE)
        
        # マージンを少し加える（見切れ防止）
        margin_in <- 0.2
        if (input$fig_format == "png") {
          png(file, width = width_in + margin_in, height = height_in + margin_in, units = "in", res = 300)
        } else {
          svg(file, width = width_in + margin_in, height = height_in + margin_in)
        }
        grid.newpage()
        grid.draw(legend_grob)
        dev.off()
      })
      
    output$DL_csv <- downloadHandler(
      filename = function() {
        paste0("Product_name.csv")
      },
      content = function(file) {
        Re <- tds_function()
        tds_df_with_time <- cbind(Time = Re$dr_list[[1]]$Time, Re$tds_df)
        readr::write_excel_csv(tds_df_with_time, file)
      }
    )
    
      
      
    
    
    
  })
}
