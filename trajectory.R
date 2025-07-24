library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(FactoMineR)
library(ggrepel)
library(purrr)
library(tools)



trajectory <- function(input, output, session) {
  # Trajectory 用 CSV ファイルを UI の fileInput で取得
  # input$files_traj が data.frame: name, size, type, datapath 等
  
  
  xinfo <- reactiveValues(min = 0, max = 1, step = 0.1)
  yinfo <- reactiveValues(min = 0, max = 1, step = 0.1)
  plot_obj_t <- reactiveVal(NULL)
  # ファイル読み込み〜PCA処理を reactive にまとめる
  trajData <- eventReactive(input$go_t, {
    req(input$files_traj)
    files <- input$files_traj$datapath
    
    # CSV → 長形式データ
    dr_raw <- map_dfr(seq_along(files), function(i) {
      file <- files[i]
      product_name <- file_path_sans_ext(basename(input$files_traj$name[i]))
      df <- read.csv(file)
      df %>%
        rename(Time = 1) %>%
        pivot_longer(-Time, names_to = "Attribute", values_to = "DR") %>%
        mutate(Product = product_name)
    })

    
    # 時間正規化&ビン分割
    dr_std <- dr_raw %>%
      group_by(Product) %>%
      mutate(Time_rel = (Time - min(Time)) / (max(Time) - min(Time))) %>%
      ungroup()
    target_t <- seq(0, 1, by = input$time_num)
    dr_11 <- dr_std %>%
      group_by(Product, Attribute) %>%
      slice(sapply(target_t, function(tt) which.min(abs(Time_rel - tt)))) %>%
      mutate(Time_bin = factor(target_t,
                               levels = target_t,
                               labels = sprintf("t%03d", as.integer(target_t * 100)))) %>%
      ungroup()
    
    # DR マトリクス作成
    dr_mat <- dr_11 %>%
      group_by(Product, Time_bin, Attribute) %>%
      summarise(DR = mean(DR), .groups = "drop") %>%
      pivot_wider(names_from = Attribute, values_from = DR) %>%
      arrange(Product, Time_bin)
    
    # PCA 実行
    pca_res <- PCA(select(dr_mat, -Product, -Time_bin), scale.unit = FALSE, graph = FALSE)
    
    # 個体座標 and 属性ベクトル
    ind <- as.data.frame(pca_res$ind$coord) %>%
      mutate(Product = dr_mat$Product,
             Time_index = as.numeric(dr_mat$Time_bin))
    var <- as.data.frame(pca_res$var$coord) %>%
      mutate(Attribute = rownames(.))
    
    # LOESS 平滑化
    smoothed_ind <- ind %>%
      group_by(Product) %>%
      arrange(Time_index) %>%
      mutate(
        Dim1_smooth = predict(loess(Dim.1 ~ Time_index, span = input$span_num)),
        Dim2_smooth = predict(loess(Dim.2 ~ Time_index, span = input$span_num))
      ) %>%
      ungroup()
    
    list(pca_res = pca_res,
         smoothed_ind = smoothed_ind,
         var = var)
  })
  
  # プロット描画
  output$trajectoryPlot <- renderPlot({
    data <- trajData()
    req(data)
    pca_res      <- data$pca_res
    smoothed_ind <- data$smoothed_ind
    var          <- data$var
    
    # ggplot の aes を冒頭で指定して、凡例を自動生成する
    p <- ggplot(smoothed_ind, aes(x = Dim1_smooth, y = Dim2_smooth,
                                  group = Product, colour = Product)) +
         theme_cowplot(font_size = input$font_size_traj,
                       font_family = input$font_family_traj,
                       line_size = 0.5,
                       rel_small = 14/input$font_size_traj,
                       rel_tiny = 11/input$font_size_traj,
                       rel_large = 16/input$font_size_traj) +
         theme(legend.text=element_text(size=10)) +
         geom_hline(yintercept = 0, linetype = "dashed")+ geom_vline(xintercept = 0, linetype = "dashed")+
         labs(title = "TDS Sensory Trajectories",
              x = paste0("PC1 (", round(pca_res$eig[1,2], 1), "%)"),
              y = paste0("PC2 (", round(pca_res$eig[2,2], 1), "%)"),
              colour = "Product")
    
    
    # カラーパレット設定（属性 + 基準線用色を明示）
    all_Product <- unique(smoothed_ind$Product)
    if (input$manual_col_traj) {
      # 入力された文字列をカンマ区切りで分割して色とする
      entered_colors <- strsplit(input$col_txt_traj, ",")[[1]]
      entered_colors <- trimws(entered_colors)  # 前後の空白を除く
      # 属性数と一致しない場合の補完（リサイクル）
      if (length(entered_colors) < length(all_Product)) {
        entered_colors <- rep(entered_colors, length.out = length(all_Product))}
      attr_colors <- entered_colors
    } else {
      attr_colors <- RColorBrewer::brewer.pal(n = max(3, length(all_Product)), name = "Set1")}
    names(attr_colors) <- all_Product
    manual_colors <- attr_colors
    
    
    
    
    p <- p + geom_path(arrow = arrow(type = "closed", length = unit(0.2, "cm")),
                       linewidth = 0.8, show.legend = TRUE) +
              geom_point(size = 1, show.legend = FALSE) +
              geom_segment(data = var,
                           aes(x = 0, y = 0, xend = Dim.1, yend = Dim.2),
                           arrow = arrow(length = unit(0.15, "cm")),
                           colour = "black", linewidth = 0.7,
                           inherit.aes = FALSE) +
              geom_text_repel(data = var,
                              aes(x = Dim.1, y = Dim.2, label = Attribute),
                              colour = "black", size = 3,
                              inherit.aes = FALSE) +
              labs(title = "TDS Sensory Trajectories",
                   x = paste0("PC1 (", round(pca_res$eig[1,2], 1), "%)"),
                   y = paste0("PC2 (", round(pca_res$eig[2,2], 1), "%)"),
                   colour = "Product")
    
    ### Get max/min value
    pb <- ggplot_build(p)
    x_breaks <- pb$layout$panel_params[[1]]$x$breaks
    y_breaks <- pb$layout$panel_params[[1]]$y$breaks
    
    xinfo$min <- min(x_breaks, na.rm = TRUE)
    xinfo$max <- max(x_breaks, na.rm = TRUE)
    xinfo$step <- if (length(na.omit(x_breaks)) >= 2) diff(x_breaks)[1] else 0.1
    
    yinfo$min <- min(y_breaks, na.rm = TRUE)
    yinfo$max <- max(y_breaks, na.rm = TRUE)
    yinfo$step <- if (length(na.omit(y_breaks)) >= 2) diff(na.omit(y_breaks))[1] else 0.1
    
    x_break_seq <- if (
      is.finite(input$xmin_input_traj) && is.finite(input$xmax_input_traj) &&
      is.finite(input$xstep_input_traj) && input$xstep_input_traj > 0 &&
      input$xmin_input_traj < input$xmax_input_traj
    ) {
      seq(input$xmin_input_traj, input$xmax_input_traj, by = input$xstep_input_traj)
    } else {
      pretty(c(xinfo$min, xinfo$max))
    }
    
    y_break_seq <- if (
      is.finite(input$ymin_input_traj) && is.finite(input$ymax_input_traj) &&
      is.finite(input$ystep_input_traj) && input$ystep_input_traj > 0 &&
      input$ymin_input_traj < input$ymax_input_traj
    ) {
      seq(input$ymin_input_traj, input$ymax_input_traj, by = input$ystep_input_traj)
    } else {
      pretty(c(yinfo$min, yinfo$max))
    }
    
    
    
    
    
    p <- p + scale_color_manual(values = manual_colors) + 
         scale_x_continuous(expand = c(0, 0),labels = label_number(drop0trailing = TRUE,  # 末尾のゼロを落とす
                                                                   trim          = TRUE), breaks = x_break_seq) + 
         scale_y_continuous(expand = c(0, 0),labels = label_number(drop0trailing = TRUE,  # 末尾のゼロを落とす
                                                                   trim          = TRUE), breaks = y_break_seq) + # 不要なスペースを落とす 
         coord_cartesian(xlim = c(input$xmin_input_traj, input$xmax_input_traj),
                         ylim = c(input$ymin_input_traj, input$ymax_input_traj))
    
    plot_obj_t(p)
    
    
    

    showtext_begin()
    p 
    }, width = reactive(input$fig_width_traj * 96),   # px換算
    height = reactive(input$fig_height_traj * 96))
    
  
  
  observe({
    updateNumericInput(session, "xmin_input_traj", value = xinfo$min, step = xinfo$step)
    updateNumericInput(session, "xmax_input_traj", value = xinfo$max, step = xinfo$step)
    updateNumericInput(session, "xstep_input_traj", value = xinfo$step, step = 0.1)
    
    updateNumericInput(session, "ymin_input_traj", value = yinfo$min, step = yinfo$step)
    updateNumericInput(session, "ymax_input_traj", value = yinfo$max, step = yinfo$step)
    updateNumericInput(session, "ystep_input_traj", value = yinfo$step, step = 0.1)
  })
  
  
    output$downloadPlot_traj <- downloadHandler(
    filename = function() paste0("tds_traj_", Sys.Date(), ".", input$fig_format_traj),
    content = function(file) {
      showtext_auto(FALSE)# definitely need
      if (input$fig_format_traj == "png") {
        ggsave2(file, plot = plot_obj_t(), device = "png", width = input$fig_width_traj, height = input$fig_height_traj, dpi = 600)
      } else {
        ggsave2(file, plot = plot_obj_t(), device = "svg", width = input$fig_width_traj, height = input$fig_height_traj)
      }
      showtext_auto(TRUE)# definitely need
    })
  

  
  
  
  
}