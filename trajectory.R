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
  
  # =====================
  # [ADD] 軸初期化フラグ
  # =====================
  # --- FIX: 軸の初期値は「データ読み込み直後の1回だけ」反映し、それ以降はユーザー入力を保持 ---
  axis_initialized_traj <- reactiveVal(FALSE)
  
  # 新しいデータ（files_traj）が入ったら、軸の自動初期化をやり直す
  observeEvent(input$files_traj, {
    axis_initialized_traj(FALSE)
  }, ignoreInit = TRUE)
  
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
      arrange(Time_rel) %>%
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
    
    list(dr_mat = dr_mat, pca_res = pca_res)
  })
  
  
  output$trajectoryPlot <- renderPlot({
    dat <- trajData()
    req(dat)
    
    dr_mat <- dat$dr_mat
    pca_res <- dat$pca_res
    
    # 個体（Product×Time_bin）のスコア
    ind_coords <- as.data.frame(pca_res$ind$coord[, 1:2])
    ind_coords$Product <- dr_mat$Product
    ind_coords$Time_bin <- dr_mat$Time_bin
    
    # 変数（Attribute）の座標
    var_coords <- as.data.frame(pca_res$var$coord[, 1:2])
    var_coords$Attribute <- rownames(var_coords)
    
    # 軌跡の平滑化（Product ごと）
    smoothed_ind <- ind_coords %>%
      group_by(Product) %>%
      arrange(Time_bin) %>%
      mutate(Time_index = as.numeric(Time_bin)) %>%
      mutate(
        Dim1_smooth = predict(loess(Dim.1 ~ Time_index, span = input$span_num), Time_index),
        Dim2_smooth = predict(loess(Dim.2 ~ Time_index, span = input$span_num), Time_index)
      ) %>%
      ungroup()
    
    manual_colors <- NULL
    all_Product <- unique(smoothed_ind$Product)
    if (input$manual_col_traj) {
      entered_colors <- strsplit(input$col_txt_traj, ",")[[1]]
      entered_colors <- trimws(entered_colors)
      if (length(entered_colors) < length(all_Product)) {
        entered_colors <- rep(entered_colors, length.out = length(all_Product))
      }
      manual_colors <- setNames(entered_colors, all_Product)
    } else {
      palette_cols <- RColorBrewer::brewer.pal(n = max(3, length(all_Product)), name = "Set1")
      manual_colors <- setNames(palette_cols[seq_along(all_Product)], all_Product)
    }
    
    
    p <- ggplot() +
      geom_path(
        data = smoothed_ind,
        aes(x = Dim1_smooth, y = Dim2_smooth, colour = Product, group = Product),
        arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
        linewidth = 1
      ) +
      geom_point(
        data = smoothed_ind,
        aes(x = Dim1_smooth, y = Dim2_smooth, colour = Product),
        size = input$point_num,
        show.legend = FALSE
      ) +
      geom_segment(
        data = var_coords,
        aes(x = 0, y = 0, xend = Dim.1, yend = Dim.2),
        arrow = arrow(length = unit(0.15, "cm")),
        colour = "black"
      ) +
      geom_text_repel(
        data = var_coords,
        aes(x = Dim.1, y = Dim.2, label = Attribute),
        colour = "black"
      ) +
      theme_cowplot(font_size = input$font_size_traj,
                    font_family = input$font_family_traj,
                    line_size = 0.5,
                    rel_small = 14/input$font_size_traj,
                    rel_tiny = 11/input$font_size_traj,
                    rel_large = 16/input$font_size_traj) +
      theme(legend.text=element_text(size=10)) +
      geom_hline(yintercept = 0, linetype = "dashed")+ geom_vline(xintercept = 0, linetype = "dashed") +
      labs(
        x = paste0("PC1 (", round(pca_res$eig[1,2], 1), "%)"),
        y = paste0("PC2 (", round(pca_res$eig[2,2], 1), "%)"),
        colour = "Product"
      )
    
    
    ### Get max/min value
    # =======================
    # [FIX] max/min が取れない問題の対策
    # =======================
    # ggplot_build() は ggplot2 のバージョン差で構造が変わることがあるため、
    # ここでは「実データ」から軸候補を作り、pretty() で初期軸を決める。
    x_breaks <- pretty(c(0, smoothed_ind$Dim1_smooth, var_coords$Dim.1))
    y_breaks <- pretty(c(0, smoothed_ind$Dim2_smooth, var_coords$Dim.2))
    xinfo$min <- min(x_breaks, na.rm = TRUE)
    xinfo$max <- max(x_breaks, na.rm = TRUE)
    xinfo$step <- if (length(na.omit(x_breaks)) >= 2) diff(na.omit(x_breaks))[1] else 0.1
    yinfo$min <- min(y_breaks, na.rm = TRUE)
    yinfo$max <- max(y_breaks, na.rm = TRUE)
    yinfo$step <- if (length(na.omit(y_breaks)) >= 2) diff(na.omit(y_breaks))[1] else 0.1
    
    # ======================================================
    # [ADD/CHANGED] 「初回だけ」自動値→UIに反映、以降はユーザー入力
    # ======================================================
    # --- FIX: 軸入力の初期化は「データごとに1回だけ」---
    use_user_axis <- axis_initialized_traj()
    
    if (!use_user_axis) {
      # 初回（新データ読み込み直後）は、自動計算した範囲を使いつつ UI に反映
      xmin <- xinfo$min; xmax <- xinfo$max; xstep <- xinfo$step
      ymin <- yinfo$min; ymax <- yinfo$max; ystep <- yinfo$step
      
      axis_initialized_traj(TRUE)  # 先に TRUE にして再描画時の上書きを防ぐ
      
      updateNumericInput(session, "xmin_input_traj", value = xmin, step = xstep)
      updateNumericInput(session, "xmax_input_traj", value = xmax, step = xstep)
      updateNumericInput(session, "xstep_input_traj", value = xstep, step = 0.1)
      
      updateNumericInput(session, "ymin_input_traj", value = ymin, step = ystep)
      updateNumericInput(session, "ymax_input_traj", value = ymax, step = ystep)
      updateNumericInput(session, "ystep_input_traj", value = ystep, step = 0.1)
    } else {
      # 2回目以降はユーザー指定を尊重（time_num / span_num を変えても上書きしない）
      xmin <- input$xmin_input_traj; xmax <- input$xmax_input_traj; xstep <- input$xstep_input_traj
      ymin <- input$ymin_input_traj; ymax <- input$ymax_input_traj; ystep <- input$ystep_input_traj
    }
    
    axis_ok <- function(minv, maxv, stepv) {
      is.numeric(minv) && length(minv) == 1 && is.finite(minv) &&
        is.numeric(maxv) && length(maxv) == 1 && is.finite(maxv) &&
        is.numeric(stepv) && length(stepv) == 1 && is.finite(stepv) && stepv > 0 &&
        minv < maxv
    }
    
    x_break_seq <- if (axis_ok(xmin, xmax, xstep)) {
      seq(xmin, xmax, by = xstep)
    } else {
      pretty(c(xinfo$min, xinfo$max))
    }
    
    y_break_seq <- if (axis_ok(ymin, ymax, ystep)) {
      seq(ymin, ymax, by = ystep)
    } else {
      pretty(c(yinfo$min, yinfo$max))
    }
    
    
    # =========================
    # [CHANGED] coord_cartesian
    # =========================
    p <- p + scale_color_manual(values = manual_colors) + 
      scale_x_continuous(
        expand = c(0, 0),
        labels = label_number(drop0trailing = TRUE, trim = TRUE),
        breaks = x_break_seq
      ) + 
      scale_y_continuous(
        expand = c(0, 0),
        labels = label_number(drop0trailing = TRUE, trim = TRUE),
        breaks = y_break_seq
      ) +
      coord_cartesian(xlim = c(xmin, xmax), ylim = c(ymin, ymax))
    
    plot_obj_t(p)
    
    
    showtext_begin()
    p
  }, width = reactive(input$fig_width_traj * 96),   # px換算
  height = reactive(input$fig_height_traj * 96))
  
  
  # ======================================================
  # [REMOVE] ここにあった observe(updateNumericInput...) を削除
  # （再描画のたびに入力を上書きしてリセットされる原因）
  # ======================================================
  # (FIX) 以前ここにあった observe(updateNumericInput...) は、再描画のたびに入力を上書きしてしまうため削除
  
  
  output$downloadPlot_traj <- downloadHandler(
    filename = function() paste0("tds_traj_", Sys.Date(), ".", input$fig_format_traj),
    content = function(file) {
      req(plot_obj_t())
      showtext_auto(FALSE) # definitely need
      if (input$fig_format_traj == "png") {
        ggsave2(file, plot = plot_obj_t(), device = "png", width = input$fig_width_traj, height = input$fig_height_traj, dpi = 600)
      } else {
        ggsave2(file, plot = plot_obj_t(), device = "svg", width = input$fig_width_traj, height = input$fig_height_traj)
      }
      showtext_auto(TRUE) # definitely need
    }
  )
  
  
}
