preprocess_server <- function(input, output, session) {
  rv <- reactiveValues(zip_path = NULL, processed_files = NULL)
  
  output$processLog <- renderPrint({})
  
  observeEvent(input$process, {
    req(input$raw_file)
    
    raw_lines <- readLines(file(input$raw_file$datapath, encoding = "CP932"))
    tds_line <- grep("^TDS", raw_lines)
    if (length(tds_line) > 0) {
      raw_lines <- raw_lines[1:(min(tds_line) - 1)]
    }
    raw_lines <- raw_lines[6:length(raw_lines)]
    raw_lines <- trimws(raw_lines)
    
    processed_files <- list()
    temp_dir <- tempfile()
    dir.create(temp_dir)
    
    i <- 1
    current_attr <- NULL
    
    while (i <= length(raw_lines)) {
      line <- raw_lines[i]
      
      if (grepl("^属性", line)) {
        current_attr <- sub("^属性", "", line)
        current_attr <- gsub("[\\/:*?\"<>|]", "", current_attr)
        if (current_attr == "" || is.na(current_attr)) current_attr <- "Unknown"
        i <- i + 1
        
      } else if (grepl("^試料", line)) {
        sample_name <- sub("^試料", "", line)
        sample_name <- gsub("[\\/:*?\"<>|]", "", sample_name)
        if (sample_name == "" || is.na(sample_name)) sample_name <- "Unknown"
        
        header <- unlist(strsplit(raw_lines[i + 1], ","))
        data_lines <- c()
        i <- i + 2
        
        while (i <= length(raw_lines) && !grepl("^試料", raw_lines[i]) && !grepl("^属性", raw_lines[i])) {
          data_lines <- c(data_lines, raw_lines[i])
          i <- i + 1
        }
        
        header[1] <- "Time"
        full_data <- read.csv(text = paste(data_lines, collapse = "
"), 
                              header = FALSE, col.names = header, fill = TRUE)
        
        out_dir <- file.path(temp_dir, sample_name)
        if (!dir.exists(out_dir)) dir.create(out_dir)
        out_path <- file.path(out_dir, paste0(current_attr, ".csv"))
        write.table(full_data, out_path, sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)
        processed_files[[length(processed_files) + 1]] <- out_path
        
      } else {
        i <- i + 1
      }
    }
    
    valid_files <- Filter(file.exists, unlist(processed_files))
    zip_path <- tempfile(fileext = ".zip")
    
    if (length(valid_files) > 0) {
      relative_paths <- file.path(basename(dirname(valid_files)), basename(valid_files))
      old_wd <- setwd(temp_dir)
      on.exit(setwd(old_wd), add = TRUE)
      
      zip(zipfile = zip_path, files = relative_paths)
    } else {
      stop("有効なファイルが存在しません。ZIPを作成できません。")
    }
    
    rv$zip_path <- zip_path
    rv$processed_files <- processed_files
    
    output$processLog <- renderPrint({
      cat("処理完了！以下のファイルが生成されました:\n")
      cat(paste(valid_files, collapse = "\n"))
    })
  })
  
  output$downloadZip <- downloadHandler(
    filename = function() {
      "processed_tds_files.zip"
    },
    content = function(file) {
      req(rv$zip_path)
      file.copy(rv$zip_path, file)
    },
    contentType = "application/zip"
  )
}
