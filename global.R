#global.R
library(showtext)
font_paths <- "C:/your/path"
font_files <- list.files(font_paths, pattern = "\\.(ttf|otf|ttc)$", full.names = TRUE, ignore.case = TRUE)
font_names <- tools::file_path_sans_ext(basename(font_files))
for (i in seq_along(font_files)) {
  font_add(font_names[i], font_files[i])
}
# フォント適用
showtext_auto()
library(cowplot)
library(RColorBrewer)
library(grDevices)
library(grid)
library(gtable)
library(shinyFiles)

get_legend_plot <- function(p) {
  gG <- ggplotGrob(p)
  legend <- gG$grobs[which(sapply(gG$grobs, function(x) x$name) == "guide-box")][[1]]
  return(legend)
}



