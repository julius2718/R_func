
# General Functions

create_empty_df <- function(nrow, ncol, colnames = c()) {
  data.frame(matrix(vector(), nrow, ncol, dimnames = list(c(), colnames)))
} # https://htsuda.net/archives/2560 (as of: 2020-08-15)

save_plot <- function(obj = NULL, file_name = "", width = NULL, height = NULL) {
  pdf(file = file_name, width = width, height = height)
  print(obj)
  dev.off()
}

# Clear Workspace--------------------------------------------------------------
# https://htsuda.net/archives/1545 (as of: 2020-08-15)
clear_console <- function() {
  # Clear console messages
  cat("\014")
}

clear_plots <- function() {
  # Clear plots
  if (dev.cur() > 1) dev.off()
}

clear_workspace <- function() {
  # Clear global workspace
  rm(list = ls(envir = globalenv()), envir = globalenv())
}

clear_all <- function() {
  # Clear console, plots, and workspace
  clear_console()
  clear_plots()
  clear_workspace()
}
