
# General Functions

create_empty_df <- function(nrow, ncol, colnames = c()) {
  data.frame(matrix(vector(), nrow, ncol, dimnames = list(c(), colnames)))
} # https://htsuda.net/archives/2560 (as of: 2020-08-15)

save_plot <- function(obj = NULL, file_name = "", width = NULL, height = NULL) {
  pdf(file = file_name, width = width, height = height)
  print(obj)
  dev.off()
}
