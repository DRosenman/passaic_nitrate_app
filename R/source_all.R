source_all <- function(folder = 'R') {
  files <- list.files(folder, full.names = TRUE)
  purrr::walk(files, source)
}
