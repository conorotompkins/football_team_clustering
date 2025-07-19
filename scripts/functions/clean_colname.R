clean_colname <- function(x) {
  stringr::str_remove(x, "\\.\\.\\.[0-9]+$")
}
