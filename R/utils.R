# ============================
# Auxiliary print functions
# ============================

#' @keywords internal
.print_header <- function(title) {
  cat("\n")
  cat(strrep("=", 50), "\n")
  cat(title, "\n")
  cat(strrep("=", 50), "\n")
}

#' @keywords internal
.print_block <- function(title, content, width = 40) {
  cat("\n", title, "\n", sep = "")
  cat(strrep("-", width), "\n")
  content()
  cat(strrep("-", width), "\n")
}
