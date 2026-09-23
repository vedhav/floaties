fmt_num <- function(x) {
  format(x, trim = TRUE, drop0trailing = TRUE)
}

# Build a list of per-row arrays for echarts series data. Lists (not atomic
# vectors) keep single rows as JSON arrays under htmlwidgets' auto_unbox.
row_values <- function(...) {
  cols <- list(...)
  lapply(seq_along(cols[[1]]), function(i) lapply(cols, `[[`, i))
}
