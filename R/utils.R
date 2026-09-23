# Column resolution and shared validation. This is the only place in the
# package where tidy evaluation happens; everything downstream sees the
# canonical columns instead of whatever the caller called them.

`%||%` <- function(x, y) if (is.null(x)) y else x

# rlang has quo_is_null() but no quo_is_false(); sort_var = FALSE is how a
# caller asks to keep the input order.
quo_is_false <- function(quo) {
  identical(rlang::quo_get_expr(quo), FALSE)
}

#' Resolve one column argument against the data
#'
#' @param quo A quosure captured from a user argument.
#' @param data The data frame it should be evaluated against.
#' @param arg The argument name, for error messages.
#' @param required Whether a `NULL` argument is an error.
#'
#' @return A vector of `nrow(data)` values, or `NULL` when the argument was
#'   `NULL` and not required.
#'
#' @noRd
eval_col <- function(quo, data, arg, required = TRUE) {
  if (rlang::quo_is_null(quo) || rlang::quo_is_missing(quo)) {
    if (required) {
      rlang::abort(sprintf("`%s` is required.", arg))
    }
    return(NULL)
  }

  out <- rlang::eval_tidy(quo, data)

  if (is.null(out)) {
    if (required) rlang::abort(sprintf("`%s` resolved to NULL.", arg))
    return(NULL)
  }
  if (length(out) == 1L && nrow(data) != 1L) {
    out <- rep(out, nrow(data))
  }
  if (length(out) != nrow(data)) {
    rlang::abort(sprintf(
      "`%s` gave %d value(s) for %d row(s) of data.",
      arg, length(out), nrow(data)
    ))
  }
  out
}

#' Coerce a resolved column to numeric, with a useful error
#' @noRd
as_numeric_col <- function(x, arg) {
  if (is.numeric(x)) return(as.numeric(x))
  out <- suppressWarnings(as.numeric(x))
  if (all(is.na(out)) && !all(is.na(x))) {
    rlang::abort(sprintf("`%s` must be numeric, not %s.", arg, class(x)[1]))
  }
  out
}

#' Check that a data frame was supplied
#' @noRd
check_data <- function(data, arg = "data") {
  if (!is.data.frame(data)) {
    rlang::abort(sprintf("`%s` must be a data frame, not %s.",
                         arg, class(data)[1]))
  }
  invisible(data)
}

#' Warn once about rows being dropped
#'
#' A plot that quietly discards a subject is worse than one that says so.
#' @noRd
warn_dropped <- function(n, why) {
  if (n > 0L) {
    rlang::warn(sprintf("Dropped %d row(s): %s.", n, why))
  }
  invisible(NULL)
}
