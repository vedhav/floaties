#' Plotting engines
#'
#' An engine says *how* a plot is drawn. Every plot function in floaties takes
#' one, and dispatches on it. The engine is the only part of the package that
#' knows which plotting library is in use: everything upstream of it works on a
#' canonical data structure with fixed column names.
#'
#' Adding a new engine means writing `render_waterfall()` and `render_swimlane()`
#' methods for it. No existing plot function has to change. See
#' `vignette("extending")` if you want to ship one from another package.
#'
#' @param ... Engine-specific options, stored on the object and passed to the
#'   renderer. Unused by the built-in engines today.
#'
#' @return An object of class `floaties_engine`.
#'
#' @examples
#' eng_ggplot2()
#'
#' @name engines
NULL

#' @rdname engines
#' @export
eng_ggplot2 <- function(...) {
  rlang::check_installed("ggplot2", "for `eng_ggplot2()`.")
  new_engine("ggplot2", ...)
}

#' @rdname engines
#' @export
eng_plotly <- function(...) {
  rlang::check_installed("plotly", "for `eng_plotly()`.")
  new_engine("plotly", ...)
}

#' @rdname engines
#' @export
eng_echarts4r <- function(...) {
  rlang::check_installed("echarts4r", "for `eng_echarts4r()`.")
  new_engine("echarts4r", ...)
}

#' Build an engine object
#'
#' The constructor an external engine package calls. Given `subclass = "foo"`,
#' the returned object has class `c("floaties_foo", "floaties_engine")`, so
#' `render_waterfall.floaties_foo()` will be dispatched to.
#'
#' @param subclass A string naming the engine, e.g. `"ggplot2"`.
#' @param ... Engine-specific options.
#'
#' @return An object of class `floaties_engine`.
#'
#' @examples
#' new_engine("text", width = 40)
#'
#' @export
new_engine <- function(subclass, ...) {
  if (!rlang::is_string(subclass)) {
    rlang::abort("`subclass` must be a single string.")
  }
  structure(
    list(...),
    class = c(paste0("floaties_", subclass), "floaties_engine")
  )
}

#' @export
print.floaties_engine <- function(x, ...) {
  cat("<floaties engine: ", engine_name(x), ">\n", sep = "")
  opts <- names(x)
  if (length(opts) > 0) {
    cat("  options: ", paste(opts, collapse = ", "), "\n", sep = "")
  }
  invisible(x)
}

engine_name <- function(x) sub("^floaties_", "", class(x)[1])

check_engine <- function(engine, arg = "engine") {
  if (!inherits(engine, "floaties_engine")) {
    rlang::abort(c(
      sprintf("`%s` must be a floaties engine.", arg),
      i = "Use eng_ggplot2(), eng_plotly() or eng_echarts4r()."
    ))
  }
  invisible(engine)
}
