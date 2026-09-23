#' Waterfall plot
#'
#' One bar per subject, sorted worst to best. The chart a medical monitor uses
#' to see at a glance who responded and by how much.
#'
#' Column roles are arguments, so it does not matter where the data came from
#' as long as it has one row per subject with a value to plot. Deriving that
#' value, for example a best percentage change from baseline, is the caller's
#' job.
#'
#' @param data A data frame, one row per subject.
#' @param x_var Column identifying the subject. One bar each.
#' @param value_var Numeric column giving the bar height.
#' @param sort_var Column to sort by. The default, `NULL`, sorts by
#'   `value_var` descending, because a waterfall that is not sorted by value is
#'   not a waterfall. Pass `FALSE` to keep the order of the input.
#' @param color_var Optional column driving the fill colour, e.g. a response
#'   category.
#' @param hlines Numeric vector of reference line positions. These are in the
#'   units of `value_var`: for RECIST against a percentage change column that is
#'   `c(20, -30)`, not `c(0.2, -0.3)`.
#' @param title Plot title, or `NULL` for none.
#' @param engine A [floaties engine][engines].
#'
#' @return The engine's own plot object: a `ggplot`, a `plotly` or an
#'   `echarts4r`. You can keep customising it with that library's own verbs.
#'
#' @examples
#' subjects <- data.frame(
#'   id       = sprintf("01-%03d", 1:8),
#'   change   = c(-100, -62, -41, -30, -8, 12, 24, 57),
#'   response = c("CR", "PR", "PR", "PR", "SD", "SD", "PD", "PD")
#' )
#'
#' waterfall(subjects, id, change)
#' waterfall(subjects, id, change, color_var = response, hlines = c(20, -30))
#'
#' @seealso [swimlane()]
#' @export
waterfall <- function(data,
                      x_var,
                      value_var,
                      sort_var = NULL,
                      color_var = NULL,
                      hlines = NULL,
                      title = NULL,
                      engine = eng_ggplot2()) {
  check_engine(engine)

  pd <- prepare_waterfall(
    data,
    x_var     = {{ x_var }},
    value_var = {{ value_var }},
    sort_var  = {{ sort_var }},
    color_var = {{ color_var }},
    hlines    = hlines,
    title     = title
  )
  render_waterfall(engine, pd)
}

#' Prepare waterfall data
#'
#' Resolves the column arguments, validates them, sorts, and returns the
#' canonical structure the renderers consume. Exported so that engine authors
#' and tests can build a `floaties_data_waterfall` without drawing anything.
#'
#' The result is a data frame with columns `.id` (a factor already in plot
#' order), `.value` and `.group`, carrying `hlines` and `title` as attributes.
#'
#' @inheritParams waterfall
#'
#' @return An object of class `floaties_data_waterfall`.
#'
#' @examples
#' subjects <- data.frame(id = c("a", "b"), change = c(10, -20))
#' prepare_waterfall(subjects, id, change)
#'
#' @export
prepare_waterfall <- function(data,
                              x_var,
                              value_var,
                              sort_var = NULL,
                              color_var = NULL,
                              hlines = NULL,
                              title = NULL) {
  check_data(data)

  x_q <- rlang::enquo(x_var)
  v_q <- rlang::enquo(value_var)
  s_q <- rlang::enquo(sort_var)
  c_q <- rlang::enquo(color_var)

  id    <- as.character(eval_col(x_q, data, "x_var"))
  value <- as_numeric_col(eval_col(v_q, data, "value_var"), "value_var")
  group <- eval_col(c_q, data, "color_var", required = FALSE)
  group <- if (is.null(group)) rep(NA_character_, length(id)) else as.character(group)

  # NULL means "sort by value", never "do not sort". FALSE keeps input order.
  if (quo_is_false(s_q)) {
    sort_by <- seq_along(value)
    decreasing <- FALSE
  } else if (rlang::quo_is_null(s_q)) {
    sort_by <- value
    decreasing <- TRUE
  } else {
    sort_by <- as_numeric_col(eval_col(s_q, data, "sort_var"), "sort_var")
    decreasing <- TRUE
  }

  out <- data.frame(
    .id = id, .value = value, .group = group,
    stringsAsFactors = FALSE
  )

  keep <- !is.na(out$.value)
  warn_dropped(sum(!keep), "`value_var` was missing")
  out <- out[keep, , drop = FALSE]
  sort_by <- sort_by[keep]

  n_dup <- sum(duplicated(out$.id))
  if (n_dup > 0L) {
    rlang::warn(sprintf(
      "%d duplicated value(s) of `x_var`: a waterfall expects one bar per subject.",
      n_dup
    ))
  }

  ord <- order(sort_by, decreasing = decreasing)
  out <- out[ord, , drop = FALSE]
  out$.id <- factor(out$.id, levels = unique(out$.id))
  rownames(out) <- NULL

  out <- structure(
    out,
    hlines = hlines,
    title  = title,
    class  = c("floaties_data_waterfall", "data.frame")
  )
  validate_waterfall_data(out)
}

#' Validate canonical waterfall data
#'
#' The contract every `render_waterfall()` method may rely on, made
#' machine-checkable. Called at the end of [prepare_waterfall()] and again on
#' entry to [render_waterfall()].
#'
#' @param x An object to check.
#'
#' @return `x`, invisibly. Errors if the contract is broken.
#'
#' @examples
#' validate_waterfall_data(prepare_waterfall(data.frame(i = "a", v = 1), i, v))
#'
#' @export
validate_waterfall_data <- function(x) {
  if (!inherits(x, "floaties_data_waterfall")) {
    rlang::abort("`x` must be a <floaties_data_waterfall>.")
  }
  missing <- setdiff(c(".id", ".value", ".group"), names(x))
  if (length(missing) > 0L) {
    rlang::abort(paste0(
      "<floaties_data_waterfall> is missing column(s): ",
      paste(missing, collapse = ", ")
    ))
  }
  if (!is.factor(x$.id)) {
    rlang::abort("`.id` must be a factor holding the plot order.")
  }
  if (!is.numeric(x$.value)) {
    rlang::abort("`.value` must be numeric.")
  }
  invisible(x)
}

#' @export
print.floaties_data_waterfall <- function(x, ...) {
  cat("<floaties_data_waterfall: ", nrow(x), " bar(s)>\n", sep = "")
  print(as.data.frame(utils::head(x, 5)))
  if (nrow(x) > 5L) cat("...\n")
  invisible(x)
}

#' Render a prepared waterfall
#'
#' The engine-specific half of the pipeline, and the only part of the package
#' that knows about a plotting library. Write methods for this to add an engine;
#' nothing else has to change.
#'
#' Methods receive canonical data and may rely on the contract enforced by
#' [validate_waterfall_data()]: `.id` is a factor in plot order, `.value` is
#' numeric, `.group` is character and may be all `NA`. Reference lines and the
#' title are attributes.
#'
#' @param engine A [floaties engine][engines]; the object dispatched on.
#' @param x Canonical data from [prepare_waterfall()].
#' @param ... Passed to methods.
#'
#' @return The engine's own plot object.
#'
#' @examples
#' pd <- prepare_waterfall(data.frame(i = c("a", "b"), v = c(1, -2)), i, v)
#' render_waterfall(eng_ggplot2(), pd)
#'
#' @export
render_waterfall <- function(engine, x, ...) {
  check_engine(engine)
  validate_waterfall_data(x)
  UseMethod("render_waterfall")
}

#' @export
render_waterfall.default <- function(engine, x, ...) {
  rlang::abort(c(
    sprintf("No waterfall renderer for engine <%s>.", engine_name(engine)),
    i = "Implement render_waterfall.floaties_<name>() to add one."
  ))
}
