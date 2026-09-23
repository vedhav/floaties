#' Swimlane plot
#'
#' One row per subject, showing what happened and when. Intervals such as
#' dosing periods are drawn as bars along the lane; point events such as
#' adverse events or milestones are drawn as markers on it.
#'
#' The data is one long data frame with one row per interval or event. A row
#' with no end, or a missing end, is a point event. Bringing ADSL, ADAE and ADEX
#' together into that shape is the caller's job: it is one `rbind()` they
#' understand, and it keeps this package out of the business of knowing which
#' datasets exist.
#'
#' @param data A data frame, one row per interval or event.
#' @param id_var Column identifying the subject. One lane each.
#' @param start_var Numeric column giving the start time, typically a study day.
#' @param end_var Optional numeric column giving the end time. Rows where this
#'   is missing are drawn as point events. Omit it entirely for data that is
#'   only events.
#' @param type_var Optional column naming the kind of thing each row is, e.g.
#'   `"dose"` or `"adverse event"`. Drives colour and the legend.
#' @param sort_lanes How to order the lanes, top to bottom. `"duration"`, the
#'   default, puts the longest time on study at the top; `"id"` sorts
#'   alphabetically; `"input"` keeps the order of first appearance.
#' @param vlines Numeric vector of reference line positions, in the units of
#'   `start_var`.
#' @param title Plot title, or `NULL` for none.
#' @param engine A [floaties engine][engines].
#'
#' @return The engine's own plot object.
#'
#' @examples
#' events <- data.frame(
#'   subject = c("01-001", "01-001", "01-002", "01-002", "01-003"),
#'   day     = c(1, 15, 1, 22, 1),
#'   stop    = c(84, NA, 56, NA, 120),
#'   what    = c("dosing", "adverse event", "dosing", "adverse event", "dosing")
#' )
#'
#' swimlane(events, subject, day, stop, what)
#'
#' @seealso [waterfall()]
#' @export
swimlane <- function(data,
                     id_var,
                     start_var,
                     end_var = NULL,
                     type_var = NULL,
                     sort_lanes = c("duration", "id", "input"),
                     vlines = NULL,
                     title = NULL,
                     engine = eng_ggplot2()) {
  check_engine(engine)

  pd <- prepare_swimlane(
    data,
    id_var     = {{ id_var }},
    start_var  = {{ start_var }},
    end_var    = {{ end_var }},
    type_var   = {{ type_var }},
    sort_lanes = sort_lanes,
    vlines     = vlines,
    title      = title
  )
  render_swimlane(engine, pd)
}

#' Prepare swimlane data
#'
#' Resolves the column arguments, validates them, orders the lanes, and returns
#' the canonical structure the renderers consume.
#'
#' The result is a data frame with columns `.id` (a factor whose level order is
#' the lane order, top to bottom), `.start`, `.end` (`NA` for point events) and
#' `.type`, carrying `vlines` and `title` as attributes.
#'
#' @inheritParams swimlane
#'
#' @return An object of class `floaties_data_swimlane`.
#'
#' @examples
#' events <- data.frame(s = c("a", "b"), d = c(1, 3), e = c(10, NA))
#' prepare_swimlane(events, s, d, e)
#'
#' @export
prepare_swimlane <- function(data,
                             id_var,
                             start_var,
                             end_var = NULL,
                             type_var = NULL,
                             sort_lanes = c("duration", "id", "input"),
                             vlines = NULL,
                             title = NULL) {
  check_data(data)
  sort_lanes <- match.arg(sort_lanes)

  i_q <- rlang::enquo(id_var)
  s_q <- rlang::enquo(start_var)
  e_q <- rlang::enquo(end_var)
  t_q <- rlang::enquo(type_var)

  id    <- as.character(eval_col(i_q, data, "id_var"))
  start <- as_numeric_col(eval_col(s_q, data, "start_var"), "start_var")

  end <- eval_col(e_q, data, "end_var", required = FALSE)
  end <- if (is.null(end)) rep(NA_real_, length(id)) else as_numeric_col(end, "end_var")

  type <- eval_col(t_q, data, "type_var", required = FALSE)
  type <- if (is.null(type)) rep(NA_character_, length(id)) else as.character(type)

  out <- data.frame(
    .id = id, .start = start, .end = end, .type = type,
    stringsAsFactors = FALSE
  )

  keep <- !is.na(out$.start)
  warn_dropped(sum(!keep), "`start_var` was missing")
  out <- out[keep, , drop = FALSE]

  backwards <- !is.na(out$.end) & out$.end < out$.start
  if (any(backwards)) {
    rlang::warn(sprintf(
      "%d interval(s) end before they start; treating them as point events.",
      sum(backwards)
    ))
    out$.end[backwards] <- NA_real_
  }

  out$.id <- factor(out$.id, levels = lane_order(out, sort_lanes))
  out <- out[order(out$.id, out$.start), , drop = FALSE]
  rownames(out) <- NULL

  out <- structure(
    out,
    vlines = vlines,
    title  = title,
    class  = c("floaties_data_swimlane", "data.frame")
  )
  validate_swimlane_data(out)
}

#' Lane order, top to bottom
#'
#' Returned as factor levels. The first level is drawn at the bottom by
#' ggplot2's discrete scale, so renderers reverse as needed.
#'
#' @noRd
lane_order <- function(x, how) {
  ids <- unique(x$.id)
  if (length(ids) == 0L) return(character(0))

  switch(
    how,
    id    = sort(ids),
    input = ids,
    duration = {
      last <- vapply(ids, function(i) {
        rows <- x[x$.id == i, , drop = FALSE]
        max(c(rows$.start, rows$.end[!is.na(rows$.end)]))
      }, numeric(1))
      ids[order(last)]
    }
  )
}

#' Validate canonical swimlane data
#'
#' The contract every `render_swimlane()` method may rely on.
#'
#' @param x An object to check.
#'
#' @return `x`, invisibly. Errors if the contract is broken.
#'
#' @examples
#' validate_swimlane_data(prepare_swimlane(data.frame(s = "a", d = 1), s, d))
#'
#' @export
validate_swimlane_data <- function(x) {
  if (!inherits(x, "floaties_data_swimlane")) {
    rlang::abort("`x` must be a <floaties_data_swimlane>.")
  }
  missing <- setdiff(c(".id", ".start", ".end", ".type"), names(x))
  if (length(missing) > 0L) {
    rlang::abort(paste0(
      "<floaties_data_swimlane> is missing column(s): ",
      paste(missing, collapse = ", ")
    ))
  }
  if (!is.factor(x$.id)) {
    rlang::abort("`.id` must be a factor holding the lane order.")
  }
  if (!is.numeric(x$.start) || !is.numeric(x$.end)) {
    rlang::abort("`.start` and `.end` must be numeric.")
  }
  if (any(is.na(x$.start))) {
    rlang::abort("`.start` must not contain missing values.")
  }
  invisible(x)
}

#' @export
print.floaties_data_swimlane <- function(x, ...) {
  n_int <- sum(!is.na(x$.end))
  cat(
    "<floaties_data_swimlane: ", nlevels(x$.id), " lane(s), ",
    n_int, " interval(s), ", nrow(x) - n_int, " event(s)>\n",
    sep = ""
  )
  print(as.data.frame(utils::head(x, 5)))
  if (nrow(x) > 5L) cat("...\n")
  invisible(x)
}

#' Render a prepared swimlane
#'
#' The engine-specific half of the pipeline. Write methods for this to add an
#' engine; nothing else has to change.
#'
#' Methods receive canonical data and may rely on the contract enforced by
#' [validate_swimlane_data()]: `.id` is a factor in lane order, `.start` is
#' numeric and complete, `.end` is numeric and `NA` for point events, `.type` is
#' character and may be all `NA`.
#'
#' @param engine A [floaties engine][engines]; the object dispatched on.
#' @param x Canonical data from [prepare_swimlane()].
#' @param ... Passed to methods.
#'
#' @return The engine's own plot object.
#'
#' @examples
#' pd <- prepare_swimlane(data.frame(s = "a", d = 1, e = 9), s, d, e)
#' render_swimlane(eng_ggplot2(), pd)
#'
#' @export
render_swimlane <- function(engine, x, ...) {
  check_engine(engine)
  validate_swimlane_data(x)
  UseMethod("render_swimlane")
}

#' @export
render_swimlane.default <- function(engine, x, ...) {
  rlang::abort(c(
    sprintf("No swimlane renderer for engine <%s>.", engine_name(engine)),
    i = "Implement render_swimlane.floaties_<name>() to add one."
  ))
}

#' Split canonical swimlane data into intervals and events
#'
#' Every renderer needs this, so it lives here rather than three times over.
#' @noRd
split_lanes <- function(x) {
  list(
    intervals = as.data.frame(x[!is.na(x$.end), , drop = FALSE]),
    events    = as.data.frame(x[is.na(x$.end), , drop = FALSE])
  )
}
