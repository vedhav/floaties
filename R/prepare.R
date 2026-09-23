# Validation and reshaping shared by every engine. Each prepare_*() returns
# data with canonical dot-prefixed columns so the engines never touch the
# user's column names.

check_data_frame <- function(x, arg) {
  if (!is.data.frame(x)) {
    stop(sprintf("`%s` must be a data frame.", arg), call. = FALSE)
  }
}

check_column_arg <- function(x, arg, optional = FALSE) {
  if (optional && is.null(x)) {
    return(invisible())
  }
  if (!is.character(x) || length(x) != 1 || is.na(x)) {
    stop(sprintf("`%s` must be a single column name.", arg), call. = FALSE)
  }
}

check_columns <- function(data, cols, arg) {
  missing <- setdiff(cols, names(data))
  if (length(missing) > 0) {
    stop(
      sprintf("Column(s) not found in `%s`: %s.", arg, paste(missing, collapse = ", ")),
      call. = FALSE
    )
  }
}

check_numeric <- function(x, col) {
  if (!is.numeric(x)) {
    stop(sprintf("Column `%s` must be numeric.", col), call. = FALSE)
  }
  if (anyNA(x)) {
    stop(sprintf("Column `%s` must not contain missing values.", col), call. = FALSE)
  }
}

as_group <- function(x) {
  if (is.factor(x)) droplevels(x) else factor(x)
}

prepare_swimlane <- function(data, id, start, end, color = NULL, ongoing = NULL,
                             events = NULL, event_time = NULL, event_type = NULL,
                             sort = TRUE) {
  check_data_frame(data, "data")
  check_column_arg(id, "id")
  check_column_arg(start, "start")
  check_column_arg(end, "end")
  check_column_arg(color, "color", optional = TRUE)
  check_column_arg(ongoing, "ongoing", optional = TRUE)
  check_columns(data, c(id, start, end, color, ongoing), "data")
  check_numeric(data[[start]], start)
  check_numeric(data[[end]], end)

  lanes <- data.frame(
    .id = as.character(data[[id]]),
    .start = data[[start]],
    .end = data[[end]],
    stringsAsFactors = FALSE
  )
  if (anyDuplicated(lanes$.id)) {
    stop("`data` must have one row per subject in `id`.", call. = FALSE)
  }
  if (any(lanes$.end < lanes$.start)) {
    stop(sprintf("`%s` must not be earlier than `%s`.", end, start), call. = FALSE)
  }
  if (!is.null(color)) {
    lanes$.color <- as_group(data[[color]])
  }
  lanes$.ongoing <- if (is.null(ongoing)) FALSE else as.logical(data[[ongoing]]) %in% TRUE

  # Factor levels run bottom-to-top, so the first level is the lowest lane.
  ord <- if (sort) {
    order(lanes$.end - lanes$.start, lanes$.id)
  } else {
    rev(seq_len(nrow(lanes)))
  }
  lanes <- lanes[ord, , drop = FALSE]
  lanes$.id <- factor(lanes$.id, levels = lanes$.id)
  rownames(lanes) <- NULL

  ev <- NULL
  if (!is.null(events)) {
    check_data_frame(events, "events")
    if (is.null(event_time) || is.null(event_type)) {
      stop("`event_time` and `event_type` are required when `events` is supplied.", call. = FALSE)
    }
    check_column_arg(event_time, "event_time")
    check_column_arg(event_type, "event_type")
    check_columns(events, c(id, event_time, event_type), "events")
    check_numeric(events[[event_time]], event_time)

    ev <- data.frame(
      .id = as.character(events[[id]]),
      .time = events[[event_time]],
      stringsAsFactors = FALSE
    )
    ev$.type <- as_group(events[[event_type]])
    unknown <- !ev$.id %in% levels(lanes$.id)
    if (any(unknown)) {
      warning(
        sprintf("Dropping %d event(s) for subjects not present in `data`.", sum(unknown)),
        call. = FALSE
      )
      ev <- ev[!unknown, , drop = FALSE]
      ev$.type <- droplevels(ev$.type)
    }
    ev$.id <- factor(ev$.id, levels = levels(lanes$.id))
    rownames(ev) <- NULL
  }

  list(
    lanes = lanes,
    events = if (!is.null(ev) && nrow(ev) > 0) ev,
    has_color = !is.null(color),
    color_label = color,
    event_label = event_type
  )
}

prepare_waterfall <- function(data, id, value, fill = NULL) {
  check_data_frame(data, "data")
  check_column_arg(id, "id")
  check_column_arg(value, "value")
  check_column_arg(fill, "fill", optional = TRUE)
  check_columns(data, c(id, value, fill), "data")
  check_numeric(data[[value]], value)

  bars <- data.frame(
    .id = as.character(data[[id]]),
    .value = data[[value]],
    stringsAsFactors = FALSE
  )
  if (anyDuplicated(bars$.id)) {
    stop("`data` must have one row per subject in `id`.", call. = FALSE)
  }
  if (!is.null(fill)) {
    bars$.fill <- as_group(data[[fill]])
  }

  # Largest increase on the left, deepest reduction on the right.
  bars <- bars[order(-bars$.value, bars$.id), , drop = FALSE]
  bars$.id <- factor(bars$.id, levels = bars$.id)
  rownames(bars) <- NULL

  list(bars = bars, has_fill = !is.null(fill), fill_label = fill)
}
