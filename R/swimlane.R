#' Swimlane plot
#'
#' @description
#' Draw a swimlane plot: one horizontal lane per subject, for example time on
#' treatment, with optional markers for events such as responses,
#' progression, or death.
#'
#' `plot_swimlane()` works with any data standard. It takes two data frames:
#'
#' * `data` has **one row per subject** and describes the lanes.
#' * `events` (optional) has any number of rows per subject and describes the
#'   markers.
#'
#' All times must be numeric and in the same unit (for example days or months
#' from the start of treatment). Derive them before calling the function.
#'
#' `prep_swimlane()` runs the data preparation step on its own and returns the
#' exact data the plot is drawn from. This helps with QC.
#'
#' @param data A data frame with one row per subject.
#' @param ... Arguments passed on to `prep_swimlane()`.
#' @param engine Graphics engine: `"ggplot2"`, `"plotly"`, or `"echarts4r"`.
#' @param subject <[`tidy-select`][tidyselect::language]> Column that
#'   identifies each subject. Must not contain missing values.
#' @param end <[`tidy-select`][tidyselect::language]> Numeric column with the
#'   time each lane ends. Rows where it is missing are dropped with a warning.
#' @param start <[`tidy-select`][tidyselect::language]> Optional numeric column
#'   with the time each lane starts. Lanes start at 0 if it is not given.
#' @param fill <[`tidy-select`][tidyselect::language]> Optional column used to
#'   color the lanes, for example the treatment arm. Factor level order is
#'   kept; missing values are shown as `"Missing"`.
#' @param ongoing <[`tidy-select`][tidyselect::language]> Optional column
#'   flagging subjects who are still ongoing; their lanes end with an
#'   `"Ongoing"` arrow marker. Either logical, or character where `"Y"` means
#'   ongoing (as in CDISC flags). Missing values count as not ongoing.
#' @param tooltip <[`tidy-select`][tidyselect::language]> Optional extra columns
#'   of `data` shown in the lane tooltip. Tooltips are only used by interactive
#'   engines.
#' @param events Optional data frame of events to draw as markers.
#' @param event_time <[`tidy-select`][tidyselect::language]> Numeric column of
#'   `events` with the time of each event. Rows where it is missing are dropped
#'   with a warning. Required when `events` is given.
#' @param event <[`tidy-select`][tidyselect::language]> Column of `events` with
#'   the event type, used for both marker shape and color. Required when
#'   `events` is given.
#' @param event_subject <[`tidy-select`][tidyselect::language]> Column of
#'   `events` that identifies the subject. Defaults to the column with the same
#'   name as `subject`. Events for subjects not in `data` are dropped with a
#'   warning.
#' @param sort How to order the lanes from top to bottom: `"descending"`
#'   (longest lane at the top, the default), `"ascending"`, or `"none"` (keep
#'   the row order of `data`).
#' @param ref_lines Optional numeric vector of times for dashed vertical
#'   reference lines.
#' @param colors Optional colors for the `fill` levels: either a named
#'   character vector (names are levels) or an unnamed one used in level order.
#' @param event_colors Optional colors for the `event` levels, given the same
#'   way as `colors`.
#' @param event_shapes Optional shapes for the `event` levels, given the same
#'   way as `colors`. Use `"circle"`, `"square"`, `"triangle"`, `"diamond"`,
#'   `"cross"`, `"star"`, or `"arrow"`.
#' @param title Optional plot title.
#' @param x_label,y_label Optional axis titles. By default the `label`
#'   attribute of the `end` and `subject` columns is used, or the column name
#'   if there is none.
#'
#' @returns
#' * `plot_swimlane()`: the engine's own object, a `ggplot` for ggplot2 or an
#'   htmlwidget for plotly and echarts4r, which you can keep customizing.
#' * `prep_swimlane()`: a `floaties_swimlane_spec` object. Its `data` element
#'   has one row per subject in plot order (top to bottom), with columns
#'   `.subject`, `.start`, `.end`, `.fill`, `.ongoing`, and `.tooltip`. Its
#'   `events` element has columns `.subject`, `.time`, `.event`, and
#'   `.tooltip`, including one `"Ongoing"` row at the end of each ongoing
#'   lane, or is `NULL`.
#'
#' @export
#' @examples
#' plot_swimlane(
#'   treatment_duration,
#'   subject = patient,
#'   end = months,
#'   fill = arm,
#'   ongoing = ongoing,
#'   events = response_events,
#'   event_time = month,
#'   event = event
#' )
#'
#' plot_swimlane(
#'   treatment_duration,
#'   subject = patient,
#'   end = months,
#'   fill = arm,
#'   ongoing = ongoing,
#'   events = response_events,
#'   event_time = month,
#'   event = event,
#'   event_shapes = c(
#'     CR = "star", PR = "triangle", PD = "square", Death = "cross"
#'   ),
#'   engine = "plotly"
#' )
#'
#' plot_swimlane(
#'   treatment_duration,
#'   subject = patient,
#'   end = months,
#'   ongoing = ongoing,
#'   events = response_events,
#'   event_time = month,
#'   event = event,
#'   engine = "echarts4r"
#' )
#'
#' # Inspect the data behind the plot
#' prep_swimlane(treatment_duration, subject = patient, end = months)
plot_swimlane <- function(data,
                          ...,
                          engine = c("ggplot2", "plotly", "echarts4r")) {
  engine <- rlang::arg_match(engine)
  render_spec(prep_swimlane(data, ...), engine)
}

#' @rdname plot_swimlane
#' @export
prep_swimlane <- function(data,
                          subject,
                          end,
                          start = NULL,
                          fill = NULL,
                          ongoing = NULL,
                          tooltip = NULL,
                          events = NULL,
                          event_time = NULL,
                          event = NULL,
                          event_subject = NULL,
                          sort = c("descending", "ascending", "none"),
                          ref_lines = NULL,
                          colors = NULL,
                          event_colors = NULL,
                          event_shapes = NULL,
                          title = NULL,
                          x_label = NULL,
                          y_label = NULL) {
  assert_arg(checkmate::check_data_frame(data), "data")
  assert_arg(checkmate::check_data_frame(events, null.ok = TRUE), "events")
  sort <- rlang::arg_match(sort)
  assert_arg(
    checkmate::check_numeric(
      ref_lines,
      any.missing = FALSE, finite = TRUE, null.ok = TRUE
    ),
    "ref_lines"
  )
  check_labels(title, x_label, y_label)

  subject_col <- resolve_column(data, rlang::enquo(subject), "subject")
  end_col <- resolve_column(
    data, rlang::enquo(end), "end",
    check = checkmate::check_numeric
  )
  start_col <- resolve_column(
    data, rlang::enquo(start), "start",
    required = FALSE, check = checkmate::check_numeric
  )
  fill_col <- resolve_column(data, rlang::enquo(fill), "fill", required = FALSE)
  ongoing_col <- resolve_column(
    data, rlang::enquo(ongoing), "ongoing",
    required = FALSE,
    check = function(x) {
      checkmate::check_multi_class(x, c("logical", "character", "factor"))
    }
  )
  tooltip_cols <- resolve_columns(data, rlang::enquo(tooltip))
  col_labels <- column_labels(data)

  data <- drop_missing(data, c(start_col, end_col), subject_col)
  check_subjects(data, subject_col)
  data <- data[order_rows(data[[end_col]], sort), , drop = FALSE]

  lane_start <- if (is.null(start_col)) 0 else data[[start_col]]
  backwards <- data[[end_col]] < lane_start
  if (any(backwards)) {
    cli::cli_abort(
      c(
        "{.arg end} must not be before {.arg start}.",
        "x" = "Lanes end before they start for
               {.val {data[[subject_col]][backwards]}}."
      ),
      class = "floaties_error_invalid_range"
    )
  }

  ongoing_flag <- if (is.null(ongoing_col)) {
    rep(FALSE, nrow(data))
  } else if (is.logical(data[[ongoing_col]])) {
    data[[ongoing_col]] %in% TRUE
  } else {
    as.character(data[[ongoing_col]]) %in% "Y"
  }

  subjects <- as.character(data[[subject_col]])
  fill_values <- if (is.null(fill_col)) {
    factor(rep("All subjects", nrow(data)))
  } else {
    as_level_factor(data[[fill_col]])
  }
  fill_levels <- levels(fill_values)

  lanes <- data.frame(
    .subject = factor(subjects, levels = subjects),
    .start = lane_start,
    .end = data[[end_col]],
    .fill = fill_values,
    .ongoing = ongoing_flag,
    .tooltip = build_tooltip(
      data,
      unique(c(
        subject_col, start_col, end_col, fill_col, ongoing_col, tooltip_cols
      )),
      col_labels
    ),
    row.names = NULL
  )

  marks <- NULL
  event_label <- NULL
  if (!is.null(events)) {
    event_subject <- rlang::enquo(event_subject)
    if (rlang::quo_is_null(event_subject)) {
      event_subject <- rlang::quo(!!subject_col)
    }
    prepped <- prep_swimlane_events(
      events,
      subjects = subjects,
      subject = event_subject,
      time = rlang::enquo(event_time),
      event = rlang::enquo(event)
    )
    marks <- prepped$data
    event_label <- prepped$label
  }
  event_levels <- levels(marks$.event)
  scales <- list(
    fill = resolve_scale(
      colors, fill_levels, default_colors(fill_levels, light = TRUE), "color"
    ),
    event_color = resolve_scale(
      event_colors, event_levels, default_colors(event_levels), "color"
    ),
    event_shape = resolve_scale(
      event_shapes, event_levels, default_shapes(event_levels), "shape"
    )
  )
  assert_arg(
    checkmate::check_subset(unname(scales$event_shape), names(shape_map)),
    "event_shapes"
  )

  # Ongoing subjects get an "Ongoing" arrow marker at the end of their lane,
  # drawn like any other event.
  if (any(ongoing_flag)) {
    marks <- rbind(marks, data.frame(
      .subject = lanes$.subject[ongoing_flag],
      .time = lanes$.end[ongoing_flag],
      .event = factor("Ongoing"),
      .tooltip = lanes$.tooltip[ongoing_flag]
    ))
    scales$event_color[["Ongoing"]] <- "#404040"
    scales$event_shape[["Ongoing"]] <- "arrow"
  }

  new_floaties_spec(
    "swimlane",
    data = lanes,
    events = marks,
    scales = scales,
    labels = list(
      title = title,
      x = x_label %||% col_labels[[end_col]],
      y = y_label %||% col_labels[[subject_col]],
      fill = if (!is.null(fill_col)) col_labels[[fill_col]],
      event = event_label
    ),
    ref_lines = ref_lines
  )
}

# Resolve and clean the events data. Returns the marker rows and the event
# column's label.
prep_swimlane_events <- function(events,
                                 subjects,
                                 subject,
                                 time,
                                 event,
                                 call = rlang::caller_env()) {
  subject_col <- resolve_column(events, subject, "event_subject", call = call)
  time_col <- resolve_column(
    events, time, "event_time",
    check = checkmate::check_numeric, call = call
  )
  event_col <- resolve_column(events, event, "event", call = call)
  col_labels <- column_labels(events)

  events <- drop_missing(
    events, time_col, subject_col,
    what = "event", allow_empty = TRUE, call = call
  )
  unknown <- !as.character(events[[subject_col]]) %in% subjects
  if (any(unknown)) {
    cli::cli_warn(
      c(
        "Dropped {sum(unknown)} event{?s} for subjects not in {.arg data}.",
        "i" = "Unknown subjects:
               {.val {unique(events[[subject_col]][unknown])}}."
      ),
      class = "floaties_warning_unknown_subjects"
    )
    events <- events[!unknown, , drop = FALSE]
  }

  list(
    data = data.frame(
      .subject = factor(as.character(events[[subject_col]]), levels = subjects),
      .time = events[[time_col]],
      .event = as_level_factor(events[[event_col]]),
      .tooltip = build_tooltip(
        events,
        unique(c(subject_col, event_col, time_col)),
        col_labels
      ),
      row.names = NULL
    ),
    label = col_labels[[event_col]]
  )
}

render_swimlane_ggplot2 <- function(spec) {
  lanes <- spec$data
  events <- spec$events
  labels <- spec$labels

  # The first subject is drawn at the top.
  lanes$.y <- rev(seq_len(nrow(lanes)))

  p <- ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = lanes,
      ggplot2::aes(
        xmin = .data$.start,
        xmax = .data$.end,
        ymin = .data$.y - 0.35,
        ymax = .data$.y + 0.35,
        fill = .data$.fill
      )
    )

  if (length(spec$ref_lines) > 0) {
    p <- p + ggplot2::geom_vline(
      xintercept = spec$ref_lines,
      linetype = "dashed",
      colour = "grey45",
      linewidth = 0.4
    )
  }

  if (!is.null(events) && nrow(events) > 0) {
    events$.y <- lanes$.y[match(events$.subject, lanes$.subject)]
    p <- p +
      ggplot2::geom_point(
        data = events,
        ggplot2::aes(
          x = .data$.time,
          y = .data$.y,
          colour = .data$.event,
          shape = .data$.event
        ),
        size = 2.8
      ) +
      ggplot2::scale_colour_manual(
        values = spec$scales$event_color,
        name = labels$event
      ) +
      ggplot2::scale_shape_manual(
        values = as.numeric(engine_shapes(spec$scales$event_shape, "ggplot2")),
        name = labels$event
      )
  }

  p +
    ggplot2::scale_fill_manual(
      values = spec$scales$fill,
      name = labels$fill,
      guide = if (is.null(labels$fill)) "none" else "legend"
    ) +
    ggplot2::scale_y_continuous(
      breaks = lanes$.y,
      labels = as.character(lanes$.subject),
      expand = ggplot2::expansion(add = 0.6)
    ) +
    ggplot2::labs(title = labels$title, x = labels$x, y = labels$y) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.box = "vertical",
      legend.box.just = "left",
      legend.justification = "left"
    )
}

render_swimlane_plotly <- function(spec) {
  lanes <- spec$data
  events <- spec$events
  labels <- spec$labels

  p <- plotly::plot_ly()
  for (level in levels(lanes$.fill)) {
    level_data <- lanes[lanes$.fill == level, , drop = FALSE]
    p <- plotly::add_bars(
      p,
      y = as.character(level_data$.subject),
      x = level_data$.end - level_data$.start,
      base = level_data$.start,
      orientation = "h",
      name = level,
      marker = list(color = spec$scales$fill[[level]]),
      text = level_data$.tooltip,
      textposition = "none",
      hovertemplate = "%{text}<extra></extra>",
      legendgroup = "lanes",
      legendgrouptitle = list(text = labels$fill),
      showlegend = !is.null(labels$fill)
    )
  }

  symbols <- engine_shapes(spec$scales$event_shape, "plotly")
  for (level in levels(events$.event)) {
    level_data <- events[events$.event == level, , drop = FALSE]
    p <- plotly::add_markers(
      p,
      y = as.character(level_data$.subject),
      x = level_data$.time,
      name = level,
      marker = list(
        symbol = symbols[[level]],
        color = spec$scales$event_color[[level]],
        size = 10,
        line = list(width = 0)
      ),
      text = level_data$.tooltip,
      hovertemplate = "%{text}<extra></extra>",
      legendgroup = "events",
      legendgrouptitle = list(text = labels$event)
    )
  }

  plotly::layout(
    p,
    title = list(text = labels$title, x = 0, xanchor = "left"),
    margin = list(t = if (is.null(labels$title)) 20 else 50),
    barmode = "overlay",
    bargap = 0.3,
    legend = list(orientation = "h", x = 0, y = -0.15),
    xaxis = list(title = labels$x, zeroline = FALSE),
    yaxis = list(
      title = labels$y,
      type = "category",
      categoryorder = "array",
      # plotly draws the first category at the bottom.
      categoryarray = rev(levels(lanes$.subject))
    ),
    shapes = lapply(spec$ref_lines, function(x) {
      list(
        type = "line",
        yref = "paper",
        y0 = 0,
        y1 = 1,
        x0 = x,
        x1 = x,
        line = list(color = "grey", dash = "dash", width = 1)
      )
    })
  )
}

# Draws one lane as a rectangle from `start` to `end`, so lanes can start at
# any time (stacked bars can't handle negative starts).
swimlane_lane_js <- "function(params, api) {
  var y = api.value(0);
  var start = api.coord([api.value(1), y]);
  var end = api.coord([api.value(2), y]);
  var height = api.size([0, 1])[1] * 0.7;
  return {
    type: 'rect',
    shape: {
      x: start[0],
      y: start[1] - height / 2,
      width: end[0] - start[0],
      height: height
    },
    style: api.style()
  };
}"

render_swimlane_echarts4r <- function(spec) {
  lanes <- spec$data
  events <- spec$events
  labels <- spec$labels

  # echarts draws the first category at the bottom.
  categories <- rev(levels(lanes$.subject))
  y_index <- function(subject) match(as.character(subject), categories) - 1

  lane_series <- lapply(levels(lanes$.fill), function(level) {
    level_data <- lanes[lanes$.fill == level, , drop = FALSE]
    list(
      type = "custom",
      name = level,
      renderItem = htmlwidgets::JS(swimlane_lane_js),
      encode = list(x = c(1, 2), y = 0),
      itemStyle = list(color = spec$scales$fill[[level]]),
      data = lapply(seq_len(nrow(level_data)), function(i) {
        list(
          value = list(
            y_index(level_data$.subject[[i]]),
            level_data$.start[[i]],
            level_data$.end[[i]]
          ),
          name = level_data$.tooltip[[i]]
        )
      })
    )
  })
  if (length(spec$ref_lines) > 0) {
    lane_series[[1]]$markLine <- echarts_ref_lines(spec$ref_lines, "xAxis")
  }

  symbols <- engine_shapes(spec$scales$event_shape, "echarts4r")
  event_series <- lapply(levels(events$.event), function(level) {
    level_data <- events[events$.event == level, , drop = FALSE]
    list(
      type = "scatter",
      name = level,
      symbol = symbols[[level]],
      symbolSize = 11,
      itemStyle = list(color = spec$scales$event_color[[level]], opacity = 1),
      data = lapply(seq_len(nrow(level_data)), function(i) {
        list(
          value = list(
            level_data$.time[[i]],
            y_index(level_data$.subject[[i]])
          ),
          name = level_data$.tooltip[[i]]
        )
      })
    )
  })

  legend <- c(
    if (!is.null(labels$fill)) levels(lanes$.fill),
    levels(events$.event)
  )

  echarts4r::e_charts() |>
    echarts4r::e_list(list(
      title = list(text = labels$title %||% ""),
      grid = list(containLabel = TRUE, left = 40, bottom = 60),
      legend = list(show = length(legend) > 0, data = legend, bottom = 0),
      tooltip = echarts_tooltip,
      xAxis = list(
        type = "value",
        name = labels$x,
        nameLocation = "middle",
        nameGap = 25
      ),
      yAxis = list(
        type = "category",
        data = categories,
        name = labels$y,
        nameLocation = "middle",
        nameGap = 60,
        axisTick = list(show = FALSE)
      ),
      series = c(lane_series, event_series)
    ))
}
