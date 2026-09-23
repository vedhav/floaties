#' Swimlane plot of time on treatment and events
#'
#' Draws a swimlane plot: one horizontal bar (lane) per subject showing time
#' on treatment, sorted with the longest at the top, and points marking
#' events such as tumor response assessments along each lane.
#'
#' `data` is in long format: one row per event, repeating the subject's
#' `duration` (and `fill`) on each row. Subjects without events have a single
#' row with `time` and `event` set to `NA`. [sdtm_swimlane_data()] returns
#' data in this shape.
#'
#' When every event is a RECIST response (`CR`, `PR`, `SD`, `NON-CR/NON-PD`,
#' `PD`, `NE`) or `Death`, standard colors are used: green for response,
#' red for progression, and a cross for death.
#'
#' The plot can be drawn with ggplot2 (static, the default), plotly, or
#' echarts4r. The interactive engines show lane and event details on hover
#' and use the same colours as the ggplot2 version.
#'
#' @param data A data frame, see Details.
#' @param id <[`data-masking`][ggplot2::aes_eval]> Column that identifies
#'   subjects.
#' @param duration <[`data-masking`][ggplot2::aes_eval]> Numeric column with
#'   the length of each subject's lane. Must be the same on every row of a
#'   subject. Subjects with a missing value are dropped with a warning.
#' @param time,event <[`data-masking`][ggplot2::aes_eval]> Optional columns
#'   with the time and type of each event. Supply both or neither.
#' @param fill <[`data-masking`][ggplot2::aes_eval]> Optional categorical
#'   column used to color the lanes, such as treatment arm.
#' @param xlab,ylab,title Axis labels and plot title.
#' @param engine Plotting package to use: `"ggplot2"`, `"plotly"`, or
#'   `"echarts4r"`. The interactive engines need their package installed.
#'
#' @return A [ggplot2::ggplot] object for `engine = "ggplot2"`, otherwise a
#'   plotly or echarts4r htmlwidget. Each can be customized further with its
#'   own package's functions.
#' @export
#'
#' @examples
#' events <- data.frame(
#'   subject = c("S1", "S1", "S1", "S2", "S2", "S3"),
#'   weeks = c(24, 24, 24, 18, 18, 12),
#'   arm = c("A", "A", "A", "B", "B", "A"),
#'   time = c(6, 12, 18, 6, 12, NA),
#'   response = c("SD", "PR", "CR", "SD", "PD", NA)
#' )
#'
#' swimlane_plot(events, subject, weeks, time, response, fill = arm)
#'
#' @examplesIf requireNamespace("plotly", quietly = TRUE)
#' swimlane_plot(events, subject, weeks, time, response, fill = arm, engine = "plotly")
#'
#' @examplesIf requireNamespace("echarts4r", quietly = TRUE)
#' swimlane_plot(events, subject, weeks, time, response, fill = arm, engine = "echarts4r")
swimlane_plot <- function(
  data,
  id,
  duration,
  time = NULL,
  event = NULL,
  fill = NULL,
  xlab = "Time since first dose",
  ylab = "Subject",
  title = NULL,
  engine = c("ggplot2", "plotly", "echarts4r")
) {
  engine <- match.arg(engine)
  check_engine(engine)
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  has_time <- !rlang::quo_is_null(rlang::enquo(time))
  has_event <- !rlang::quo_is_null(rlang::enquo(event))
  has_fill <- !rlang::quo_is_null(rlang::enquo(fill))
  if (has_time != has_event) {
    stop("`time` and `event` must be supplied together.", call. = FALSE)
  }

  ids <- rlang::eval_tidy(rlang::enquo(id), data)
  durations <- rlang::eval_tidy(rlang::enquo(duration), data)
  if (!is.numeric(durations)) {
    stop("`duration` must be a numeric column.", call. = FALSE)
  }
  if (any(tapply(durations, ids, function(x) length(unique(x))) > 1)) {
    stop("`duration` must be the same on every row of a subject.", call. = FALSE)
  }

  missing <- is.na(durations)
  if (any(missing)) {
    warning(
      sprintf(
        "Removed %d subject(s) with missing `duration`.",
        length(unique(ids[missing]))
      ),
      call. = FALSE
    )
    data <- data[!missing, , drop = FALSE]
    ids <- ids[!missing]
    durations <- durations[!missing]
  }

  # Discrete y axes run bottom to top, so shortest first puts longest on top.
  first <- !duplicated(ids)
  lane_order <- ids[first][order(durations[first])]
  data$.lane <- factor(ids, levels = lane_order)
  data$.duration <- durations
  if (has_fill) {
    data$.fill <- rlang::eval_tidy(rlang::enquo(fill), data)
  }
  lanes <- data[first, , drop = FALSE]

  events <- NULL
  if (has_event) {
    data$.time <- rlang::eval_tidy(rlang::enquo(time), data)
    data$.event <- rlang::eval_tidy(rlang::enquo(event), data)
    events <- data[!is.na(data$.time) & !is.na(data$.event), , drop = FALSE]
  }

  labels <- list(
    id = rlang::as_label(rlang::enquo(id)),
    duration = rlang::as_label(rlang::enquo(duration)),
    time = if (has_time) rlang::as_label(rlang::enquo(time)),
    event = if (has_event) rlang::as_label(rlang::enquo(event)),
    fill = if (has_fill) rlang::as_label(rlang::enquo(fill)),
    x = xlab,
    y = ylab,
    title = title
  )

  render <- switch(
    engine,
    ggplot2 = swimlane_ggplot,
    plotly = swimlane_plotly,
    echarts4r = swimlane_echarts
  )
  render(lanes, events, has_fill, labels)
}

swimlane_ggplot <- function(lanes, events, has_fill, labels) {
  p <- ggplot2::ggplot(
    lanes,
    ggplot2::aes(x = .data$.duration, y = .data$.lane)
  )

  if (has_fill) {
    p <- p +
      ggplot2::geom_col(ggplot2::aes(fill = .data$.fill), width = 0.6) +
      ggplot2::scale_fill_brewer(palette = "Pastel1")
  } else {
    p <- p + ggplot2::geom_col(fill = "grey80", width = 0.6)
  }

  if (!is.null(events)) {
    styles <- event_styles(events$.event)
    if (styles$known) {
      p <- p +
        ggplot2::geom_point(
          data = events,
          ggplot2::aes(x = .data$.time, colour = .data$.event, shape = .data$.event),
          size = 2.5
        ) +
        ggplot2::scale_colour_manual(values = event_colours, limits = styles$levels) +
        ggplot2::scale_shape_manual(values = event_shapes, limits = styles$levels)
    } else {
      p <- p +
        ggplot2::geom_point(
          data = events,
          ggplot2::aes(x = .data$.time, colour = .data$.event),
          size = 2.5
        )
    }
  }

  p +
    ggplot2::labs(
      x = labels$x,
      y = labels$y,
      title = labels$title,
      fill = labels$fill,
      colour = labels$event,
      shape = labels$event
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}

# Lanes grouped by fill level: one trace or series per level, in legend order.
lane_groups <- function(lanes, has_fill) {
  if (!has_fill) {
    return(list(
      values = rep("all", nrow(lanes)),
      levels = "all",
      colours = c(all = "#CCCCCC")
    ))
  }
  levels <- group_levels(lanes$.fill)
  list(
    values = group_values(lanes$.fill),
    levels = levels,
    colours = group_colours(levels, pastel_colours)
  )
}

lane_hover <- function(lanes, has_fill, labels, br) {
  hover <- paste0(
    labels$id, ": ", lanes$.lane,
    br, labels$duration, ": ", format_number(lanes$.duration)
  )
  if (has_fill) {
    hover <- paste0(hover, br, labels$fill, ": ", group_values(lanes$.fill))
  }
  hover
}

swimlane_plotly <- function(lanes, events, has_fill, labels) {
  groups <- lane_groups(lanes, has_fill)
  hover <- lane_hover(lanes, has_fill, labels, "<br>")

  p <- plotly::plot_ly()
  for (i in seq_along(groups$levels)) {
    rows <- groups$values == groups$levels[i]
    p <- plotly::add_bars(
      p,
      x = lanes$.duration[rows],
      y = as.character(lanes$.lane[rows]),
      orientation = "h",
      name = groups$levels[i],
      marker = list(color = groups$colours[[groups$levels[i]]]),
      hovertext = hover[rows],
      hoverinfo = "text",
      textposition = "none",
      legendgroup = "lanes",
      legendgrouptitle = if (i == 1) list(text = labels$fill),
      showlegend = has_fill
    )
  }

  if (!is.null(events)) {
    styles <- event_styles(events$.event)
    values <- group_values(events$.event)
    hover <- paste0(
      labels$id, ": ", events$.lane,
      "<br>", labels$event, ": ", values,
      "<br>", labels$time, ": ", format_number(events$.time)
    )
    for (i in seq_along(styles$levels)) {
      level <- styles$levels[i]
      rows <- values == level
      p <- plotly::add_markers(
        p,
        x = events$.time[rows],
        y = as.character(events$.lane[rows]),
        name = level,
        marker = list(
          color = styles$colour[[level]],
          symbol = styles$plotly[[level]],
          size = 9
        ),
        hovertext = hover[rows],
        hoverinfo = "text",
        legendgroup = "events",
        legendgrouptitle = if (i == 1) list(text = labels$event)
      )
    }
  }

  plotly::layout(
    p,
    barmode = "relative",
    bargap = 0.4,
    title = plotly_title(labels$title),
    xaxis = list(title = labels$x, rangemode = "tozero"),
    yaxis = list(
      title = labels$y,
      type = "category",
      categoryorder = "array",
      categoryarray = levels(lanes$.lane)
    )
  )
}

swimlane_echarts <- function(lanes, events, has_fill, labels) {
  lanes <- lanes[order(lanes$.lane), , drop = FALSE]
  groups <- lane_groups(lanes, has_fill)

  # One column per fill level, NA elsewhere, stacked so lanes stay centred.
  wide <- data.frame(.y = as.character(lanes$.lane))
  series <- paste0("s", seq_along(groups$levels))
  for (i in seq_along(groups$levels)) {
    wide[[series[i]]] <- ifelse(groups$values == groups$levels[i], lanes$.duration, NA)
  }

  p <- echarts4r::e_charts_(wide, ".y", reorder = FALSE)
  for (i in seq_along(groups$levels)) {
    p <- echarts4r::e_bar_(
      p,
      series[i],
      name = groups$levels[i],
      stack = "lanes",
      barCategoryGap = "40%",
      itemStyle = list(color = groups$colours[[groups$levels[i]]])
    )
  }
  p <- echarts4r::e_flip_coords(p)

  # Events are added as raw series: e_data() would re-sort the lane axis.
  if (!is.null(events)) {
    styles <- event_styles(events$.event)
    values <- group_values(events$.event)
    for (level in styles$levels) {
      rows <- which(values == level)
      p$x$opts$series <- c(p$x$opts$series, list(list(
        type = "scatter",
        name = level,
        data = lapply(rows, function(r) {
          list(value = list(events$.time[r], as.character(events$.lane[r])))
        }),
        symbol = styles$echarts[[level]],
        symbolSize = 10,
        itemStyle = list(color = styles$colour[[level]]),
        z = 3
      )))
    }
  }

  formatter <- sprintf(
    "function(p) {
      var t = (+p.value[0]).toFixed(1);
      if (p.seriesType === 'bar') {
        var s = %s + ': ' + p.value[1] + '<br/>' + %s + ': ' + t;
        if (%s) s += '<br/>' + %s + ': ' + p.seriesName;
        return s;
      }
      return %s + ': ' + p.value[1] + '<br/>' + %s + ': ' + p.seriesName +
        '<br/>' + %s + ': ' + t;
    }",
    js_string(labels$id),
    js_string(labels$duration),
    tolower(has_fill),
    js_string(if (has_fill) labels$fill else ""),
    js_string(labels$id),
    js_string(if (is.null(labels$event)) "" else labels$event),
    js_string(if (is.null(labels$time)) "" else labels$time)
  )

  p$x$opts$xAxis <- list(list(
    type = "value",
    name = labels$x,
    nameLocation = "middle",
    nameGap = 30
  ))
  p$x$opts$yAxis[[1]]$name <- labels$y
  p$x$opts$yAxis[[1]]$nameLocation <- "end"
  p <- echarts4r::e_grid(p, containLabel = TRUE, left = 20)
  p <- echarts4r::e_tooltip(p, trigger = "item", formatter = htmlwidgets::JS(formatter))
  p <- echarts_legend(p, c(
    if (has_fill) groups$levels,
    if (!is.null(events)) event_styles(events$.event)$levels
  ))
  if (!is.null(labels$title)) {
    p <- echarts4r::e_title(p, labels$title)
  }
  p
}
