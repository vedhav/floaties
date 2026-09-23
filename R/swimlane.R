#' Swimlane plot
#'
#' Draws one horizontal lane per subject spanning its time on study, with
#' optional markers for events (e.g. responses, dose changes) and arrows for
#' subjects who are still ongoing.
#'
#' `swimlane_plot()` dispatches to the generator for the chosen `engine`. The
#' generators can also be called directly: `swimlane_ggplot()` returns a
#' ggplot object, `swimlane_plotly()` a plotly htmlwidget and
#' `swimlane_echarts4r()` an echarts4r htmlwidget. All three take the same
#' arguments and draw the same plot.
#'
#' @param data A data frame with one row per subject.
#' @param id Name of the subject identifier column. The same column name is
#'   used to match `events` to lanes.
#' @param start,end Names of the numeric columns holding the start and end of
#'   each lane.
#' @param color Optional name of a column used to colour lanes, such as the
#'   treatment arm.
#' @param ongoing Optional name of a logical column; `TRUE` marks subjects
#'   still on study, drawn with an arrow at the end of their lane.
#' @param events Optional data frame of events, one row per event, containing
#'   the `id`, `event_time` and `event_type` columns.
#' @param event_time,event_type Names of the event time (numeric) and event
#'   type columns in `events`. Required when `events` is supplied.
#' @param sort If `TRUE`, lanes are ordered by duration with the longest at
#'   the top. If `FALSE`, lanes keep the row order of `data`, top to bottom.
#' @param title Optional plot title.
#' @param x_label,y_label Axis titles.
#' @param engine Plotting engine: one of `"ggplot2"`, `"plotly"` or
#'   `"echarts4r"`. Defaults to the `floaties.engine` option, or `"ggplot2"`
#'   if that is unset.
#' @param ... Arguments passed on to the engine's generator.
#'
#' @return A ggplot object, or a plotly or echarts4r htmlwidget, depending on
#'   the engine.
#' @examples
#' lanes <- data.frame(
#'   subject = c("S01", "S02", "S03", "S04"),
#'   start = 0,
#'   end = c(120, 85, 200, 45),
#'   arm = c("A", "B", "A", "B"),
#'   on_study = c(TRUE, FALSE, TRUE, FALSE)
#' )
#' events <- data.frame(
#'   subject = c("S01", "S01", "S02", "S03", "S04"),
#'   day = c(30, 90, 60, 150, 40),
#'   response = c("PR", "CR", "PD", "PR", "PD")
#' )
#'
#' swimlane_plot(
#'   lanes,
#'   id = "subject", start = "start", end = "end",
#'   color = "arm", ongoing = "on_study",
#'   events = events, event_time = "day", event_type = "response"
#' )
#'
#' if (requireNamespace("plotly", quietly = TRUE)) {
#'   swimlane_plot(
#'     lanes,
#'     id = "subject", start = "start", end = "end", color = "arm",
#'     engine = "plotly"
#'   )
#' }
#' @export
swimlane_plot <- function(data, ..., engine = getOption("floaties.engine", "ggplot2")) {
  engine <- match_engine(engine)
  generator <- switch(engine,
    ggplot2 = swimlane_ggplot,
    plotly = swimlane_plotly,
    echarts4r = swimlane_echarts4r
  )
  generator(data, ...)
}

#' @rdname swimlane_plot
#' @export
swimlane_ggplot <- function(data, id, start, end, color = NULL, ongoing = NULL,
                            events = NULL, event_time = NULL, event_type = NULL,
                            sort = TRUE, title = NULL, x_label = "Time",
                            y_label = "Subject") {
  check_engine_installed("ggplot2")
  prep <- prepare_swimlane(
    data, id, start, end, color, ongoing, events, event_time, event_type, sort
  )
  lanes <- prep$lanes

  lane_aes <- if (prep$has_color) {
    ggplot2::aes(
      x = .data$.start, xend = .data$.end, y = .data$.id, yend = .data$.id,
      colour = .data$.color
    )
  } else {
    ggplot2::aes(x = .data$.start, xend = .data$.end, y = .data$.id, yend = .data$.id)
  }

  p <- ggplot2::ggplot(lanes) +
    ggplot2::geom_segment(lane_aes, linewidth = 5) +
    ggplot2::labs(title = title, x = x_label, y = y_label, colour = prep$color_label) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())

  if (!is.null(prep$events)) {
    p <- p +
      ggplot2::geom_point(
        data = prep$events,
        ggplot2::aes(x = .data$.time, y = .data$.id, shape = .data$.type),
        size = 2.5
      ) +
      ggplot2::labs(shape = prep$event_label)
  }

  if (any(lanes$.ongoing)) {
    ongoing_lanes <- lanes[lanes$.ongoing, , drop = FALSE]
    ongoing_lanes$.tip <- ongoing_lanes$.end + arrow_length(prep)
    p <- p +
      ggplot2::geom_segment(
        data = ongoing_lanes,
        ggplot2::aes(x = .data$.end, xend = .data$.tip, y = .data$.id, yend = .data$.id),
        arrow = ggplot2::arrow(length = ggplot2::unit(0.15, "cm"), type = "closed"),
        linewidth = 0.5
      ) +
      ggplot2::labs(caption = "Arrow indicates ongoing")
  }

  p
}

#' @rdname swimlane_plot
#' @export
swimlane_plotly <- function(data, id, start, end, color = NULL, ongoing = NULL,
                            events = NULL, event_time = NULL, event_type = NULL,
                            sort = TRUE, title = NULL, x_label = "Time",
                            y_label = "Subject") {
  check_engine_installed("plotly")
  prep <- prepare_swimlane(
    data, id, start, end, color, ongoing, events, event_time, event_type, sort
  )
  lanes <- prep$lanes
  lanes$.duration <- lanes$.end - lanes$.start
  lanes$.hover <- sprintf(
    "%s<br>Start: %s<br>End: %s", lanes$.id, fmt_num(lanes$.start), fmt_num(lanes$.end)
  )

  bar_args <- list(
    plotly::plot_ly(),
    data = lanes, x = ~.duration, base = ~.start, y = ~.id, orientation = "h",
    text = ~.hover, hoverinfo = "text", textposition = "none"
  )
  bar_args <- if (prep$has_color) {
    c(bar_args, list(color = ~.color))
  } else {
    c(bar_args, list(name = "Lane", showlegend = FALSE))
  }
  p <- do.call(plotly::add_bars, bar_args)

  if (!is.null(prep$events)) {
    ev <- prep$events
    ev$.hover <- sprintf("%s<br>%s: %s", ev$.id, ev$.type, fmt_num(ev$.time))
    p <- plotly::add_markers(
      p,
      data = ev, x = ~.time, y = ~.id, symbol = ~.type,
      marker = list(color = "black", size = 9),
      text = ~.hover, hoverinfo = "text", inherit = FALSE
    )
  }

  if (any(lanes$.ongoing)) {
    p <- plotly::add_markers(
      p,
      data = lanes[lanes$.ongoing, , drop = FALSE], x = ~.end, y = ~.id,
      name = "Ongoing", marker = list(symbol = "triangle-right", color = "black", size = 10),
      hoverinfo = "skip", inherit = FALSE
    )
  }

  plotly::layout(
    p,
    title = title,
    barmode = "overlay",
    xaxis = list(title = x_label, zeroline = FALSE),
    yaxis = list(
      title = y_label, type = "category",
      categoryorder = "array", categoryarray = levels(lanes$.id)
    )
  )
}

#' @rdname swimlane_plot
#' @export
swimlane_echarts4r <- function(data, id, start, end, color = NULL, ongoing = NULL,
                               events = NULL, event_time = NULL, event_type = NULL,
                               sort = TRUE, title = NULL, x_label = "Time",
                               y_label = "Subject") {
  check_engine_installed("echarts4r")
  prep <- prepare_swimlane(
    data, id, start, end, color, ongoing, events, event_time, event_type, sort
  )
  lanes <- prep$lanes
  lanes$.lane <- as.integer(lanes$.id) - 1L
  groups <- if (prep$has_color) lanes$.color else factor(rep("Lane", nrow(lanes)))

  # Lanes are custom series so they can start anywhere on the axis; a stacked
  # bar with a transparent offset breaks for negative start times.
  lane_series <- lapply(levels(groups), function(g) {
    d <- lanes[groups == g, , drop = FALSE]
    list(
      type = "custom",
      name = g,
      renderItem = htmlwidgets::JS(echarts_lane_js),
      encode = list(x = list(0, 1), y = 2),
      data = row_values(d$.start, d$.end, d$.lane, as.character(d$.id))
    )
  })

  event_series <- list()
  if (!is.null(prep$events)) {
    ev <- prep$events
    symbols <- rep_len(echarts_symbols, nlevels(ev$.type))
    event_series <- lapply(seq_len(nlevels(ev$.type)), function(i) {
      d <- ev[as.integer(ev$.type) == i, , drop = FALSE]
      list(
        type = "scatter",
        name = levels(ev$.type)[i],
        symbol = symbols[i],
        symbolSize = 10,
        itemStyle = list(color = "#333333"),
        data = row_values(d$.time, as.integer(d$.id) - 1L, as.character(d$.id))
      )
    })
  }

  if (any(lanes$.ongoing)) {
    d <- lanes[lanes$.ongoing, , drop = FALSE]
    event_series <- c(event_series, list(list(
      type = "scatter",
      name = "Ongoing",
      symbol = "triangle",
      symbolRotate = -90,
      symbolSize = 10,
      itemStyle = list(color = "#333333"),
      data = row_values(d$.end, d$.lane, as.character(d$.id))
    )))
  }

  opts <- list(
    tooltip = list(trigger = "item", formatter = htmlwidgets::JS(echarts_swimlane_tooltip_js)),
    legend = list(bottom = 0),
    grid = list(containLabel = TRUE, bottom = 60),
    xAxis = list(type = "value", name = x_label, nameLocation = "middle", nameGap = 30),
    yAxis = list(type = "category", name = y_label, data = as.list(levels(lanes$.id))),
    series = c(lane_series, event_series)
  )
  if (!is.null(title)) {
    opts$title <- list(text = title)
  }

  echarts4r::e_list(echarts4r::e_charts(), opts)
}

echarts_symbols <- c("circle", "diamond", "rect", "triangle", "roundRect", "pin")

echarts_lane_js <- "function(params, api) {
  var lane = api.value(2);
  var from = api.coord([api.value(0), lane]);
  var to = api.coord([api.value(1), lane]);
  var height = api.size([0, 1])[1] * 0.6;
  var rect = echarts.graphic.clipRectByRect(
    {x: from[0], y: from[1] - height / 2, width: to[0] - from[0], height: height},
    {x: params.coordSys.x, y: params.coordSys.y,
     width: params.coordSys.width, height: params.coordSys.height}
  );
  return rect && {type: 'rect', transition: ['shape'], shape: rect, style: api.style()};
}"

echarts_swimlane_tooltip_js <- "function(p) {
  var v = p.value;
  if (p.seriesType === 'custom') {
    return v[3] + '<br/>Start: ' + v[0] + '<br/>End: ' + v[1];
  }
  return v[2] + '<br/>' + p.seriesName + ': ' + v[0];
}"

# Space the ongoing arrow at 3% of the plotted time range.
arrow_length <- function(prep) {
  times <- c(prep$lanes$.start, prep$lanes$.end, prep$events$.time)
  span <- diff(range(times))
  if (span == 0) 1 else span * 0.03
}
