#' @rdname render_swimlane
#'
#' @details
#' The echarts4r method draws lanes on a category y axis and time on a value x
#' axis. Intervals become line segments separated by `NA`, which is what lets
#' overlapping intervals on one lane render correctly, and point events become
#' a scatter series. Unlike the ggplot2 and plotly methods it does not colour by
#' `.type`, because doing so needs one series per type and the legend stops
#' matching the other engines. Treat that as a known difference rather than a
#' bug, and eyeball the result: no test can tell you a swimlane reads correctly.
#'
#' @export
render_swimlane.floaties_echarts4r <- function(engine, x, ...) {
  rlang::check_installed("echarts4r", "to render with `eng_echarts4r()`.")

  lanes <- levels(x$.id)
  lane_index <- function(id) match(as.character(id), lanes) - 1L

  # Two points and a break per interval, so overlapping intervals on the same
  # lane stay separate instead of being joined into one line.
  ints <- x[!is.na(x$.end), , drop = FALSE]
  seg <- if (nrow(ints) > 0L) {
    data.frame(
      x   = as.vector(rbind(ints$.start, ints$.end, NA_real_)),
      seg = as.vector(rbind(
        lane_index(ints$.id), lane_index(ints$.id), NA_integer_
      )),
      evt = NA_integer_
    )
  } else {
    NULL
  }

  evs <- x[is.na(x$.end), , drop = FALSE]
  pts <- if (nrow(evs) > 0L) {
    data.frame(x = evs$.start, seg = NA_integer_, evt = lane_index(evs$.id))
  } else {
    NULL
  }

  d <- rbind(seg, pts)
  if (is.null(d) || nrow(d) == 0L) {
    d <- data.frame(x = numeric(0), seg = integer(0), evt = integer(0))
  }

  e <- echarts4r::e_charts(d, x)

  if (!is.null(seg)) {
    e <- echarts4r::e_line(
      e, seg,
      name         = "interval",
      connectNulls = FALSE,
      symbol       = "none",
      lineStyle    = list(width = 8, color = floaties_palette("bar"))
    )
  }
  if (!is.null(pts)) {
    e <- echarts4r::e_scatter(
      e, evt,
      name       = "event",
      symbol     = "diamond",
      symbolSize = 9,
      itemStyle  = list(color = floaties_palette("rule"))
    )
  }

  e <- echarts4r::e_x_axis(e, type = "value")
  e <- echarts4r::e_y_axis(e, type = "category", data = as.list(lanes))

  for (v in attr(x, "vlines")) {
    e <- echarts4r::e_mark_line(e, data = list(xAxis = v),
                                title = as.character(v))
  }

  e <- echarts4r::e_tooltip(e)
  if (!is.null(attr(x, "title"))) {
    e <- echarts4r::e_title(e, attr(x, "title"))
  }
  e
}
