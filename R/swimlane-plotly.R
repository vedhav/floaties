#' @rdname render_swimlane
#' @export
render_swimlane.floaties_plotly <- function(engine, x, ...) {
  rlang::check_installed("plotly", "to render with `eng_plotly()`.")

  parts <- split_lanes(x)
  typed <- !all(is.na(x$.type))

  p <- plotly::plot_ly()

  if (nrow(parts$intervals) > 0L) {
    p <- if (typed) {
      plotly::add_segments(
        p, data = parts$intervals,
        x = ~.start, xend = ~.end, y = ~.id, yend = ~.id,
        color = ~.type, line = list(width = 8)
      )
    } else {
      plotly::add_segments(
        p, data = parts$intervals,
        x = ~.start, xend = ~.end, y = ~.id, yend = ~.id,
        line = list(width = 8, color = floaties_palette("bar")),
        showlegend = FALSE
      )
    }
  }

  if (nrow(parts$events) > 0L) {
    p <- if (typed) {
      plotly::add_markers(
        p, data = parts$events, x = ~.start, y = ~.id,
        color = ~.type, marker = list(symbol = "diamond", size = 9)
      )
    } else {
      plotly::add_markers(
        p, data = parts$events, x = ~.start, y = ~.id,
        marker = list(symbol = "diamond", size = 9,
                      color = floaties_palette("rule")),
        showlegend = FALSE
      )
    }
  }

  shapes <- lapply(attr(x, "vlines"), function(v) {
    list(
      type = "line", yref = "paper", y0 = 0, y1 = 1, x0 = v, x1 = v,
      line = list(dash = "dash", color = floaties_palette("rule"))
    )
  })

  plotly::layout(
    p,
    title  = attr(x, "title"),
    xaxis  = list(title = ""),
    yaxis  = list(
      title         = "",
      type          = "category",
      categoryorder = "array",
      categoryarray = levels(x$.id)
    ),
    shapes = shapes
  )
}
