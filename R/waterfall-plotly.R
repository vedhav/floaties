#' @rdname render_waterfall
#' @export
render_waterfall.floaties_plotly <- function(engine, x, ...) {
  rlang::check_installed("plotly", "to render with `eng_plotly()`.")

  d <- as.data.frame(x)
  grouped <- !all(is.na(d$.group))

  p <- if (grouped) {
    plotly::plot_ly(d, x = ~.id, y = ~.value, color = ~.group, type = "bar")
  } else {
    plotly::plot_ly(
      d, x = ~.id, y = ~.value, type = "bar",
      marker = list(color = floaties_palette("bar"))
    )
  }

  shapes <- lapply(attr(x, "hlines"), function(h) {
    list(
      type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = h, y1 = h,
      line = list(dash = "dash", color = floaties_palette("rule"))
    )
  })

  plotly::layout(
    p,
    title  = attr(x, "title"),
    xaxis  = list(
      title         = "",
      categoryorder = "array",
      categoryarray = levels(d$.id)
    ),
    yaxis  = list(title = ""),
    shapes = shapes
  )
}
