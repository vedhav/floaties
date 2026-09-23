#' @rdname render_waterfall
#' @export
render_waterfall.floaties_echarts4r <- function(engine, x, ...) {
  rlang::check_installed("echarts4r", "to render with `eng_echarts4r()`.")

  d <- data.frame(
    id    = factor(as.character(x$.id), levels = levels(x$.id)),
    value = x$.value,
    group = x$.group,
    stringsAsFactors = FALSE
  )
  grouped <- !all(is.na(d$group))

  e <- if (grouped) {
    echarts4r::e_charts(dplyr::group_by(d, group), id)
  } else {
    echarts4r::e_charts(d, id)
  }

  e <- echarts4r::e_bar(e, value, name = "change")
  if (!grouped) {
    e <- echarts4r::e_color(e, floaties_palette("bar"))
  }

  for (h in attr(x, "hlines")) {
    e <- echarts4r::e_mark_line(
      e,
      data  = list(yAxis = h),
      title = as.character(h)
    )
  }

  e <- echarts4r::e_tooltip(e, trigger = "axis")
  e <- echarts4r::e_x_axis(e, axisLabel = list(rotate = 90, fontSize = 9))
  if (!is.null(attr(x, "title"))) {
    e <- echarts4r::e_title(e, attr(x, "title"))
  }
  e
}
