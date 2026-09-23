#' @rdname render_waterfall
#' @export
render_waterfall.floaties_ggplot2 <- function(engine, x, ...) {
  d <- as.data.frame(x)
  grouped <- !all(is.na(d$.group))

  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[[".id"]], y = .data[[".value"]]))

  p <- p + if (grouped) {
    ggplot2::geom_col(ggplot2::aes(fill = .data[[".group"]]))
  } else {
    ggplot2::geom_col(fill = floaties_palette("bar"))
  }

  for (h in attr(x, "hlines")) {
    p <- p + ggplot2::geom_hline(
      yintercept = h,
      linetype   = "dashed",
      colour     = floaties_palette("rule")
    )
  }

  p +
    ggplot2::labs(title = attr(x, "title"), x = NULL, y = NULL, fill = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x     = ggplot2::element_text(angle = 90, vjust = 0.5, size = 7),
      panel.grid.major.x = ggplot2::element_blank()
    )
}
