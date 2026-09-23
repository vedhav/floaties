#' @rdname render_swimlane
#' @export
render_swimlane.floaties_ggplot2 <- function(engine, x, ...) {
  parts <- split_lanes(x)
  typed <- !all(is.na(x$.type))

  p <- ggplot2::ggplot()

  if (nrow(parts$intervals) > 0L) {
    args <- list(
      data      = parts$intervals,
      mapping   = if (typed) {
        ggplot2::aes(
          x = .data[[".start"]], xend = .data[[".end"]],
          y = .data[[".id"]], yend = .data[[".id"]],
          colour = .data[[".type"]]
        )
      } else {
        ggplot2::aes(
          x = .data[[".start"]], xend = .data[[".end"]],
          y = .data[[".id"]], yend = .data[[".id"]]
        )
      },
      linewidth = 3,
      lineend   = "round"
    )
    # Setting a fixed colour alongside a colour aesthetic makes ggplot2 warn
    # about an empty aesthetic, so only one of the two is ever passed.
    if (!typed) args$colour <- floaties_palette("bar")
    p <- p + do.call(ggplot2::geom_segment, args)
  }

  if (nrow(parts$events) > 0L) {
    args <- list(
      data    = parts$events,
      mapping = if (typed) {
        ggplot2::aes(
          x = .data[[".start"]], y = .data[[".id"]],
          colour = .data[[".type"]]
        )
      } else {
        ggplot2::aes(x = .data[[".start"]], y = .data[[".id"]])
      },
      size  = 2.5,
      shape = 18
    )
    if (!typed) args$colour <- floaties_palette("rule")
    p <- p + do.call(ggplot2::geom_point, args)
  }

  for (v in attr(x, "vlines")) {
    p <- p + ggplot2::geom_vline(
      xintercept = v, linetype = "dashed", colour = floaties_palette("rule")
    )
  }

  # A zero-length `limits` breaks scale expansion, so a swimlane of no
  # subjects has to skip the scale rather than pass character(0).
  if (nlevels(x$.id) > 0L) {
    p <- p + ggplot2::scale_y_discrete(limits = levels(x$.id))
  }

  p +
    ggplot2::labs(title = attr(x, "title"), x = NULL, y = NULL, colour = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      axis.text.y        = ggplot2::element_text(size = 8)
    )
}
