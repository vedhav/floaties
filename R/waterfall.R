#' Waterfall plot
#'
#' Draws one vertical bar per subject showing a change from baseline, such as
#' the best percentage change in tumour size, sorted from the largest increase
#' to the deepest reduction.
#'
#' `waterfall_plot()` dispatches to the generator for the chosen `engine`. The
#' generators can also be called directly: `waterfall_ggplot()` returns a
#' ggplot object, `waterfall_plotly()` a plotly htmlwidget and
#' `waterfall_echarts4r()` an echarts4r htmlwidget. All three take the same
#' arguments and draw the same plot.
#'
#' @param data A data frame with one row per subject.
#' @param id Name of the subject identifier column.
#' @param value Name of the numeric column holding each subject's change.
#' @param fill Optional name of a column used to colour bars, such as the best
#'   overall response.
#' @param ref_lines Numeric vector of y values drawn as dashed reference lines.
#'   The default marks the RECIST thresholds for progression (+20%) and
#'   partial response (-30%). Use `NULL` for none.
#' @param title Optional plot title.
#' @param x_label,y_label Axis titles.
#' @inheritParams swimlane_plot
#'
#' @return A ggplot object, or a plotly or echarts4r htmlwidget, depending on
#'   the engine.
#' @examples
#' tumour <- data.frame(
#'   subject = sprintf("S%02d", 1:8),
#'   best_change = c(35, 12, -5, -28, -31, -45, -60, -100),
#'   response = c("PD", "SD", "SD", "SD", "PR", "PR", "PR", "CR")
#' )
#'
#' waterfall_plot(tumour, id = "subject", value = "best_change", fill = "response")
#'
#' if (requireNamespace("echarts4r", quietly = TRUE)) {
#'   waterfall_plot(
#'     tumour,
#'     id = "subject", value = "best_change", fill = "response",
#'     engine = "echarts4r"
#'   )
#' }
#' @export
waterfall_plot <- function(data, ..., engine = getOption("floaties.engine", "ggplot2")) {
  engine <- match_engine(engine)
  generator <- switch(engine,
    ggplot2 = waterfall_ggplot,
    plotly = waterfall_plotly,
    echarts4r = waterfall_echarts4r
  )
  generator(data, ...)
}

#' @rdname waterfall_plot
#' @export
waterfall_ggplot <- function(data, id, value, fill = NULL, ref_lines = c(20, -30),
                             title = NULL, x_label = "Subject",
                             y_label = "Best % change from baseline") {
  check_engine_installed("ggplot2")
  prep <- prepare_waterfall(data, id, value, fill)

  bar_aes <- if (prep$has_fill) {
    ggplot2::aes(x = .data$.id, y = .data$.value, fill = .data$.fill)
  } else {
    ggplot2::aes(x = .data$.id, y = .data$.value)
  }

  p <- ggplot2::ggplot(prep$bars, bar_aes) +
    ggplot2::geom_col(width = 0.8) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::labs(title = title, x = x_label, y = y_label, fill = prep$fill_label) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1),
      panel.grid.major.x = ggplot2::element_blank()
    )

  if (length(ref_lines) > 0) {
    p <- p + ggplot2::geom_hline(yintercept = ref_lines, linetype = "dashed", colour = "grey40")
  }

  p
}

#' @rdname waterfall_plot
#' @export
waterfall_plotly <- function(data, id, value, fill = NULL, ref_lines = c(20, -30),
                             title = NULL, x_label = "Subject",
                             y_label = "Best % change from baseline") {
  check_engine_installed("plotly")
  prep <- prepare_waterfall(data, id, value, fill)
  bars <- prep$bars
  bars$.hover <- sprintf("%s: %s", bars$.id, fmt_num(bars$.value))

  bar_args <- list(
    plotly::plot_ly(),
    data = bars, x = ~.id, y = ~.value,
    text = ~.hover, hoverinfo = "text", textposition = "none"
  )
  bar_args <- if (prep$has_fill) {
    c(bar_args, list(color = ~.fill))
  } else {
    c(bar_args, list(name = "Change", showlegend = FALSE))
  }
  p <- do.call(plotly::add_bars, bar_args)

  ref_shapes <- lapply(ref_lines, function(y) {
    list(
      type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = y, y1 = y,
      line = list(dash = "dash", color = "grey", width = 1)
    )
  })

  plotly::layout(
    p,
    title = title,
    barmode = "overlay",
    shapes = ref_shapes,
    xaxis = list(
      title = x_label, type = "category",
      categoryorder = "array", categoryarray = levels(bars$.id)
    ),
    yaxis = list(title = y_label)
  )
}

#' @rdname waterfall_plot
#' @export
waterfall_echarts4r <- function(data, id, value, fill = NULL, ref_lines = c(20, -30),
                                title = NULL, x_label = "Subject",
                                y_label = "Best % change from baseline") {
  check_engine_installed("echarts4r")
  prep <- prepare_waterfall(data, id, value, fill)
  bars <- prep$bars
  groups <- if (prep$has_fill) bars$.fill else factor(rep("Change", nrow(bars)))

  # One series per group, each with a slot for every subject; stacking keeps
  # the single non-empty bar per subject centred on its category.
  series <- lapply(levels(groups), function(g) {
    in_group <- groups == g
    list(
      type = "bar",
      name = g,
      stack = "waterfall",
      barCategoryGap = "20%",
      data = lapply(seq_along(in_group), function(i) {
        if (in_group[i]) bars$.value[i] else "-"
      })
    )
  })

  if (length(ref_lines) > 0) {
    series[[1]]$markLine <- list(
      symbol = "none",
      silent = TRUE,
      lineStyle = list(type = "dashed", color = "#666666"),
      data = lapply(ref_lines, function(y) list(yAxis = y))
    )
  }

  opts <- list(
    tooltip = list(trigger = "item"),
    legend = list(show = prep$has_fill, bottom = 0),
    grid = list(containLabel = TRUE, bottom = 90),
    xAxis = list(
      type = "category", name = x_label, nameLocation = "middle", nameGap = 50,
      data = as.list(levels(bars$.id)), axisLabel = list(rotate = 90)
    ),
    yAxis = list(type = "value", name = y_label, nameLocation = "middle", nameGap = 40),
    series = series
  )
  if (!is.null(title)) {
    opts$title <- list(text = title)
  }

  echarts4r::e_list(echarts4r::e_charts(), opts)
}
