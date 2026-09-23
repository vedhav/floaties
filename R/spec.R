# The spec is the contract between `prep_*()` and the renderers. `prep_*()`
# does all data work and builds it; renderers only read it.
#
# * `plot`: plot name, used to pick the renderer.
# * `data`: one row per subject, in plot order, with `.`-prefixed columns.
# * `events`: marker rows (swimlane only), or `NULL`.
# * `scales`: named vectors mapping levels to colors or shapes.
# * `labels`: `title`, `x`, `y`, and legend titles (`fill`, `event`). A `NULL`
#   `fill` title means the plot has no fill legend.
# * `ref_lines`: numeric vector of reference lines, or `NULL`.
new_floaties_spec <- function(plot,
                              data,
                              events = NULL,
                              scales = list(),
                              labels = list(),
                              ref_lines = NULL) {
  structure(
    list(
      plot = plot,
      data = data,
      events = events,
      scales = scales,
      labels = labels,
      ref_lines = ref_lines
    ),
    class = c(paste0("floaties_", plot, "_spec"), "floaties_spec")
  )
}

#' @export
print.floaties_spec <- function(x, ...) {
  cli::cat_line("<floaties ", x$plot, " spec: ", nrow(x$data), " subjects>")
  print(x$data, ...)
  if (!is.null(x$events)) {
    cli::cat_line()
    cli::cat_line("<", nrow(x$events), " events>")
    print(x$events, ...)
  }
  invisible(x)
}

# The one place that maps a plot and an engine to a renderer.
render_spec <- function(spec, engine) {
  renderers <- switch(spec$plot,
    waterfall = list(
      ggplot2 = render_waterfall_ggplot2,
      plotly = render_waterfall_plotly,
      echarts4r = render_waterfall_echarts4r
    ),
    swimlane = list(
      ggplot2 = render_swimlane_ggplot2,
      plotly = render_swimlane_plotly,
      echarts4r = render_swimlane_echarts4r
    )
  )
  renderers[[engine]](spec)
}

# One HTML tooltip string per row, from the named columns. `labels` comes
# from `column_labels()`.
build_tooltip <- function(data, cols, labels) {
  parts <- lapply(cols, function(col) {
    x <- data[[col]]
    x <- if (is.numeric(x)) {
      format(x, trim = TRUE, drop0trailing = TRUE, justify = "none")
    } else {
      as.character(x)
    }
    x[is.na(x)] <- "Missing"
    paste0(
      "<b>", htmltools::htmlEscape(labels[[col]]), ":</b> ",
      htmltools::htmlEscape(x)
    )
  })
  do.call(paste, c(parts, sep = "<br>"))
}

# echarts pieces shared by all echarts renderers. Tooltips are stored in each
# data point's `name`.
echarts_tooltip <- list(
  trigger = "item",
  formatter = htmlwidgets::JS("function(params) { return params.name; }")
)

echarts_ref_lines <- function(values, axis) {
  list(
    silent = TRUE,
    symbol = "none",
    label = list(show = FALSE),
    lineStyle = list(type = "dashed", color = "grey", width = 1),
    data = lapply(values, function(v) rlang::set_names(list(v), axis))
  )
}
