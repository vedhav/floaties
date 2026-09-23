#' Waterfall plot of best percent change from baseline
#'
#' Draws a clinical waterfall plot: one bar per subject showing the best
#' percent change from baseline (for example, in tumor size), sorted from the
#' largest increase to the largest decrease. Dashed reference lines mark the
#' RECIST thresholds for progressive disease (+20%) and partial response
#' (-30%) by default.
#'
#' The plot can be drawn with ggplot2 (static, the default), plotly, or
#' echarts4r. The interactive engines show each subject's values on hover and
#' use the same colours as the ggplot2 version.
#'
#' @param data A data frame with one row per subject.
#' @param id <[`data-masking`][ggplot2::aes_eval]> Column that identifies
#'   subjects. Values must be unique.
#' @param value <[`data-masking`][ggplot2::aes_eval]> Numeric column holding
#'   the best percent change from baseline. Rows with missing values are
#'   dropped with a warning.
#' @param fill <[`data-masking`][ggplot2::aes_eval]> Optional categorical
#'   column used to color the bars, such as best overall response or
#'   treatment arm.
#' @param ref_lines Numeric vector of y values where dashed reference lines
#'   are drawn. Use `NULL` for none.
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
#' set.seed(1)
#' tumor <- data.frame(
#'   subject = sprintf("S%02d", 1:30),
#'   pchg = round(runif(30, -100, 60)),
#'   arm = sample(c("Placebo", "Treatment"), 30, replace = TRUE)
#' )
#'
#' waterfall_plot(tumor, subject, pchg)
#' waterfall_plot(tumor, subject, pchg, fill = arm, title = "Best response")
#'
#' @examplesIf requireNamespace("plotly", quietly = TRUE)
#' waterfall_plot(tumor, subject, pchg, fill = arm, engine = "plotly")
#'
#' @examplesIf requireNamespace("echarts4r", quietly = TRUE)
#' waterfall_plot(tumor, subject, pchg, fill = arm, engine = "echarts4r")
waterfall_plot <- function(
  data,
  id,
  value,
  fill = NULL,
  ref_lines = c(20, -30),
  xlab = "Subject",
  ylab = "Best % change from baseline",
  title = NULL,
  engine = c("ggplot2", "plotly", "echarts4r")
) {
  engine <- match.arg(engine)
  check_engine(engine)
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  if (!is.null(ref_lines) && !is.numeric(ref_lines)) {
    stop("`ref_lines` must be numeric or NULL.", call. = FALSE)
  }

  ids <- rlang::eval_tidy(rlang::enquo(id), data)
  values <- rlang::eval_tidy(rlang::enquo(value), data)
  has_fill <- !rlang::quo_is_null(rlang::enquo(fill))

  if (!is.numeric(values)) {
    stop("`value` must be a numeric column.", call. = FALSE)
  }

  missing <- is.na(values)
  if (any(missing)) {
    warning(
      sprintf("Removed %d row(s) with missing `value`.", sum(missing)),
      call. = FALSE
    )
    data <- data[!missing, , drop = FALSE]
    ids <- ids[!missing]
    values <- values[!missing]
  }

  if (anyDuplicated(ids)) {
    stop("`id` must be unique: expected one row per subject.", call. = FALSE)
  }

  data$.id <- stats::reorder(droplevels(factor(ids)), -values)
  data$.value <- values
  if (has_fill) {
    data$.fill <- rlang::eval_tidy(rlang::enquo(fill), data)
  }

  labels <- list(
    id = rlang::as_label(rlang::enquo(id)),
    value = rlang::as_label(rlang::enquo(value)),
    fill = if (has_fill) rlang::as_label(rlang::enquo(fill)),
    x = xlab,
    y = ylab,
    title = title
  )

  render <- switch(
    engine,
    ggplot2 = waterfall_ggplot,
    plotly = waterfall_plotly,
    echarts4r = waterfall_echarts
  )
  render(data, has_fill, ref_lines, labels)
}

waterfall_ggplot <- function(data, has_fill, ref_lines, labels) {
  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data$.id, y = .data$.value))

  if (has_fill) {
    p <- p + ggplot2::geom_col(ggplot2::aes(fill = .data$.fill), width = 0.8)
  } else {
    p <- p + ggplot2::geom_col(fill = "grey40", width = 0.8)
  }

  p <- p + ggplot2::geom_hline(yintercept = 0, colour = "black")

  if (length(ref_lines) > 0) {
    p <- p +
      ggplot2::geom_hline(
        yintercept = ref_lines,
        linetype = "dashed",
        colour = "grey30"
      )
  }

  p +
    ggplot2::labs(x = labels$x, y = labels$y, title = labels$title, fill = labels$fill) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank()
    )
}

# Bars grouped by fill level: one trace or series per level, in legend order.
waterfall_groups <- function(data, has_fill) {
  if (!has_fill) {
    return(list(
      values = rep("all", nrow(data)),
      levels = "all",
      colours = c(all = "#666666")
    ))
  }
  levels <- group_levels(data$.fill)
  list(
    values = group_values(data$.fill),
    levels = levels,
    colours = group_colours(levels, hue_colours)
  )
}

waterfall_plotly <- function(data, has_fill, ref_lines, labels) {
  groups <- waterfall_groups(data, has_fill)
  hover <- paste0(
    labels$id, ": ", data$.id,
    "<br>", labels$value, ": ", format_number(data$.value)
  )
  if (has_fill) {
    hover <- paste0(hover, "<br>", labels$fill, ": ", groups$values)
  }

  p <- plotly::plot_ly()
  for (level in groups$levels) {
    rows <- groups$values == level
    p <- plotly::add_bars(
      p,
      x = as.character(data$.id[rows]),
      y = data$.value[rows],
      name = level,
      marker = list(color = groups$colours[[level]]),
      hovertext = hover[rows],
      hoverinfo = "text",
      textposition = "none",
      showlegend = has_fill
    )
  }

  shapes <- lapply(ref_lines, function(y) {
    list(
      type = "line",
      xref = "paper",
      x0 = 0,
      x1 = 1,
      y0 = y,
      y1 = y,
      line = list(dash = "dash", color = "#4D4D4D", width = 1)
    )
  })

  plotly::layout(
    p,
    barmode = "relative",
    bargap = 0.2,
    title = plotly_title(labels$title),
    xaxis = list(
      title = labels$x,
      type = "category",
      categoryorder = "array",
      categoryarray = levels(data$.id),
      showticklabels = FALSE
    ),
    yaxis = list(title = labels$y, zeroline = TRUE, zerolinecolor = "black"),
    shapes = shapes,
    legend = list(title = list(text = labels$fill))
  )
}

waterfall_echarts <- function(data, has_fill, ref_lines, labels) {
  data <- data[order(data$.id), , drop = FALSE]
  groups <- waterfall_groups(data, has_fill)

  # One column per fill level, NA elsewhere, stacked so bars stay centred.
  wide <- data.frame(.x = as.character(data$.id))
  series <- paste0("s", seq_along(groups$levels))
  for (i in seq_along(groups$levels)) {
    wide[[series[i]]] <- ifelse(groups$values == groups$levels[i], data$.value, NA)
  }

  p <- echarts4r::e_charts_(wide, ".x", reorder = FALSE)
  for (i in seq_along(groups$levels)) {
    p <- echarts4r::e_bar_(
      p,
      series[i],
      name = groups$levels[i],
      stack = "bars",
      barCategoryGap = "20%",
      itemStyle = list(color = groups$colours[[groups$levels[i]]])
    )
  }

  if (length(ref_lines) > 0) {
    p$x$opts$series[[1]]$markLine <- list(
      symbol = "none",
      silent = TRUE,
      label = list(show = FALSE),
      lineStyle = list(type = "dashed", color = "#4D4D4D"),
      data = lapply(ref_lines, function(y) list(yAxis = y))
    )
  }

  formatter <- sprintf(
    "function(p) {
      var s = %s + ': ' + p.value[0] + '<br/>' + %s + ': ' + (+p.value[1]).toFixed(1);
      if (%s) s += '<br/>' + %s + ': ' + p.seriesName;
      return s;
    }",
    js_string(labels$id),
    js_string(labels$value),
    tolower(has_fill),
    js_string(if (has_fill) labels$fill else "")
  )

  p <- echarts4r::e_x_axis(
    p,
    name = labels$x,
    nameLocation = "middle",
    nameGap = 15,
    axisLabel = list(show = FALSE),
    axisTick = list(show = FALSE)
  )
  p <- echarts4r::e_y_axis(p, name = labels$y, nameLocation = "middle", nameGap = 40)
  p <- echarts4r::e_tooltip(p, trigger = "item", formatter = htmlwidgets::JS(formatter))
  p <- echarts_legend(p, if (has_fill) groups$levels)
  if (!is.null(labels$title)) {
    p <- echarts4r::e_title(p, labels$title)
  }
  p
}
