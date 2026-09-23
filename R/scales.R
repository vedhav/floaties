# Colors and shapes. `prep_*()` resolves every scale to a named vector
# (names are levels) so renderers only look values up.

# Keep existing level order, make missing values an explicit "Missing" level,
# and drop unused levels so every engine shows the same legend.
as_level_factor <- function(x) {
  if (!is.factor(x)) {
    x <- factor(x)
  }
  if (anyNA(x)) {
    levels(x) <- union(levels(x), "Missing")
    x[is.na(x)] <- "Missing"
  }
  droplevels(x)
}

# Okabe-Ito colors (colorblind friendly), with grey for "Missing". `light`
# mixes them with white, for swimlane lanes that have markers drawn on top.
default_colors <- function(levels, light = FALSE) {
  okabe_ito <- unname(grDevices::palette.colors(palette = "Okabe-Ito"))
  palette <- okabe_ito[c(6, 7, 4, 2, 8, 3, 5)]
  n <- sum(levels != "Missing")
  colors <- if (n <= length(palette)) {
    palette[seq_len(n)]
  } else {
    grDevices::hcl.colors(n, palette = "Dark 3")
  }
  if (light && n > 0) {
    rgb <- grDevices::col2rgb(colors) / 255
    rgb <- rgb + (1 - rgb) * 0.7
    colors <- grDevices::rgb(rgb[1, ], rgb[2, ], rgb[3, ])
  }
  out <- rep(if (light) "#DDDDDD" else "#999999", length(levels))
  out[levels != "Missing"] <- colors
  rlang::set_names(out, levels)
}

default_shapes <- function(levels, call = rlang::caller_env()) {
  shapes <- c("circle", "square", "triangle", "diamond", "cross", "star")
  if (length(levels) > length(shapes)) {
    cli::cli_abort(
      c(
        "Can't pick default shapes for more than {length(shapes)} event types.",
        "x" = "There are {length(levels)} event types.",
        "i" = "Set {.arg event_shapes}, or combine some event types."
      ),
      class = "floaties_error_argument",
      call = call
    )
  }
  rlang::set_names(shapes[seq_along(levels)], levels)
}

# Match user values (colors or shapes) to levels. `values` can be named by
# level, or unnamed and used in level order. `defaults` is only evaluated when
# `values` is `NULL`.
resolve_scale <- function(values,
                          levels,
                          defaults,
                          what,
                          arg = rlang::caller_arg(values),
                          call = rlang::caller_env()) {
  if (is.null(values)) {
    return(defaults)
  }
  assert_arg(checkmate::check_character(values, any.missing = FALSE), arg, call)

  if (rlang::is_named(values)) {
    missing <- setdiff(levels, names(values))
    if (length(missing) > 0) {
      cli::cli_abort(
        c(
          "{.arg {arg}} must have a {what} for every level.",
          "x" = "No {what} for {.val {missing}}."
        ),
        class = "floaties_error_argument",
        call = call
      )
    }
    return(values[levels])
  }

  n <- length(levels)
  if (length(values) < n) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must have at least {n} {what}{cli::qty(n)}{?s}.",
        "x" = "{length(values)} {what}{cli::qty(length(values))}{?s}
               supplied for {n} level{?s}."
      ),
      class = "floaties_error_argument",
      call = call
    )
  }
  rlang::set_names(values[seq_len(n)], levels)
}

# Marker shapes every engine can draw, and each engine's name for them.
# ggplot2 uses point codes (62 draws a ">"). echarts has no built-in cross,
# star, or right-pointing arrow, so those use SVG paths. "arrow" marks
# ongoing swimlane subjects.
shape_map <- list(
  circle = c(ggplot2 = "16", plotly = "circle", echarts4r = "circle"),
  square = c(ggplot2 = "15", plotly = "square", echarts4r = "rect"),
  triangle = c(
    ggplot2 = "17", plotly = "triangle-up", echarts4r = "triangle"
  ),
  diamond = c(ggplot2 = "18", plotly = "diamond", echarts4r = "diamond"),
  cross = c(
    ggplot2 = "4",
    plotly = "x",
    echarts4r = paste0(
      "path://M2,0 L5,3 L8,0 L10,2 L7,5 L10,8 L8,10 L5,7 L2,10 L0,8 ",
      "L3,5 L0,2 Z"
    )
  ),
  star = c(
    ggplot2 = "8",
    plotly = "star",
    echarts4r = paste0(
      "path://M5,0 L6.2,3.6 L10,3.6 L6.9,5.9 L8.1,9.5 L5,7.3 L1.9,9.5 ",
      "L3.1,5.9 L0,3.6 L3.8,3.6 Z"
    )
  ),
  arrow = c(
    ggplot2 = "62",
    plotly = "triangle-right",
    echarts4r = "path://M0,0 L10,5 L0,10 Z"
  )
)

# Translate a named shape vector (names are levels) to one engine's names.
engine_shapes <- function(shapes, engine) {
  rlang::set_names(
    vapply(shape_map[shapes], function(s) s[[engine]], character(1)),
    names(shapes)
  )
}
