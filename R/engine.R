floaties_engines <- c("ggplot2", "plotly", "echarts4r")

match_engine <- function(engine) {
  rlang::arg_match0(engine, floaties_engines, arg_nm = "engine")
}

check_engine_installed <- function(engine) {
  rlang::check_installed(engine, reason = sprintf("to use the %s engine.", engine))
}
