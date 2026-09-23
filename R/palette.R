# Not a theme system. Six lines so that three engines agree on two colours,
# which is the minimum needed to stop them drifting apart. Build something
# larger only when a real divergence shows up.

FLOATIES_COLOURS <- c(
  bar  = "#0099F9",
  rule = "#507084"
)

#' Colours shared by every engine
#'
#' @param which Name of a colour: `"bar"` or `"rule"`.
#'
#' @return A hex colour string.
#'
#' @examples
#' floaties_palette("bar")
#'
#' @export
floaties_palette <- function(which = names(FLOATIES_COLOURS)) {
  which <- match.arg(which)
  unname(FLOATIES_COLOURS[[which]])
}
