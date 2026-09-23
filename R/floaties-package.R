#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang .data
## usethis namespace: end
NULL

# Canonical columns, plus the plain names the echarts4r methods build and then
# refer to non-standardly.
utils::globalVariables(c(
  ".id", ".value", ".group", ".start", ".end", ".type",
  "id", "value", "group", "seg", "evt"
))
