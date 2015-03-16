#' Cross Tabulation and Table Creation
#'
#' Build a contingency table of the counts at each combination of factor levels,
#' incorporating missing values by default.
#' @param ...
#'   Arguments provided to \code{\link{table}}.
#' @return
#'   An array of integer values of class "table".
#' @export
#' @examples
#' mytable(c(1, 1, 1, 2, NA, 3, 4, 1, 10, 3))
#'
mytable <- function(...) {
  table(..., useNA="ifany")
}
