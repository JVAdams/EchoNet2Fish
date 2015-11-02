#' Midpoint Between the Minimum and the Maximum
#'
#' Calculate the midpoint between the minimum and the maximum of a vector.
#' @param x
#'   A numeric vector.
#' @return
#'   A numeric scalar representing the midpoint between the minimum and the
#'   maximum of \code{x}, ignoring missing values.
#' @export
#' @examples
#' midpoint(c(10:20, 90))

midpoint <- function(x) {
	# calculate the midpoint between the min and the max
	(max(x, na.rm=TRUE) + min(x, na.rm=TRUE))/2
}
