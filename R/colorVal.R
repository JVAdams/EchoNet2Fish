#' Colors Based on Values
#'
#' Assign a range of unique colors based on values.
#' @param x
#'   A numeric vector of values on which the colors are based.
#' @param n
#'   A numeric scalar giving the number of unique colors in the palette,
#'   default 100.
#' @param low
#'   A numeric scalar giving the hue in [0, 1] at which the color palette
#'   begins for low values, default 4/6 for blue.
#' @param high
#'   A numeric scalar giving the hue in [0, 1] at which the color palette
#'   ends for high values, default 0 for red.
#' @importFrom plotrix rescale
#' @import grDevices
#' @export
#' @seealso
#'   \code{\link{rainbow}}
#' @examples
#' rand <- rnorm(20)
#' plot(rand, rand, pch=16, col=colorVal(rand))
#'
colorVal <- function(x, n=100, low=4/6, high=0/6) {
	# assign a specified number of rainbow colors to a collection of values
	# default color range from blue (4/6) to red (0/6)
	y <- round(plotrix::rescale(x, c(0, n-1))) + 1
	# reverse the direction of y (e.g., from 1:100 to 100:1)
	y <- n + 1 - y
	rainbow(n, start=high, end=low)[y]
}



