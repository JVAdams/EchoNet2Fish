#' Test for and Plot Errors in Acoustic Survey Values
#'
#' Test for and plot errors in acoustic survey data, based on reported
#' lows, highs, and in-between values.
#' @param low
#'   A numeric vector of low values.
#' @param high
#'   A numeric vector of high values, the same length as \code{low}.
#' @param between
#'   A numeric vector of in between values, the same length as \code{low}.
#' @param lowhighKnown
#'   A logical scalar indicating whether the vector representing the lows
#'   and the vector representing the highs are known, default FALSE.
#'   If FALSE, the low (and high) value is calculated as the elementwise
#'   minimum (and maximum) of the three vectors, \code{low}, \code{high},
#'   and \code{between}.
#' @param varname
#'   A character scalar identifying what the values represent, used as the
#'   y-axis label if test=FALSE, default "Varname".
#' @param test
#'   A logical scalar indicating whether to conduct a test for errors (TRUE)
#'   or to draw a plot of the results (FALSE, the default).
#' @param ...
#'   Additional arguments to \code{\link[graphics]{plot}}.
#' @return
#'   If \code{test} = TRUE, a logical scalar is returned indicating whether
#'   there were errors in the values (TRUE) or not (FALSE).
#'   If \code{test} = FALSE, a figure is drawn, but no value is returned.
#' @import graphics
#' @export

plotValues <- function(low, high, between, lowhighKnown=FALSE,
  varname="Varname", test=FALSE, ...) {
	# plot lows, highs, and betweens for a given metric
	x <- seq(high)
	if(!lowhighKnown) {
		newlo <- pmin(low, high, between, na.rm=TRUE)
		newhi <- pmax(low, high, between, na.rm=TRUE)
		low <- newlo
		high <- newhi
	}
	ord <- order(high)
	lo. <- low[ord]
	be. <- between[ord]
	hi. <- high[ord]
	sel.lo <- (!is.na(lo.) & !is.na(be.) & lo. > be.) |
	  (!is.na(lo.) & !is.na(hi.) & lo. > hi.)
	sel.be <- !is.na(be.) & !is.na(hi.) & be. > hi.
	if(!test) {
		yr <- range(lo., hi., be., na.rm=TRUE)
		par(mar=c(4, 4, 1, 1), cex=1.5)
		plot(x, be., type="n", ylim=yr, las=1,
		  xlab="Records ordered by max", ylab=varname, ...)
		points(x[!sel.lo], lo.[!sel.lo], pch=6, col="blue")
		points(x[!sel.be], be.[!sel.be])
		points(x, hi., pch=2, col="red")
		points(x[sel.lo], lo.[sel.lo], pch=6, col="cyan")
		points(x[sel.be], be.[sel.be], pch=3, col="green")
		legend("topleft", c(paste0("Mid > Max (", format(sum(sel.be, na.rm=TRUE),
		  big.mark=","), " records)"), "Max",
			paste0("Min > Mid or Max (",
			  format(sum(sel.lo, na.rm=TRUE), big.mark=","), " records)"),
		  "Mid < Max", "Min < Mid and Max"),
			col=c("green", "red", "cyan", "black", "blue"), pch=c(3, 2, 6, 1, 6),
		  cex=1.5)
	} else {
  	problems <- sum(sum(sel.be, na.rm=TRUE), sum(sel.lo, na.rm=TRUE))
    if(is.na(problems) | problems < 1) return(FALSE) else return(TRUE)
  }
}
