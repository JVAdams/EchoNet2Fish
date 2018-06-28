#' Assign Groups to Data on the Log Scale
#'
#' Assign groups to a collection of values, on the log-transformed scale.
#' @param x
#'   Numeric vector of values to group.
#' @param n
#'   Integer scalar, the number of groups.
#' @param xR
#'   Numeric vector of length two, range of values used to define groups,
#'   default is the range of finite, positive, non-missing \code{x} values.
#' @export
#' @examples
#' myx <- c(100, 3000, 3, 20, 8000, 1000, 400, 50, 7, 20000)
#' groupLog(myx, n=3)
#'
groupLog <- function(x, n, xR=range(x[x>0 & !is.na(x) & is.finite(x)])) {
  sel <- x>0 & !is.na(x) & is.finite(x)
  x2 <- x[sel]
  mybrks <- 10^quantile(log10(x2), seq(0, 1, length=n+1))
	grp <- rep(NA, length(x))
	grp[sel] <- cut(x2, breaks=mybrks, include.lowest=TRUE, labels=FALSE)
	return(grp)
}
