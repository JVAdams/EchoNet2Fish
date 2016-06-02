#' Multipanel Histogram of Values
#'
#' Multipanel histogram of values, one histogram for each group.
#' @param x
#'   Numeric vector of values to be plotted.
#' @param freq
#'   Numeric vector of frequencies corresponding to \code{x}, default
#'   a vector of 1s the  same length as \code{x}.
#' @param bygroup
#'   Vector, identifying the group membership of the values to be plotted.
#' @param sug
#'   Vector, identifying the unique groups to which locations may belong,
#'   default sort(unique(\code{bygroup})).
#' @param xlab
#'   Character scalar, label to assign to collection of x-axes.
#' @param ylab
#'   Character scalar, label to assign to collection of y-axes.
#' @param barw
#'   Numeric scalar, the width of the bars in units of \code{x}, default 5.
#' @param samescale
#'   Logical scalar, indicating if the same (TRUE, default) or different
#'   (FALSE) lon/lat scales should be used for all panels.
#' @param fillcol
#'   A scalar, the color used to fill the histograms, default "gray".
#' @param rangecol
#'   A scalar, the color used to draw the lines denoting the range of values,
#'   default "red".
#' @param mar
#'   A numeric vector of length 4, the number of lines of margin
#'   c(bottom, left, top, right) around each plotted map plot, default
#'   c(3, 3, 2, 1).
#' @param oma
#'   A numeric vector of length 4, the number of lines of the outer margin
#'   c(bottom, left, top, right) around the entire collection of panels,
#'   default c(2, 2, 1, 1).
#' @import grDevices graphics
#' @export
#' @examples
#' myval <- c(23, 35, 47, 62)
#' myfreq <- c(5, 2, 3, 4)
#' mygroup <- c("A", "A", "B", "B")
#' histMulti(x=myval, freq=myfreq, bygroup=mygroup)
#' histMulti(x=myval, freq=myfreq, bygroup=mygroup, samescale=FALSE)
#'
histMulti <- function(x, freq=rep(1, length(x)), bygroup,
  sug=sort(unique(bygroup)), xlab="Values", ylab="Frequency", barw=5,
  samescale=TRUE, fillcol="gray", rangecol="red", mar=c(3, 3, 2, 1),
  oma=c(2, 2, 1, 1)) {

  par(mfrow=n2mfrow(length(sug)), mar=mar, oma=oma, yaxs="i", cex=1)

	bks <- seq(-barw, max(x)+barw, barw)

  if(samescale) {
    xr <- range(x, na.rm=TRUE)
  	ally <- rep(x, freq)
    ymax <- max(table(cut(ally, bks)))
    yr <- c(0, ymax)
  }

	for(i in seq(along=sug)) {
		sel <- bygroup==sug[i]

    y <- rep(x[sel], freq[sel])
		thisxr <- range(x[sel])
		if(!samescale) {
		  xr <- thisxr
      ymax <- max(table(cut(y, bks)))
      yr <- c(0, ymax)
		}

		hist(y, xlim=xr+barw*c(-1, 1), ylim=c(0, ymax*1.05),
			breaks=bks, col=fillcol, main=sug[i], las=1)
		abline(v=thisxr, lwd=2, col=rangecol)
		box()
	}

	mtext(xlab, side=1, outer=TRUE, cex=1.5)
	mtext(ylab, side=2, outer=TRUE, cex=1.5)
}
