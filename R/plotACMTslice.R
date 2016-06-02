#' Multipanel Plot of AC and MT Samples by Slice
#'
#' Multipanel plot of acoustics and midwater trawl samples by slice.
#' @param MTgroup
#'   Vector, identifying the group membership (typically, the slice)
#'   of the midwater trawl tow locations to be mapped.
#' @param ACgroup
#'   Vector, identifying the group membership (typically, the slice)
#'   of the acoustic transect locations to be mapped.
#' @param sug
#'   Vector, identifying the unique groups to which locations may belong,
#'   default sort(unique(c(\code{MTgroup}, \code{ACgroup}))).
#' @param MTbd
#'   Numeric vector of bottom depths sampled by midwater trawl tows,
#'   same length as \code{MTgroup}.
#' @param ACbd
#'   Numeric vector of bottom depths sampled by acoustic transects,
#'   same length as \code{ACgroup}.
#' @param MTwd
#'   Numeric vector of water depths sampled by midwater trawl tows,
#'   same length as \code{MTgroup}.
#' @param ACwd
#'   Numeric vector of water depths sampled by acoustic transects,
#'   same length as \code{ACgroup}.
#' @param Gcol
#'   A vector, the color used for plot symbols,
#'   same length as \code{sug}.
#'   If NULL, the default, colors will be assigned automatically.
#' @param boxcol
#'   A scalar, the color of the three- to five-sided box around the scatterplot,
#'   default "gray".
#' @param axlabelcol
#'   A scalar, the color of the axis labels, default "gray".
#' @param axtitlecol
#'   A scalar, the color of the axis titles, default "darkgray".
#' @param oma
#'   A numeric vector of length 4, the number of lines of outer margin
#'   c(bottom, left, top, right) around the multipanel plots, default
#'   c(1.5, 2, 1.5, 2).
#' @inheritParams
#'   mapMulti
#' @import graphics
#' @export
#' @examples
#' # acoustic transects
#' agroup <- c("Main", "Bay")[c(1, 1, 1, 1, 2, 2, 2)]
#' abd <- c(50, 100, 50, 100, 50, 100, 50)
#' awd <- c(10, 10, 20, 20, 10, 10, 20)
#'
#' # midwater trawls
#' mgroup <- c("Main", "Bay")
#' mbd <- c(30, 80)
#' mwd <- c(15, 60)
#'
#' plotACMTslice(MTgroup=mgroup, ACgroup=agroup, MTbd=mbd, ACbd=abd,
#'   MTwd=mwd, ACwd=awd)
#'
plotACMTslice <- function(MTgroup, ACgroup,
  sug=sort(unique(c(MTgroup, ACgroup))),
  MTbd, ACbd, MTwd, ACwd, Gcol=NULL, boxcol="gray",
  axlabelcol="gray", axtitlecol="darkgray",
  mar=c(0, 0, 3, 3), oma=c(1.5, 2, 2.5, 2)) {

  oddgroups <- setdiff(unique(MTgroup), ACgroup)
  if(length(oddgroups)>0) warning(
    "Some slices were sampled with midwater trawls but not acoustics.")

  oddgroups2 <- setdiff(unique(ACgroup), MTgroup)
  if(length(oddgroups2)>0) warning(
    "Some slices were sampled with acoustics but not midwater trawls.")

	par(mfrow=c(length(sug), 2), mar=mar, oma=oma)

  iord <- 1:length(sug)
  if(is.null(Gcol)) {
    Gcol <- iord
  }
  xr <- range(ACbd, MTbd, na.rm=TRUE)
  yr <- range(-ACwd, -MTwd, na.rm=TRUE)

	for(i in iord) {

		# plot AC data
		sel <- ACgroup==sug[i]
		plot(1, 1, xlim=xr, ylim=yr, type="n", axes=FALSE, xlab="", ylab="")
		points(jitter(ACbd)[sel], -jitter(ACwd)[sel], col=Gcol[i])
		axis(3, col=boxcol, col.axis=axlabelcol, cex.axis=1.5)
		axis(4, las=1, at=axTicks(4), labels=-axTicks(4), col=boxcol,
		  col.axis=axlabelcol, cex.axis=1.5)
		pusr <- par("usr")
		lines(c(pusr[2], -pusr[3], pusr[1], pusr[1]),
		  c(pusr[3], pusr[3], -pusr[1], pusr[4]), col=boxcol)
		box(bty="7", col=boxcol)
		mtext(sug[i], side=2, cex=1.2)
		if(i==1) mtext("AC", side=3, line=4, cex=1.2)

		# plot MT data
		sel2 <- MTgroup==sug[i]
		plot(1, 1, xlim=xr, ylim=yr, type="n", axes=FALSE, xlab="", ylab="")
		points(MTbd[sel2], -MTwd[sel2], col=Gcol[i], lwd=2, cex=2)
		axis(3, col=boxcol, col.axis=axlabelcol, cex.axis=1.5)
		axis(4, las=1, at=axTicks(4), labels=-axTicks(4), col=boxcol,
		  col.axis=axlabelcol, cex.axis=1.5)
		lines(c(pusr[2], -pusr[3], pusr[1], pusr[1]),
		  c(pusr[3], pusr[3], -pusr[1], pusr[4]), col=boxcol)
		box(bty="7", col=boxcol)
		if(i==1) mtext("MT", side=3, line=4, cex=1.2)
	}
	mtext("Bottom depth  (m)", side=3, outer=TRUE, line=-0.5, col=axtitlecol,
		cex=1.2)
	mtext("Water depth  (m)", side=4, outer=TRUE, line=0.5, col=axtitlecol,
	  cex=1.2)
}
