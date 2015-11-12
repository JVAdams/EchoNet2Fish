#' Plot Acoustic Survey Data using Different Colored Symbols
#'
#' Plot acoustic survey data, interval vs. layer, using different colored
#' symbols for data exploration purposes.  Place multiple group-specific plots
#' on one page, using the same x- and y-scales.
#' @param interval
#'   A numeric vector of intervals along the length of an acoustic transect.
#' @param layer
#'   A numeric vector of layers from surface to bottom along the vertical
#'   water column of an acoustic transect, all values should be <= 0,
#'   the same length as \code{interval}.
#' @param group
#'   A vector of group identifiers, the same length as \code{interval}.
#' @param grouporder
#'   A vector of unique group identifiers, providing the order that each group
#'   will be plotted, the same length as \code{unique(group)}, default
#'   \code{sort(unique(group))}.
#' @param colorz
#'   A vector of character or numeric colors to use,
#'   the same length as \code{interval}.
#' @param main
#'   A character scalar of the main title of the plot, default "".
#' @details
#' 	 The \code{column1name} argument is needed to handle occasional problems
#' 	 with byte order marks at the beginning of the csv files, which can result
#' 	 in strange characters being added to the name of the first column.
#' 	 See, for example, this
#' 	 \href{http://stackoverflow.com/questions/15259139/when-i-import-text-file-into-r-i-get-a-special-character-appended-to-the-first}{link}.
#' @export

plotIntLay <- function(interval, layer, group, grouporder=sort(unique(group)),
  colorz, main="") {
	# plot interval versus layer with different colors for symbols
	par(mfrow=n2mfrow(length(grouporder)), mar=c(2, 2, 2, 1), oma=c(2, 2, 2, 0),
	  cex=1)
	for(i in seq_along(grouporder)) {
		sel <- group==grouporder[i]
		plot(interval[sel], layer[sel], col=colorz[sel], pch=16,
			ylim=range(layer, na.rm=TRUE), xlab="", ylab="", main=grouporder[i])
	}
	mtext("Interval", side=1, outer=T, line=1, cex=1.5)
	mtext("Layer depth", side=2, outer=T, line=1, cex=1.5)
	mtext(main, side=3, outer=T, cex=1.5)
}
