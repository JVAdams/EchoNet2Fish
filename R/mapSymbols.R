#' Draw a Map using Different Colored Symbols
#'
#' Draw a map using different colored symbols for data exploration purposes.
#' @param lat
#'   A numeric vector of latitudes in decimal degrees.
#' @param long
#'   A numeric vector of longitudes in decimal degrees.
#'   Same length as\code{lat}.
#' @param colorz
#'   A vector of character or numeric colors to use, either of length
#'   1 or the same length as \code{lat} and \code{long}.
#' @param main
#'   A character scalar of the main title of the plot, default "".
#' @param pch
#'   A vector of plotting characters or symbols, either of length
#'   1 or the same length as \code{lat} and \code{long}, default 1.
#'   See \code{\link[graphics]{points}}.
#' @param cex
#'   A numeric vector giving the amount by which plotting characters and
#'   symbols should be scaled relative to the default, either of length
#'   1 or the same length as \code{lat} and \code{long}, default 1.5.
#' @param xla
#'   A numeric scalar giving an added margin of decimal degrees to be mapped
#'   beyond the range of longitudes in \code{long}, default 0.
#' @param yla
#'   A numeric scalar giving an added margin of decimal degrees to be mapped
#'   beyond the range of latitudes in \code{lat}, default \code{xla}.
#' @details
#' 	 The \code{column1name} argument is needed to handle occasional problems
#' 	 with byte order marks at the beginning of the csv files, which can result
#' 	 in strange characters being added to the name of the first column.
#' 	 See, for example, this
#' 	 \href{http://stackoverflow.com/questions/15259139/when-i-import-text-file-into-r-i-get-a-special-character-appended-to-the-first}{link}.
#' @import
#'   maps mapdata
#' @export
#' @examples
#' \dontrun{
#'  latitude <- c(43.25, 45.73, 45.71, 44.84)
#'  longitude <- c(-82.30, -80.85, -84.03, -80.39)
#'  basincode <- c(1, 2, 1, 2)
#'  mapSymbols(lat=latitude, long=longitude, colorz=basincode+3,
#'   pch=16, xla=0.4)
#' }

mapSymbols <- function(lat, long, colorz, main="", pch=1, cex=1.5,
    xla=0, yla=xla) {
	map("worldHires", xlim=range(long, na.rm=TRUE) + c(-1, 1)*xla,
	  ylim=range(lat, na.rm=TRUE) + c(-1, 1)*yla, mar=c(0, 0, 2, 0))
	points(long, lat, pch=pch, cex=cex, col=colorz)
	box()
	mtext(main, side=3, cex=1.2)
}
