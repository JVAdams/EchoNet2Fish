#' Draw a Map using Different Colored Symbols for Groups
#'
#' Draw a map using different colored symbols for different groups.
#' @param bygroup
#'   Vector, identifying the group membership of the locations to be mapped.
#' @param sug
#'   Vector, identifying the unique groups to which locations may belong,
#'   default sort(unique(\code{bygroup})).
#' @param addMean
#'   Logical scalar indicating if group names should be added to plot at the
#'   mean location of each bygroup, default TRUE.
#' @param lon
#'   A numeric vector of longitudes in decimal degrees.
#' @param lat
#'   A numeric vector of latitudes in decimal degrees.
#'   Same length as \code{lon}.
#' @param rlon
#'   A numeric vector of length 2, range of longitudes to map,
#'   in decimal degrees, default is the range of \code{lon}.
#' @param rlat
#'   A numeric vector of length 2, range of latitudes to map,
#'   in decimal degrees, default is the range of \code{lat}.
#' @param cushion
#'   A numeric scalar indicating the amount of cushion to add to the \code{rlon}
#'   and \code{rlat} ranges in decimal degrees, default 0.1.
#' @param colorz
#'   A vector of colors to use, either of length 1 or the same length as
#'   \code{lon}.  If NULL, the default, a range of colors will be assigned
#'   automatically.
#' @param pch
#'   A vector of plotting characters or symbols, either of length
#'   1 or the same length as \code{lon}, default 1.
#'   See \code{\link[graphics]{points}}.
#' @param cex
#'   A numeric vector giving the amount by which plotting characters and
#'   symbols should be scaled relative to the default, either of length
#'   1 or the same length as \code{lon}, default 1.5.
#' @param mapcol
#'   A scalar, the color used to draw the map lines (e.g., lake boundary),
#'   default "gray".
#' @param mar
#'   A numeric vector of length 4, the number of lines of margin
#'   c(bottom, left, top, right) around the plotted map plot, default
#'   c(0, 0, 0, 0).
#' @export
#' @examples
#' \dontrun{
#'  mygroup <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)
#'  mylon <- -c(81.1, 81.2, 80.5, 83, 82.2, 82.7, 82.7, 82, 82.2)
#'  mylat <- c(45.7, 45.4, 45, 45.5, 45.4, 45, 44.4, 44.4, 43.9)
#'  mapByGroup(bygroup=mygroup, lon=mylon, lat=mylat, cushion=0.7)
#' }
#'
mapByGroup <- function(bygroup, sug=sort(unique(bygroup)), addMean=TRUE,
  lon, lat, rlon=range(lon, na.rm=TRUE), rlat=range(lat, na.rm=TRUE),
  cushion=0.1, colorz=NULL, pch=1, cex=1.5, mapcol="gray", mar=c(0, 0, 2, 0)) {

  maps::map("world", xlim=rlon + cushion*c(-1, 1), ylim=rlat + cushion*c(-1, 1),
	  col=mapcol, mar=mar)
  maps::map("lakes", add=TRUE, col=mapcol)
  if(is.null(colorz)) {
    colorz <- as.numeric(as.factor(bygroup))
  }
	points(lon, lat, pch=pch, cex=cex, col=colorz)
	if(addMean) {
  	text(tapply(lon, bygroup, mean), tapply(lat, bygroup, mean),
  	  sug, cex=2, col=colorz[match(sort(sug), bygroup)])
	}
}
