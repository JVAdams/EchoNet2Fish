#' Multipanel Map of Locations
#'
#' Multipanel map of locations, one map for each group.
#' @param bygroup
#'   Vector, identifying the group membership of the locations to be mapped.
#' @param sug
#'   Vector, identifying the unique groups to which locations may belong,
#'   default sort(unique(\code{bygroup})).
#' @param rlon
#'   A numeric vector of length 2, range of longitudes to map,
#'   in decimal degrees, default
#'   range(\code{lon}, na.rm=TRUE) + 0.1*c(-1, 1).
#' @param rlat
#'   A numeric vector of length 2, range of latitudes to map,
#'   in decimal degrees, default
#'   range(\code{lat}, na.rm=TRUE) + 0.1*c(-1, 1).
#' @param IDcol
#'   A vector, the color used to map locations, same length as \code{bygroup}.
#'   If NULL, the default, a range of colors will be assigned automatically.
#' @param mapcol
#'   A scalar, the color used to draw the map lines (e.g., lake boundary),
#'   default "gray".
#' @param mar
#'   A numeric vector of length 4, the number of lines of margin
#'   c(bottom, left, top, right) around the plotted map plot, default
#'   c(0, 0, 0, 0).
#' @inheritParams
#'   mapText
#' @import
#'   maps
#' @export
#' @examples
#' mygroup <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)
#' mylon <- -c(81.1, 81.2, 80.5, 83, 82.2, 82.7, 82.7, 82, 82.2)
#' mylat <- c(45.7, 45.4, 45, 45.5, 45.4, 45, 44.4, 44.4, 43.9)
#' mapACstrata(bygroup=mygroup, ID=myID, lon=mylon, lat=mylat)
#'
mapACstrata <- function(bygroup, sug=sort(unique(bygroup)),
  lon, lat, rlon=range(lon, na.rm=TRUE) + 0.1*c(-1, 1),
  rlat=range(lat, na.rm=TRUE) + 0.1*c(-1, 1), IDcol=NULL, mapcol="gray",
  mar=c(0, 0, 0, 0)) {

  iord <- 1:length(sug)
  if(is.null(IDcol)) {
    IDcol <- iord
  }

	map("worldHires", xlim=rlon, ylim=rlat, mar=mar, col=mapcol)
	points(lon, lat, col=IDcol[match(bygroup, sug)])
	text(tapply(lon, bygroup, mean), tapply(lat, bygroup, mean),
	  sug, cex=2, col=IDcol)
}
