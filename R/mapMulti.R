#' Multipanel Map of Locations
#'
#' Multipanel map of locations, one map for each group.
#' @param bygroup
#'   Vector, identifying the group membership of the locations to be mapped.
#' @param sug
#'   Vector, identifying the unique groups to which locations may belong,
#'   default sort(unique(\code{bygroup})).
#' @param ID
#'   Vector, text to print on the map at each location,
#'   same length as \code{bygroup}.
#' @param boldID
#'   Vector, subset of unique \code{ID}s to be emphasized on the map
#'   with large bold font.
#' @param short
#'   Logical scalar, indicating aspect of map area.  If TRUE, the default,
#'   the mapped area is assumed to be wider (longitudinally) than tall.
#'   Used to better arrange multiple maps on a single page.
#' @param rlon
#'   A numeric vector of length 2, range of longitudes to map,
#'   in decimal degrees, default range(\code{lon}, na.rm=TRUE).
#' @param rlat
#'   A numeric vector of length 2, range of latitudes to map,
#'   in decimal degrees, default range(\code{lat}, na.rm=TRUE).
#' @param IDcol
#'   A vector, the color used to map locations, same length as \code{bygroup}.
#'   If NULL, the default, a range of colors will be assigned automatically.
#' @param mapcol
#'   A scalar, the color used to draw the map lines (e.g., lake boundary),
#'   default "gray".
#' @param boxcol
#'   A scalar, the color used to draw the box around the map, default "gray".
#' @param misscol
#'   A scalar, the color used to label maps with no locations in the
#'   given bygroup, default "brown".
#' @param misstext
#'   A character scalar, the text used to label maps with no locations in the
#'   given bygroup, default " - No sites".
#' @param mar
#'   A numeric vector of length 4, the number of lines of margin
#'   c(bottom, left, top, right) around each plotted map plot, default
#'   c(0, 0, 2.5, 0).
#' @inheritParams
#'   mapText
#' @export
#' @examples
#' \dontrun{
#'  mygroup <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)
#'  myID <- LETTERS[1:9]
#'  mylon <- rnorm(9, mean=-82)
#'  mylat <- rnorm(9, mean=45)
#'  mapMulti(bygroup=mygroup, ID=myID, boldID=c("G", "A"), lon=mylon, lat=mylat)
#'  mapMulti(bygroup=mygroup, sug=1:4, ID=myID, lon=mylon, lat=mylat)
#' }
#'
mapMulti <- function(bygroup, sug=sort(unique(bygroup)), ID, boldID=NULL,
  short=TRUE, lon, lat, rlon=range(lon, na.rm=TRUE),
  rlat=range(lat, na.rm=TRUE), IDcol=NULL, mapcol="gray", boxcol="gray",
  misscol="brown", misstext=" - No sites", mar=c(0, 0, 2.5, 0)) {

  mf <- n2mfrow(length(sug))
  if(!short) {
    mf <- rev(mf)
  }
  par(mfrow=mf)

  if(is.null(IDcol)) {
    # assign colors so that like colors are geographically separated
    loc1dim <- cmdscale(dist(latlon2utm(lon, lat)), k=1)
    separate <- rep(1:3, length.out=length(loc1dim))
    IDcol <- colorVal(1:length(ID), n=length(ID), low=6/6, high=2/6)[
      order(loc1dim)[order(separate)]]
  }

  iord <- 1:length(sug)
	for(i in iord) {
		selm <- bygroup==sug[i] & !is.na(bygroup)
#		map("worldHires", xlim=rlon, ylim=rlat, mar=mar, col=mapcol)
		map("world", xlim=rlon, ylim=rlat, mar=mar, col=mapcol)
		box(col=boxcol)
		if(sum(selm[!is.na(selm)])>0) {
			bold <- if(is.null(boldID)) 1 else ((ID[selm] %in% boldID) + 1)
			par(xpd=NA)
			text(lon[selm], lat[selm], ID[selm], col=IDcol[selm], cex=bold, font=bold)
			par(xpd=FALSE)
			mtext(sug[i], side=3)
		} else {
			mtext(paste0(sug[i], misstext), side=3, col=misscol)
		}
	}
}
