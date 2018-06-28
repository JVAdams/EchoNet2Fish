#' Multipanel Map of Locations
#'
#' Multipanel map of locations, one map for each group.
#' @param bygroup
#'   Vector, identifying the group membership of the locations to be mapped.
#' @param sug
#'   Vector, identifying the unique groups to which locations may belong,
#'   default sort(unique(\code{bygroup})).
#' @param plottext
#'   Logical scalar indicating if text (TRUE) or symbols (FALSE, default)
#'   should be printed at each location.
#' @param ID
#'   Vector, plot symbol (if \code{plottext}=FALSE) or text
#'   (if \code{plottext}=TRUE)
#'   to print on the map at each location, either of length 1 or
#'   the same length as \code{bygroup}, default 1.
#' @param emphasis
#'   Logical vector, indicating observations that should be emphasized on the
#'   map, same length as \code{bygroup}, default NULL.
#' @param short
#'   Logical scalar, indicating aspect of map area.  If TRUE, the default,
#'   the mapped area is assumed to be wider (longitudinally) than tall.
#'   Used to better arrange multiple maps on a single page.
#' @param samescale
#'   Logical scalar, indicating if the same (TRUE, default) or different
#'   (FALSE) lon/lat scales should be used for all panels.
#' @param rlon
#'   A numeric vector of length 2, range of longitudes to map,
#'   in decimal degrees.  The default, NULL, means that the range of \code{lon}
#'   will be used, either over all panels (if \code{samescale}=TRUE) or
#'   for each panel separately (if \code{samescale}=FALSE).
#' @param rlat
#'   A numeric vector of length 2, range of latitudes to map,
#'   in decimal degrees.  The default, NULL, means that the range of \code{lat}
#'   will be used, either over all panels (if \code{samescale}=TRUE) or
#'   for each panel separately (if \code{samescale}=FALSE).
#' @param cushion
#'   A numeric scalar indicating the amount of cushion to add to the \code{rlon}
#'   and \code{rlat} ranges in decimal degrees, default 0.1.
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
#'   mapByGroup
#' @import grDevices graphics
#' @export
#' @examples
#' \dontrun{
#'  mygroup <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)
#'  myID <- LETTERS[1:9]
#'  mylon <- rnorm(9, mean=-82)
#'  mylat <- rnorm(9, mean=45)
#'  mapMulti(bygroup=mygroup, plottext=TRUE, ID=myID,
#'    emphasis=myID %in% c("G", "A"), cushion=0, lon=mylon, lat=mylat)
#'  mapMulti(bygroup=mygroup, sug=1:4, short=FALSE, lon=mylon, lat=mylat,
#'    samescale=FALSE)
#' }
#'
mapMulti <- function(bygroup, sug=sort(unique(bygroup)), plottext=FALSE, ID=1,
  emphasis=NULL, short=TRUE, lon, lat, samescale=TRUE, rlon=NULL, rlat=NULL,
  cushion=0.1, IDcol=NULL, mapcol="gray", boxcol="gray", misscol="brown",
  misstext=" - No sites", mar=c(0, 0, 2.5, 0)) {

  mf <- n2mfrow(length(sug))
  if(!short) {
    mf <- rev(mf)
  }
  par(mfrow=mf, mar=mar)

  if(is.null(emphasis)) {
    emphasis <- rep(FALSE, length(bygroup))
  }

  if(is.null(rlon) & samescale) {
    rlon <- range(lon, na.rm=TRUE)
  }
  if(is.null(rlat) & samescale) {
    rlat <- range(lat, na.rm=TRUE)
  }

  if(length(ID)==1) ID <- rep(ID, length(bygroup))

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
		if(sum(selm[!is.na(selm)])>0) {
  		if(is.null(rlon)) {
        rlon. <- range(lon[selm], na.rm=TRUE)
  		} else {
  		  rlon. <- rlon
  		}
  		if(is.null(rlat)) {
        rlat. <- range(lat[selm], na.rm=TRUE)
  		} else {
  		  rlat. <- rlat
  		}
  		maps::map("world", xlim=rlon. + cushion*c(-1, 1), ylim=rlat. + cushion*c(-1, 1),
        mar=mar, col=mapcol)
      maps::map("lakes", add=TRUE, col=mapcol)
			bold <- emphasis + 1
			par(xpd=NA)
			if(plottext) {
  			text(lon[selm], lat[selm], ID[selm], col=IDcol[selm], cex=bold,
  			  font=bold)
			} else {
  			points(lon[selm], lat[selm], pch=ID[selm], col=IDcol[selm], cex=bold,
  			  lwd=bold)
			}
			par(xpd=FALSE)
			mtext(sug[i], side=3)
		} else {
		  plot(0, 0, type="n", axes=FALSE, xlab="", ylab="")
			mtext(paste0(sug[i], misstext), side=3, col=misscol)
		}
		box(col=boxcol)
	}
}
