#' Multipanel Map of Locations
#'
#' Multipanel map of locations, one map for each group.
#' @param MTgroup
#'   Vector, identifying the group membership of the midwater trawl tow
#'   locations to be mapped.  The unique values of \code{MTgroup} should be
#'   a subset of \code{ACgroup}.
#' @param ACgroup
#'   Vector, identifying the group membership of the acoustic transect
#'   locations to be mapped.
#' @param sug
#'   Vector, identifying the unique groups to which locations may belong,
#'   default sort(unique(c(\code{MTgroup}, \code{ACgroup}))).
#' @param MTID
#'   Vector, unique identification of each midwater trawl tow,
#'   same length as \code{MTgroup}.
#' @param ACID
#'   Vector, identification of acoustic transects used to apportion each
#'   acoustic transect,
#'   same length as \code{ACgroup}.
#' @param short
#'   Logical scalar, indicating aspect of map area.  If TRUE, the default,
#'   the mapped area is assumed to be wider (longitudinally) than tall.
#'   Used to better arrange multiple maps on a single page.
#' @param MTlon
#'   Numeric vector, longitudes of midwater trawl tow locations to map,
#'   in decimal degrees, same length as \code{MTgroup}.
#' @param MTlat
#'   Numeric vector, latitudes of midwater trawl tow locations to map,
#'   in decimal degrees, same length as \code{MTgroup}.
#' @param AClon
#'   Numeric vector, longitudes of acoustic transect locations to map,
#'   in decimal degrees, same length as \code{ACgroup}.
#' @param AClat
#'   Numeric vector, latitudes of acoustic transect locations to map,
#'   in decimal degrees, same length as \code{ACgroup}.
#' @param rlon
#'   A numeric vector of length 2, range of longitudes to map,
#'   in decimal degrees, default range(\code{MTlon}, \code{AClon}, na.rm=TRUE).
#' @param rlat
#'   A numeric vector of length 2, range of latitudes to map,
#'   in decimal degrees, default range(\code{MTlat}, \code{AClat}, na.rm=TRUE).
#' @param MTIDcol
#'   A scalar, the color used to map the midwater trawl locations,
#'   same length as \code{MTgroup}.
#'   If NULL, the default, a range of colors will be assigned automatically.
#' @param misstext
#'   A character scalar, the text used to label maps with no locations in the
#'   given bygroup, default " - No tows".
#' @inheritParams
#'   mapMulti
#' @seealso
#'   \code{\link{mapMulti}}
#' @import grDevices graphics
#' @export
#' @examples
#' \dontrun{
#'  # acoustic transects
#'  agroup <- c("Main", "Bay")[c(1, 1, 1, 1, 2, 2, 2)]
#'  aID <- c("A", "B", "B", "B", "C", "C", "D")
#'  alon <- -c(83.5, 82.6, 82, 82, 80.6, 80.5, 81.3)
#'  alat <- c(45.6, 44.5, 44.6, 44.1, 45.3, 44.8, 45.7)
#'
#'  # midwater trawls
#'  mgroup <- c("Main", "Bay")[c(1, 1, 2, 2)]
#'  mID <- c("A", "B", "C", "D")
#'  mlon <- -c(83, 83, 80.4, 81)
#'  mlat <- c(45.4, 44.5, 45, 45.5)
#'
#'  # illustrate assignment of midwater trawl tows to acoustic transects
#'  mapAppor(MTgroup=mgroup, ACgroup=agroup, MTID=mID, ACID=aID, MTlon=mlon,
#'    MTlat=mlat, AClon=alon, AClat=alat, rlon=c(-84, -80), rlat=c(43, 46))
#'  }
#'
mapAppor <- function(MTgroup, ACgroup, sug=sort(unique(c(MTgroup, ACgroup))),
  MTID, ACID, short=TRUE, MTlon, MTlat, AClon, AClat,
  rlon=range(MTlon, AClon, na.rm=TRUE),
  rlat=range(MTlat, AClat, na.rm=TRUE), MTIDcol=NULL, mapcol="gray",
  boxcol="gray", misscol="brown", misstext=" - No tows",
  mar=c(0, 0, 2.5, 0)) {

  oddgroups <- setdiff(unique(MTgroup), ACgroup)
  if(length(oddgroups)>0) stop(
    "The unique values of MTgroup should be a subset of ACgroup.")

  mf <- n2mfrow(length(sug))
  if(!short) {
    mf <- rev(mf)
  }
  par(mfrow=mf)

  if(is.null(MTIDcol)) {
    # assign colors so that like colors are geographically separated
    loc1dim <- cmdscale(dist(latlon2utm(MTlon, MTlat)), k=1)
    separate <- rep(1:3, length.out=length(loc1dim))
    MTIDcol <- colorVal(1:length(MTID), n=length(MTID), low=6/6, high=2/6)[
      order(loc1dim)[order(separate)]]
  }
  ACIDcol <- recode(ACID, MTID, MTIDcol)

  iord <- 1:length(sug)
	for(i in iord) {
		selm <- MTgroup==sug[i] & !is.na(MTgroup)
		sela <- ACgroup==sug[i] & !is.na(ACgroup)

		maps::map("world", xlim=rlon, ylim=rlat, mar=mar, col=mapcol)
    maps::map("lakes", add=TRUE, col=mapcol)
		box(col=boxcol)
		if(sum(selm[!is.na(selm)])>0) {
			points(AClon[sela], AClat[sela], col=ACIDcol[sela], pch=3)
			# add convex hull for each trawl haul
			sut <- sort(unique(ACID[sela]))
			for(j in seq(along=sut)) {
				selz <- sela & ACID==sut[j]
				hpts <- chull(AClon[selz], AClat[selz])
				hpts <- c(hpts, hpts[1])
				lines(AClon[selz][hpts], AClat[selz][hpts], lty=3)
			}
			mtext(sug[i], side=3)
			points(MTlon[selm], MTlat[selm], pch=16, col=MTIDcol[selm], cex=2)
			points(MTlon[selm], MTlat[selm], pch=21, bg="white", cex=1.5)
		} else {
			points(AClon[sela], AClat[sela], col="brown", pch=4)
			mtext(paste0(sug[i], misstext), side=3, col=misscol)
		}
	}
}
