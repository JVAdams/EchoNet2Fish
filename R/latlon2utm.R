#' Convert Lon/Lat to UTM
#'
#' Convert longitude and latitude coordinates to Universal Transverse
#' Mercator coordinates.
#' @param lon
#'   Numeric vector of longitudes.
#' @param lat
#'   Numeric vector of latitudes, same length as \code{lon}.
#' @param zone
#'   Numeric scalar of the UTM zone, default NULL.  See details.
#' @param quiet
#'   Logical scalar indicating whether to refrain from printing the determined
#'   zone when \code{zone} is NULL, default TRUE.
#' @details
#'   If \code{zone} is NULL, then the zone is determined from the medians
#'   of \code{lon} and \code{lat}.
#' @return
#'   A data frame with two columns (easting and northing) and as many rows as
#'   the length of \code{lon} containing the converted UTM coordinates in
#'   meters.
#' @export
#' @seealso
#'   \code{\link{lon2utmZone}}
#' @references
#'   Based on a function posted by Stanislav on 13 May 2015 on stackoverflow
#'   \href{http://stackoverflow.com/a/30225804/2140956}{[link]}.
#' @importFrom sp coordinates proj4string CRS spTransform
#' @examples
#' mylon <- c(-92.11, -76.47, -82.27, -83.42)
#' mylat <- c(46.76, 44.34, 44.76, 45.41)
#' latlon2utm(mylon, mylat, quiet=FALSE)
#' sapply(1:length(mylon), function(i)
#'   latlon2utm(mylon[i], mylat[i], quiet=FALSE))
#' latlon2utm(mylon, mylat, 18)
#'
latlon2utm <- function(lon, lat, zone=NULL, quiet=TRUE) {
  if(is.null(zone)) {
    zone <- lon2utmZone(median(lon, na.rm=TRUE), median(lat, na.rm=TRUE))
    if(!quiet) print(zone)
  }
  xy <- data.frame(easting=lon, northing=lat)
  sp::coordinates(xy) <- c("easting", "northing")
  sp::proj4string(xy) <- sp::CRS("+proj=longlat +datum=WGS84")
  res <- sp::spTransform(xy, sp::CRS(paste0("+proj=utm +zone=", zone, " ellps=WGS84")))
  return(as.data.frame(res))
}
