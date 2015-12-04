#' Determine UTM Zone from Longitude
#'
#' Determine the Universal Transverse Mercator zone that contains the given
#' longitude coordinates.
#' @param lon
#'   Numeric vector of longitudes.
#' @param lat
#'   Numeric vector of latitudes, same length as \code{lon}, default NULL.
#' @return
#'   A numeric vector giving the UTM zones (integers) containing the \code{lon}.
#'   If \code{lat} is not NULL, then zones are corrected to account for
#'   the special cases of Svalbard and parts of Norway.
#' @export
#' @references
#'   Based on a function posted by Josh O'Brien on 8 Feb 2012 on stackoverflow
#'   \href{http://stackoverflow.com/a/9188972/2140956}{[link]}.
#'   and code for exceptions posted by wittrup on 13 Sep 2013 on stackoverflow
#'   \href{http://stackoverflow.com/a/18785421/2140956}{[link]}.
#' @examples
#' mylon <- c(-82.27, NA, -122.44, 21.97, 21.97)
#' mylat <- c(44.76, 37.77, 37.77, NA, 78.64)
#' lon2utmZone(mylon)
#' lon2utmZone(mylon, mylat)
#'

lon2utmZone <- function(lon, lat=NULL) {
  z <- (floor((lon + 180)/6) %% 60) + 1
  if(!is.null(lat)) {
    nm <- !is.na(lon) & !is.na(lat)
    z[nm & lat>55 & z==31 & lat<64 & lon>2] <- 32
    z[nm & lat>71 & z==32 & lon<9] <- 31
    z[nm & lat>71 & z==32 & lon>8] <- 33
    z[nm & lat>71 & z==34 & lon<21] <- 33
    z[nm & lat>71 & z==34 & lon>20] <- 35
    z[nm & lat>71 & z==36 & lon<33] <- 35
    z[nm & lat>71 & z==36 & lon>32] <- 37
  }
  return(z)
}
