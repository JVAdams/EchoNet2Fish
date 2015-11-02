#' Add Text to a Map
#'
#' Add identifying text to a map based on a single grouping variable.
#' @param lat
#'   A numeric vector of latitudes in decimal degrees.
#' @param long
#'   A numeric vector of longitudes in decimal degrees.
#'   Same length as\code{lat}.
#' @param group
#'   A character or numeric vector of group identifiers,
#'   the same length as \code{lat} and \code{long}.
#' @param cex
#'   A numeric scalar giving the amount by which plotting characters
#'   should be scaled relative to the default, default 1.5.
#' @param ...
#'   Additional arguments to \code{\link[graphics]{text}}.
#' @details
#' 	 Group identifiers are added to a map, typically created with
#' 	 \code{\link{mapSymbols}}, at the midpoint of the range of each groups'
#' 	 latitudes and longitudes
#' @import
#'   maps mapdata
#' @export
#' @examples
#' \dontrun{
#'  latitude <- c(43.25, 45.73, 45.71, 44.84)
#'  longitude <- c(-82.30, -80.85, -84.03, -80.39)
#'  basincode <- c(1, 2, 1, 2)
#'  basin <- c("Main", "GBay", "Main", "GBay")
#'  mapSymbols(lat=latitude, long=longitude, colorz=basincode+3,
#'   pch=16, xla=0.4)
#'  mapText(lat=latitude, long=longitude, group=basin)
#' }

mapText <- function(lat, long, group, cex=1.5, ...) {
	# add text to plot at group lat/long midpoints
	sug <- sort(unique(group))
	lng <- tapply(long, group, midpoint)
	ltg <- tapply(lat, group, midpoint)
	text(lng, ltg, sug, cex=cex, ...)
}
