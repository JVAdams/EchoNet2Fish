#' Multipanel Map of Locations, with Gaps
#'
#' Multipanel map of locations, arranged according to membership in the first
#' of two different groupings, with gaps (empty plot panels)
#' between the first groups.
#' @param df
#'   Data frame of values to plot, column names should identify membership
#'   in two groups, separated by a period, "group1.group2".
#' @param lon
#'   Numeric vector, longitudes of locations to map,
#'   in decimal degrees, same length as \code{MTgroup}.
#' @param lat
#'   Numeric vector, latitudes of locations to map,
#'   in decimal degrees, same length as \code{MTgroup}.
#' @param rlon
#'   A numeric vector of length 2, range of longitudes to map,
#'   in decimal degrees, default range(\code{lon}, na.rm=TRUE).
#' @param rlat
#'   A numeric vector of length 2, range of latitudes to map,
#'   in decimal degrees, default range(\code{lat}, na.rm=TRUE).
#' @param nrows
#'   A numeric scalar, the number of rows of plot panels in the page.
#' @param nsymbols
#'   An integer scalar, between 1 and 7 inclusive,
#'   the number of different symbol sizes/colors to plot,
#'   default 7.
#' @param mar
#'   A numeric vector of length 4, the number of lines of margin
#'   c(bottom, left, top, right) around each plotted map plot, default
#'   c(0, 0, 3, 0).
#' @importFrom RColorBrewer brewer.pal
#' @import graphics
#' @export
#' @examples
#' \dontrun{
#'  mydf <- as.data.frame(matrix(rlnorm(35, 5), nrow=7, dimnames=list(NULL,
#'    c("Dog.large", "Dog.small", "Cat.large", "Cat.small", "Fish.all"))))
#'  mylon <- -c(83, 83, 80.4, 81)
#'  mylat <- c(45.4, 44.5, 45, 45.5)
#'  mapBy2Groups(df=mydf, lon=mylon, lat=mylat, nrows=3)
#'  }
#'
mapBy2Groups <- function(df, lon, lat, rlon=range(lon, na.rm=TRUE),
  rlat=range(lat, na.rm=TRUE), nrows, nsymbols=7, mar=c(0, 0, 3, 0)) {

  if(nsymbols < 1 | nsymbols > 7)
    stop("nsymbols must be between 1 and 7, inclusive")

	namz <- names(df)
	sp.grps <- strsplit(namz, "\\.")
	grp.sp <- sapply(sp.grps, "[", 1)
	npanels <- length(grp.sp) + length(unique(grp.sp)) - 1
	ncols <- ceiling(npanels/nrows)

	symsize <- seq(0.5, 2.5, length=nsymbols)
  mypalette <- RColorBrewer::brewer.pal(nsymbols+2, "GnBu")[-(1:2)]

	y <- unlist(df)
	RY <- range(y[y>0 & !is.na(y) & is.finite(y)])
	par(mfrow=c(nrows, ncols), mar=mar)
	for(i in seq(namz)) {
		if(i>1) if(grp.sp[i]!=grp.sp[i-1]) frame()
		# divide the nonzero data into nsymbols groups on a log scale
  	symb <- groupLog(df[, i], n=nsymbols, xR=YR)
		maps::map("world", xlim=rlon + 0.1*c(-1, 1),
		  ylim=rlat + 0.1*c(-1, 1), mar=mar, col="gray")
    maps::map("lakes", add=TRUE, col="gray")
		mtext(namz[i], side=3)
		points(lon, lat, cex=symsize[symb], col=mypalette[symb])
	}
}
