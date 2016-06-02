#' Plot Boxplots of Continuous Data by Species
#'
#' Boxplots of species versus continuous data on the square root scale.
#' @param x
#'   A vector (can be numeric, character, or factor) identifying species,
#'   default \code{Species}.
#' @param y
#'   A numeric vector of data to plot against species, the same length as
#'   \code{x}.
#' @param ylabb
#'   A character scalar of the y-axis label.
#' @param uniqSp
#'   A vector of unique species, a subset of \code{x}, default
#'   \code{sort(unique(x))}.
#' @import graphics
#' @export

plotSpecies <- function(y, ylabb, x=Species, uniqSp=sort(unique(x))) {
	par(mar=c(4, 4, 1, 1))
	plot(as.factor(x), sqrt(y), xlab="Species", ylab=ylabb, axes=FALSE)
	axis(1, at=seq(uniqSp), labels=uniqSp)
	axis(2, at=pretty(sqrt(y)), labels=pretty(sqrt(y))^2, las=1)
	box()
}
