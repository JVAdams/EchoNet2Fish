#' Plot Age-LENGTH Key
#'
#' Plot age-length key as a bubble plot.
#' @param m
#'   A numeric matrix of counts or relative frequencies with the two dimensions
#'   representing ages and lengths.
#' @param inc
#'   A numeric scalar giving the radius (in inches) of the bubble representing
#'   the largest value in \code{m}, default 0.3.
#' @param zeroes
#'   A scalar giving the symbol to be used for plotting zeroes, default 4.
#'   See the argument \code{pch} of the \code{\link{points}} function
#'   for possible values and their interpretation.
#' @param fg
#'   A scalar giving the color the circles are to be drawn in.
#' @param bg
#'   A scalar giving the color with which the circles are to be filled.  The default,
#'   NA, leaves the symbols unfilled.
#' @param ...
#'   Additional arguments to \code{\link{plot}}.
#' @details
#'   The matrix \code{m} can have ages in rows and lengths in
#'   columns or vice versa.  In either case, unique row values are plotted
#'   along the x-axis, unique column values are plotted along the y-axis.
#'
#'   Values in the matrix \code{m} are represented as circles, with zero
#'   values represented by black dots.
#' @import graphics
#' @export
#' @examples
#' mymat <- matrix(c(10, 1, 0, 0, 5, 0, 0, 3, 2, 0, 0, 1), nrow=3,
#'  dimnames=list(age=1:3, len=seq(25, 55, 10)))
#' plotAgeLen(mymat, inc=1)
#' plotAgeLen(t(mymat), xlab="LENGTH", ylab="Age")
#'
plotAgeLen <- function(m, inc=0.3, zeroes=4, fg="red", bg=NA, ...) {
  # plots values of matrix as different sized circles
  # dimension 1 is plotted on x-axis, dimension 2 on y-axis
  uxy <- dimnames(m)
  ux <- as.numeric(uxy[[1]])
  uxlab <- NULL
  if(any(is.na(ux))) {
    ux <- seq(along=ux)
    uxlab <- uxy[[1]]
  }
  uy <- as.numeric(uxy[[2]])
  uylab <- NULL
  if(any(is.na(uy))) {
    uy <- seq(along=uy)
    uylab <- uxy[[2]]
  }
  cush.x <- mean(abs(diff(ux)))
  cush.y <- mean(abs(diff(uy)))
  x <- ux[row(m)]
  y <- uy[col(m)]
  z <- sqrt(abs(as.vector(m)))
  plot(x, y, type="n", xlim=range(ux) + cush.x*c(-1, 1),
       ylim=range(uy) + cush.y*c(-1, 1), axes=FALSE, ...)
  symbols(x, y, circles=z, inches=inc, fg=fg, bg=bg, add=TRUE)
  sel <- !is.na(z) & abs(z)<1e-7
  points(x[sel], y[sel], pch=zeroes)
  if(is.null(uxlab)) axis(1) else axis(1, at=ux, labels=uxlab)
  if(is.null(uylab)) axis(2, las=1) else axis(2, at=uy, labels=uylab, las=1)
  box()
}
