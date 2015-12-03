#' Estimate the Number of Targets in the Acoustic Beam
#'
#' Estimate the number of targets in the acoustic beam.
#' @param c
#'   Numeric scalar, speed of sound in m/s, default 1450.
#' @param tau
#'   Numeric scalar, pulse length in s, default 0.0004.
#' @param psi
#'   Numeric scalar, two-way equivalent beam angle in steradians.
#' @param R
#'   Numeric vector, range to target in m.
#' @param n1
#'   Numeric vector, volumetric fish density in 1/m^2, same length as \code{R},
#'   typically the result of a call to \code{\link{estN1}}.
#' @details
#'   The number of scatterers per unit volume
#'   is estimated according to Sawada et al. (1993) as
#'   Nv = c * tau * psi * R^2 * n1 / 2.
#' @return
#'   A numeric vector the same length as \code{R} containing the estimated
#'   number of targets in the acoustic beam.
#' @export
#' @references
#' Sawada, K., Furusawa, M., and Williamson, N.J. 1993.
#'   Conditions for the precise measurement of fish target strength in situ.
#'   Fisheries Science (Tokyo) 20:15–21.
#' @seealso
#'   \code{\link{estN1}}
#' @examples
#' mypsi <- 0.008
#' myrange <- c(10, 20, 30)
#' myn1 <- c(0.0001, 0.01, 0.0006)
#' estNv(psi=mypsi, R=myrange, n1=myn1)
#'
estNv <- function(c=1450, tau=0.0004, psi, R, n1) {
  Nv <- c * tau * psi * R^2 * n1 / 2
  return(Nv)
}
