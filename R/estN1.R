#' Estimate the Volumetric Fish Density
#'
#' Estimate the volumetric fish density in number per m^2.
#' @param Sv
#'   Numeric vector, volume backscattering coefficient (unitless).
#' @param sigma
#'   Numeric vector, backscattering cross-section in m^2,
#'   same length as \code{Sv}.
#' @return
#'   A numeric vector the same length as \code{Sv} containing the estimated
#'   volumetric fish density in number per m^2.
#' @export
#' @seealso
#'   \code{\link{estNv}}
#' @examples
#' mySv <- c(-71, -65, -89)
#' mysigma <- c(6e-4, 3e-5, 2e-6)
#' estN1(Sv=mySv, sigma=mysigma)
#'
estN1 <- function(Sv, sigma) {
  n1 <- (10^(Sv/10))/sigma
  return(n1)
}
