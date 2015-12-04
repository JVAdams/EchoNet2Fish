#' Estimate the Volumetric Fish Density
#'
#' Estimate the volumetric fish density in number per m^3.
#' @param Sv
#'   Numeric vector, volume backscattering strength in dB.
#' @param sigmabs
#'   Numeric vector, backscattering cross-section in m^2,
#'   same length as \code{Sv}.
#' @return
#'   A numeric vector the same length as \code{Sv} containing the estimated
#'   volumetric fish density in number per m^3.
#' @export
#' @details
#'   The volumetric fish density (in #/m^3)
#'   is estimated as
#'   rhov = sv/\code{sigmabs},
#'   where the volume backscattering coefficient (in 1/m) is
#'   sv = 10^(\code{Sv}/10).
#'   (Note distinction between upper and lower case S in \code{Sv} and sv.)
#' @seealso
#'   \code{\link{estNv}}
#' @examples
#' mySv <- c(-71, -65, -89)
#' mysigma <- c(6e-4, 3e-5, 2e-6)
#' estrhov(Sv=mySv, sigmabs=mysigma)
#'
estrhov <- function(Sv, sigmabs) {
  # volume backscatter coefficient (in 1/m)
  sv <- 10^(Sv/10)
  rhov <- sv/sigmabs
  return(rhov)
}
