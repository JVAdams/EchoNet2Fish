#' Density Per Unit Area
#'
#' Calculate the density of fish in number per unit area
#' from the backscattering cross
#' section, sigma_bs in m^2, and the area backscattering coefficient,
#' s_a (unitless).
#' @param sigmabs
#'   A numeric vector, the backscattering cross section, sigma_bs in m^2.
#' @param ABC
#'   A numeric vector, area backscattering coefficient, s_a (unitless),
#'   the same length as \code{sigmabs}.
#' @param hectare
#'   A logical scalar, indicating if the density should be calculated as the
#'   number per hectare (the default, TRUE), or the number per m^2 (FALSE).
#' @details
#'   For density in number per m^2, p_a = s_a / sigma_bs.
#' @return
#'   A numeric vector of fish densities, p_a in number per unit area,
#'   the same length as \code{sigmabs}.
#' @export
#' @examples
#' paDens(c(0.001, 0.01, 0.1), c(4e-5, 5e-6, 8e-9))
#'
paDens <- function(sigmabs, ABC, hectare=TRUE) {
  pa <- ABC / sigmabs
  if(hectare) {
    pa <- 10000 * pa
  }
  return(pa)
}
