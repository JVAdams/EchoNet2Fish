#' Convert Backscattering Cross Section to Target Strength
#'
#' Convert backscattering cross section (sigma_bs in m^2) to
#' target strength (TS in dB).
#' @param sigmabs
#'   A numeric vector of backscattering cross sections, sigma_bs, in m^2.
#' @details
#'   TS = 10*log10(sigma_bs).
#' @return
#'   A numeric vector of target strengths, TS, in dB, same
#'   length as \code{sigmabs}.
#' @seealso
#'   \code{\link{TS2sigma}}
#' @export
#' @examples
#' sigma2TS(c(1e-03, 1e-04, 1e-05, 1e-06))
#'
  sigma2TS <- function(sigmabs) {
    TS <- 10*log10(sigmabs)
    return(TS)
  }
