#' Convert Target Strength to Backscattering Cross Section
#'
#' Convert target strength (TS in dB) to backscattering cross section
#' (sigma_bs in m^2), <sigma_bs>.
#' @param TS
#'   A numeric vector of target strengths, TS, in dB.
#' @details
#'   sigma_bs = 10^(TS/10).
#' @return
#'   A numeric vector of backscattering cross sections, sigma_bs, in m^2, same
#'   length as \code{TS}.
#' @seealso
#'   \code{\link{sigma2TS}}
#' @export
#' @examples
#' TS2sigma(c(-30, -40, -50, -60))
#'
  TS2sigma <- function(TS) {
    sigmabs <- 10^(TS/10)
    return(sigmabs)
  }
