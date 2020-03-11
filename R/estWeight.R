#' Estimate Fish WEIGHT From LENGTH
#'
#' Estimate fish weight from fish length.
#' @param Lmm
#'   A numeric vector, the total length of fish in mm.
#' @param LWa
#'   A numeric vector of the multiplicative parameter in a length-weight
#'   relation, in log(g).  See details.
#' @param LWb
#'   A numeric vector of the exponential parameter in a length-weight
#'   relation, in 1/log(mm).  See details.
#' @details
#'   The weight (in g) is calculated from the length-weight relation as
#'   Wg = \code{LWa} * \code{Lmm} ^ \code{LWb}.
#' @return
#'   A numeric vector of fish weights (in g), the same length as \code{Lmm}.
#' @export
#' @examples
#' paDens(c(0.001, 0.01, 0.1), c(4e-5, 5e-6, 8e-9))
#'
estWEIGHT <- function(Lmm, LWa, LWb) {
  Wg <- LWa * Lmm ^ LWb
  return(Wg)
}
