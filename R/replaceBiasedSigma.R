#' Replace Biased Backscattering Cross Sections
#'
#' Replace biased backscattering cross sections (sigma_bs in m^2)
#' with averages from unbiased sigma_bs using cells in the same
#' layer and (if possible) transect.
#' @param df
#'   A data frame containing the variables specified by name below.
#' @param varNv
#'   A character scalar, the name of the variable in \code{df} containing
#'   a numeric vector of the number of fish per acoustic sampling volume.
#' @param varsigmabs
#'   A character scalar, the name of the variable in \code{df} containing
#'   a numeric vector of the backscattering cross section, sigma_bs in m^2.
#' @param varTranLay
#'   A character vector, the names of the variable in \code{df} used to identify
#'   unique transect-layers.  Include names of other needed by variables
#'   if \code{df} contains information from multiple waterbodies or time
#'   periods.
#' @param varLay
#'   A character vector, the names of the variable in \code{df} used to identify
#'   unique layers.  The default, \code{varTranLay}[-1], assumes that the first
#'   variable names in \code{varTranLay} identifies the transect.
#'   This must be a subset of \code{varTranLay}.  Include names of other needed
#'   by variables if \code{df} contains information from multiple waterbodies
#'   or time periods.
#' @param Nvcut
#'   A numeric scalar, the cutoff for Nv (specified by \code{VarNv}), above
#'   which all sigma_bs are considered biased, default 0.1.  Any sigma_bs with
#'   missing values are left unchanged.
#' @details
#'   TS = 10*log10(sigma_bs).
#' @return
#'   A numeric vector of target strengths, TS, in dB, same
#'   length as \code{sigmabs}.
#' @export
#' @examples
#' mydf <- data.frame(nv=c(0.01, 0.1, 0.2, 0.2, 0.3, 0.05, 0.01),
#'   sig=1:7, tran=c(1, 1, 1, 2, 2, 1, 1), layer=c(1, 1, 1, 2, 2, 2, 2))
#' sig <- replaceBiasedSigma(df=mydf, varNv="nv", varsigmabs="sig",
#'   varTranLay=c("tran", "layer"))
#' cbind(mydf, sig)
#'
replaceBiasedSigma <- function(df, varNv, varsigmabs, varTranLay,
  varLay=varTranLay[-1], Nvcut=0.1) {

  df$origord <- 1:dim(df)[1]

  dfunbiased <- df[df[, varNv] <= Nvcut & !is.na(df[, varNv]), ]
  formtl <- formula(paste(varsigmabs, "~", paste(varTranLay, collapse=" + ")))
  tranlay <- aggregate(formtl, mean, data=dfunbiased)
  names(tranlay)[names(tranlay)==varsigmabs] <- "sigunb.tranlay"

  forml <- formula(paste(varsigmabs, "~", paste(varLay, collapse=" + ")))
  lay <- aggregate(forml, mean, data=dfunbiased)
  names(lay)[names(lay)==varsigmabs] <- "sigunb.lay"

  df2 <- merge(df, tranlay, by=varTranLay, all=TRUE)
  df3 <- merge(df2, lay, by=varLay, all=TRUE)
  df3 <- df3[order(df3$origord), ]

  sig <- df3[, varsigmabs]

  # replace biased sigma with transect-layer mean of unbiased sigma
  biasRmiss <- df3[, varNv] > Nvcut & !is.na(df3[, varNv])
  sig[biasRmiss] <- df3$sigunb.tranlay[biasRmiss]
  # if no transect-layer mean, use layer mean
  notranlay <- is.na(df3$sigunb.tranlay)
  sig[biasRmiss & notranlay] <- df3$sigunb.lay[biasRmiss & notranlay]

  return(sig)
}
