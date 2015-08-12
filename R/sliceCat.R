#' Categorize Observations as Slices for Matching Acoustic Densities and Trawl
#' Catches
#'
#' Categorize observations as spatial slices for matching fish densities
#' estimated from acoustic transects and speciec compositions estimated from
#' midwater trawl catches.
#' @param sliceDef
#'   A list of at least two named sub-lists defining the slices into which
#'   observations will be classified.  Each sub-list contains one or more
#'   named numeric vectors of length two, identifying the parameter
#'   (the name of the vector) and the range of values that contribute
#'   to the slice definition.
#'   Each interval is closed on the left and open on the right (see Details).
#'   The name of each sub-list is the name of the slice to be assigned.
#'   See Examples.
#' @param fdp
#'   A numeric vector of fishing depths (the distance from the surface of the
#'   water to the depth of a fish in the water) corresponding to the
#'   observations which are to be categorized into slices.  Only necessary if
#'   required by \code{sliceDef}, default NULL.
#'   default
#' @param bdp
#'   A numeric vector of bottom depths (the distance from the surface of the
#'   water to the substrate) corresponding to the
#'   observations which are to be categorized into slices.  Only necessary if
#'   required by \code{sliceDef}, default NULL.
#' @param lat
#'   A numeric vector of latitudes corresponding to the
#'   observations which are to be categorized into slices.  Only necessary if
#'   required by \code{sliceDef}, default NULL.
#' @return
#'   A character vector the same length as the observations variables
#'   (\code{fdp}, \code{bdp}, \code{lat}), identifying the slice to which
#'   each observation belongs.
#' @details
#' Each interval of sliceDef is closed on the left and open on the right.
#' In other words, if you assign an interval of fdp=c(10, 20), observations
#' >= 10 and < 20 will be considered for inclusion in that slice.
#'
#' All observation variables (\code{fdp}, \code{bdp}, \code{lat}), if not NULL,
#'  must be the same length.
#' @export
#' @examples
#' myslicedef <- list(
#'   epiNear = list( fdp=c(0,   4), bdp=c(0,   6) ),
#'   epiOff =  list( fdp=c(0,   4), bdp=c(6, Inf) ),
#'   hypo =    list( fdp=c(4, Inf) )
#' )
#' fishingD <- 1:7
#' bottomD <- c(2, 10, 4, 12, 6, 14, 8)
#' slice <- sliceCat(myslicedef, fdp=fishingD, bdp=bottomD)
#' data.frame(fishingD, bottomD, slice)

sliceCat <- function(sliceDef, fdp=NULL, bdp=NULL, lat=NULL) {
  # matrix of variables with names for easy reference
  parmat <- cbind(fdp=fdp, bdp=bdp, lat=lat)
  if(any(dim(parmat)<1)) stop("No observations to categorize")
  L <- length(sliceDef)
  # matrix of logicals indicating slice membership of observations
  slicecats <- names(sliceDef)
  if(is.null(slicecats)) stop("The elements of sliceDef must be named.")
  selmat <- matrix(TRUE, nrow=dim(parmat)[1], ncol=L,
    dimnames=list(NULL, slicecats))
  possiblepars <- c("fdp", "bdp", "lat")
  for(i in 1:L) {
    L2 <- length(sliceDef[[i]])
    pars <- names(sliceDef[[i]])
    if(any(!(pars %in% colnames(parmat))))
      stop(paste("sliceDef requires observations for",
        paste(pars, collapse=", ")))
    if(is.null(pars) | any(!(pars %in% possiblepars)))
      stop(paste("Elements of sliceDef sub-lists must be named either",
        paste(possiblepars, collapse=", ")))
    sel <- selmat[, i]
    for(j in 1:L2) {
      rng <- sliceDef[[i]][[j]]
      var <- parmat[, pars[j]]
      sel <- sel & var >= rng[1] & var < rng[2]
    }
    selmat[, i] <- sel
  }
  memberofxslices <- apply(selmat, 1, sum)
  sel0 <- memberofxslices<1
  if(sum(sel0)>0) {
    warning("At least one observation is not contained in any slice")
    print(cbind(selmat, parmat)[sel0, ])
  }
  sel2 <- memberofxslices>1
  if(sum(sel2)>0) {
    warning("At least one observation is contained in more than one slice")
    print(cbind(selmat, parmat)[sel2, ])
  }
  numobsinslice <- apply(selmat, 2, sum)
  selmiss <- numobsinslice < 1
  if(sum(selmiss)>0) {
    warning(paste("No observations in slice",
      paste(slicecats[selmiss], collapse=", ")))
  }
  slice <- slicecats[apply(selmat, 1, function(row) match(TRUE, row))]
  return(slice)
}
