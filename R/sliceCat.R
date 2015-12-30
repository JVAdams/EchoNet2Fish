#' Categorize Observations as Slices for Matching Acoustic Densities and Trawl
#' Catches
#'
#' Categorize observations as spatial slices for matching fish densities
#' estimated from acoustic transects and species compositions estimated from
#' midwater trawl catches.
#' @param sliceDef
#'   A list of at least two named sub-lists defining the slices into which
#'   observations will be classified.  Each sub-list contains one or more
#'   named numeric vectors of length two, identifying the parameter
#'   (the name of the vector) and the range of values (see Details) that
#'   contribute
#'   to the slice definition, with one exception.  For slicing by regions,
#'   the named vector is character (not numeric), is of length one or more
#'   (not necessarily two), and specifies all of the regions that contribute
#'   to the slice definition.
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
#' @param lon
#'   A numeric vector of longitudes corresponding to the
#'   observations which are to be categorized into slices.  Only necessary if
#'   required by \code{sliceDef}, default NULL.
#' @param lat
#'   A numeric vector of latitudes corresponding to the
#'   observations which are to be categorized into slices.  Only necessary if
#'   required by \code{sliceDef}, default NULL.
#' @param reg
#'   A character vector of regions corresponding to the
#'   observations which are to be categorized into slices.  Only necessary if
#'   required by \code{sliceDef}, default NULL.
#' @return
#'   A character vector the same length as the observations variables
#'   (\code{fdp}, \code{bdp}, \code{lon}, \code{lat}, \code{reg}), 
#'   identifying the slice to which each observation belongs.
#' @details
#' For ranges of values, each interval of sliceDef is closed on the left and
#' open on the right.
#' In other words, if you assign an interval of fdp=c(10, 20), observations
#' >= 10 and < 20 will be considered for inclusion in that slice.
#'
#' All observation variables (\code{fdp}, \code{bdp}, \code{lon}, \code{lat},
#' \code{reg}), if not NULL, must be the same length.
#'
#' @export
#' @examples
#' fishingD <- 1:7
#' bottomD <- c(2, 10, 4, 12, 6, 14, 8)
#' region <- c("a", "b", "c", "a", "b", "c", "a")
#'
#' # slice by fishing depth and bottom depth
#' myslicedef <- list(
#'   epiNear = list( fdp=c(0, 4), bdp=c(0, 6) ),
#'   epiOff = list( fdp=c(0, 4), bdp=c(6, Inf) ),
#'   hypo = list( fdp=c(4, Inf) )
#' )
#' sliceCat(myslicedef, fdp=fishingD, bdp=bottomD)
#'
#' # slice by fishing depth and region
#' myslicedef2 <- list(
#'   epiA = list( fdp=c(0, 4), reg="a" ),
#'   epiBC = list( fdp=c(0, 4), reg=c("b", "c") ),
#'   hypo = list( fdp=c(4, Inf) )
#' )
#' sliceCat(myslicedef2, fdp=fishingD, reg=region)

sliceCat <- function(sliceDef, fdp=NULL, bdp=NULL, lon=NULL, lat=NULL, reg=NULL) {
  # data frame of variables with names for easy reference
  parmat <- as.data.frame(cbind(fdp=fdp, bdp=bdp, lon=lon, lat=lat, reg=reg))
  pr <- match("reg", names(parmat))
  if(!is.na(pr)) {
    parmat <- data.frame(apply(parmat[, -pr, drop=FALSE], 2, as.numeric),
      reg=parmat[, pr])
  }
  if(any(dim(parmat)<1)) stop("No observations to categorize")
  L <- length(sliceDef)
  # matrix of logicals indicating slice membership of observations
  slicecats <- names(sliceDef)
  if(is.null(slicecats)) stop("The elements of sliceDef must be named.")
  selmat <- matrix(TRUE, nrow=dim(parmat)[1], ncol=L,
    dimnames=list(NULL, slicecats))
  possiblepars <- c("fdp", "bdp", "lon", "lat", "reg")
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
      if(pars[j]=="reg") {
        sel <- sel & var %in% rng
      } else {
        sel <- sel & var >= rng[1] & var < rng[2]
      }
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
