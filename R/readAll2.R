#' Read in All the Data Corresponding to a Specified Reference Row
#'
#' Read in all the acoustic and midwater trawl data corresponding to the
#' specified row or a reference csv file.  Save the data as objects in
#' an RData file.
#' @param refdir
#'   A character scalar giving the directory in which the reference csv file,
#'   \code{ref}, is located.
#' @param keyvals
#'   A vector giving the values of \code{keyvars} to identify a single row of
#'   the reference csv file, \code{ref}.
#' @param keyvars
#'   A character vector giving the names of columns in the reference csv file,
#'   \code{ref}, that will be used to identify a single row, default
#'   c("LAKE", "YEAR").
#' @param rdat
#'   A character scalar giving the name of the RData file that will be saved
#'   containing all of the objects corresponding to the selected row of
#'   the reference csv file, \code{ref}, default "ACMT".
#' @param AC
#'   A logical scalar indicating if you want to read in the acoustic data,
#'   default TRUE.
#' @param MT
#'   A logical scalar indicating if you want to read in the midwater trawl
#'   data, default TRUE.
#' @param refcsv
#'   A character scalar giving the name of the reference csv file,
#'   \code{ref}, default "Reference".  The csv file must have the following
#'   10 columns:
#'   \itemize{
#'     \item \code{subdir} = a subdirectory of \code{refdir} containing
#'     all the other subdirectories and files,
#'     \item \code{svsubdir} = the Sv subdirectory,
#'     \item \code{tssubdir} = the TS subdirectory,
#'     \item \code{optropf} = the midwater trawl operations file,
#'     \item \code{trcatchf} = the midwater trawl catch file,
#'     \item \code{trlff} = the midwater trawl lengths file,
#'     \item \code{keysp1} = the species code for \code{keyfile1},
#'     \item \code{keyfile1} = the age-length csv file for species\code{keysp1},
#'     \item \code{keysp2} = the species code for \code{keyfile2},
#'     \item \code{keyfile2} = the age-length csv file for species\code{keysp2}.
#'   }
#'   There should also be additional columns for \code{keyvars}.
#' @return
#'   A character scalar giving the path of the subdirectory
#'   containing all of inputs and outputs for the given \code{keyvars} and
#'   \code{keyvals}.
#' @details
#'   The acoustic and midwater trawl data corresponding to the selected row
#'   of the reference csv file are read in and saved as objects in the
#'   specified RData file, in \code{subdir}.  Objects include
#'   the scalar
#'     \code{rdat};
#'   the vectors
#'     \code{keyvals} and \code{keyvars};
#'   and the data frames
#'     \code{inputs} (the selected row from the reference data);
#'     \code{sv} and \code{ts} (acoustic data);
#'     \code{optrop}, \code{trcatch}, and \code{trlf} (midwater trawl data);
#'     \code{key1} and \code{key2} (age-length keys, if specified).
#' @import utils
#' @export
#'
readAll2 <- function(refdir, keyvals, keyvars=c("LAKE", "YEAR"), rdat="ACMT",
  AC=TRUE, MT=TRUE, refcsv="Reference") {
  # read in file that has subdirectory and file names for each lake-year
  ref <- read.csv(paste0(refdir, "/", refcsv, ".csv"))
  sel <- array(NA, dim=c(dim(ref)[1], length(keyvars)))
  for(i in seq_along(keyvars)) {
    sel[, i] <- ref[, keyvars[i]]==keyvals[i]
  }
  selrow <- apply(sel, 1, all)
  if(sum(selrow)!=1) {
    stop("Need one row in reference file for the specified LAKE and YEAR.")
  }

  inputs <- ref[selrow, ]

  maindir <-   paste0(refdir, "/", inputs$subdir, "/")

  svdir <-     paste0(maindir, inputs$svsubdir, "/")
  tsdir <-     paste0(maindir, inputs$tssubdir, "/")

  optrop.f <-  paste0(maindir, inputs$optropf, ".csv")
  trcatch.f <- paste0(maindir, inputs$trcatchf, ".csv")
  trlf.f <-    paste0(maindir, inputs$trlff, ".csv")
  if(!is.na(inputs$keysp1)) {
    keysp1.f <- list(sp=inputs$keysp1, file=paste0(maindir, inputs$keyfile1, ".csv"))
  } else {
    keysp1.f <- NULL
  }
  if(!is.na(inputs$keysp2)) {
    keysp2.f <- list(sp=inputs$keysp2, file=paste0(maindir, inputs$keyfile2, ".csv"))
  } else {
    keysp2.f <- NULL
  }

  # list of objects to save
  keepobjs <- c("rdat", "keyvals", "keyvars", "inputs")
  if(AC) keepobjs <- c(keepobjs, "sv", "ts")
  if(MT) keepobjs <- c(keepobjs, "optrop", "trcatch", "trlf")

  if(AC) {
    # read in AC data
    sv <- readSVTS(svtsdir=svdir, oldname="ABC", newname="PRC_ABC",
      elimMiss=c("Lat_M", "Sv_mean"),
      datevars="Date_M", addyear=TRUE, tidy=TRUE)

    if(!("PRC_ABC" %in% names(sv))) {
      if(!("Thickness_mean" %in% names(sv))) {
        print(names(sv))
        stop("The sv file MUST include either the area backscattering coefficient, named 'ABC' or 'PRC_ABC', or the mean thickness of the analyzed domain, named 'Thickness_mean'.")
      } else {
        sv$PRC_ABC = 10^(sv$Sv_mean/10)*sv$Thickness_mean
        sv <- sv[!is.na(sv$PRC_ABC), ]
      }
    }
    ts <- readSVTS(tsdir, datevars=NULL)

    # add Region_Name to SV and TS files
    if(!("Region_name" %in% names(sv))) sv$Region_name <-
      paste0("RR", as.numeric(as.factor(sv$source)))
    if(!("Region_name" %in% names(ts))) ts$Region_name <-
      paste0("RR", as.numeric(as.factor(ts$source)))
  }

  if(MT) {
    # read in MT data
    # RVCAT data
    optrop <- read.csv(optrop.f, as.is=TRUE, na.strings="NULL")
    trcatch <- read.csv(trcatch.f, as.is=TRUE, na.strings="NULL")
    trlf <- read.csv(trlf.f, as.is=TRUE, na.strings="NULL")

    jvanames <- function(charvec) {
      make.names(casefold(charvec), unique = TRUE, allow_ = FALSE)
    }

    fixnames <- function(df, needed) {
      now <- names(df)
      have <- jvanames(now)
      need <- jvanames(needed)
      miss <- needed[setdiff(need, have)]
      if(length(miss)>0) stop("The ", deparse(substitute(x)),
        " csv file is missing at least one needed column: ",
        paste(miss, collapse=", "))
      indx <- match(need, have)
      now[indx] <- needed
      return(now)
    }

    # needed names for optrop, trcatch, and trlf files
    neednames <- list(
      c("OP_ID", "YEAR", "LAKE", "BEG_DEPTH", "END_DEPTH", "FISHING_DEPTH",
        "Transect", "Latitude", "Longitude"),
      c("OP_ID", "SPECIES", "WEIGHT", "N"),
      c("OP_ID", "SPECIES", "LENGTH", "N"))

    names(optrop) <- fixnames(optrop, neednames[[1]])
    names(trcatch) <- fixnames(trcatch, neednames[[2]])
    names(trlf) <- fixnames(trlf, neednames[[3]])

    if(!is.null(keysp1.f)) {
      key1 <- read.csv(keysp1.f$file, as.is=TRUE)
      key1$sp <- keysp1.f$sp
      keepobjs <- c(keepobjs, "key1")
    }
    if(!is.null(keysp2.f)) {
      key2 <- read.csv(keysp2.f$file, as.is=TRUE)
      key2$sp <- keysp2.f$sp
      keepobjs <- c(keepobjs, "key2")
    }
  }

  save(list=keepobjs, file=paste0(maindir, rdat, ".RData"))
  return(maindir)
}
