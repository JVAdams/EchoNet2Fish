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
#' @details
#'   The acoustic and midwater trawl data corresponding to the selected row
#'   of the reference csv file are read in and saved as objects in the
#'   specified RData file, in \code{subdir}.
#' @export
#'
readAll <- function(refdir, keyvals, keyvars=c("LAKE", "YEAR"), rdat="ACMT",
  refcsv="Reference") {
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

  df <- ref[selrow, ]

  maindir <-   paste0(refdir, "/", df$subdir, "/")
  svdir <-     paste0(maindir, df$svsubdir, "/")
  tsdir <-     paste0(maindir, df$tssubdir, "/")
  optrop.f <-  paste0(maindir, df$optropf, ".csv")
  trcatch.f <- paste0(maindir, df$trcatchf, ".csv")
  trlf.f <-    paste0(maindir, df$trlff, ".csv")
  if(!is.na(df$keysp1)) {
    keysp1.f <- list(sp=df$keysp1, file=paste0(maindir, df$keyfile1, ".csv"))
  } else {
    keysp1.f <- NULL
  }
  if(!is.na(df$keysp2)) {
    keysp2.f <- list(sp=df$keysp2, file=paste0(maindir, df$keyfile2, ".csv"))
  } else {
    keysp2.f <- NULL
  }

  inputs <- df
  rm(ref, i, sel, selrow, df)

  # read in AC data
  sv <- readSVTS(svdir, oldname="Good_samples", newname="Samples",
    elimMiss=c("Lat_M", "Sv_mean", "PRC_ABC"))
  ts <- readSVTS(tsdir, datevars=NULL)

  # read in MT data
  # RVCAT data
  optrop <- read.csv(optrop.f, as.is=TRUE, na.strings="NULL")
  trcatch <- read.csv(trcatch.f, as.is=TRUE, na.strings="NULL")
  trlf <- read.csv(trlf.f, as.is=TRUE, na.strings="NULL")
  if(!is.null(keysp1.f)) {
    key1 <- read.csv(keysp1.f$file, as.is=TRUE)
    key1$sp <- keysp1.f$sp
  } else {
    rm(keysp1.f)
  }
  if(!is.null(keysp2.f)) {
    key2 <- read.csv(keysp2.f$file, as.is=TRUE)
    key2$sp <- keysp2.f$sp
  } else {
    rm(keysp2.f)
  }
  rm(svdir, tsdir, optrop.f, trcatch.f, trlf.f)
  save(list=ls(all.names=TRUE), file=paste0(maindir, rdat, ".RData"))
  return(maindir)
}
