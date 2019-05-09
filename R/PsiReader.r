#' @title {Obtain EBA for all transducers}
#'
#'
#' @description {
#' \code{PsiReader}() prompts the user for as many equivalent beam (EBA)
#' angles as there are unique transducers in the Sv data. EBA is then
#' converted to Psi (Psi = 10^(EBA/10)).}
#' @param EVfolder
#'   Vector of base folder names in which the EV files used to generate the
#'   data are location. Length is the same as the number of rows in \code{sv}.
#' @param EBA
#'   Numeric scalar of the equivalent beam angle. Must be negative and in dB.
#'   Unknown what will happen if otherwise.
#' @details
#'   This function is primarily intended for use by estimateLake() to enable use
#'   of transducer specific Psi rather than one value assumed to be adequate for
#'   all transducers.
#' @return
#'   A data frame with three columns (dat.source, psi, and EBA) and as many rows as
#'   the number of unique transducers in the Sv data.
#' @export
#' @seealso
#'   \code{\link{EstNv}}
#' @references
#'   Based on a function posted by EuGENE on 02 February 2018 on stackoverflow
#'   \href{https://stackoverflow.com/questions/48588180/in-r-how-to-read-multiple-integers-from-user-prompt/48588287#48588287}{[link]}.


  ReadPsi <- function() {
    x <- 1
    psi.df = data.frame()
    sv$EVfolder <- sapply(strsplit(sapply(strsplit(sv$EV_filename, "[\\]"),  "[", 6), "[/]"), "[",1)
    ev.source.freq <- unique(sv[c("EVfolder", "Frequency")])
    unique.transducer <- paste0(ev.source.freq$EVfolder, " - ", ev.source.freq$Frequency, " kHz" )
    while(x<=length(unique.transducer)) {
      df <- data.frame(dat.source = NA, psi = NA)
      dat.source <- unique.transducer[x]
      df$dat.source <- dat.source
      df$EBA <- as.numeric(svDialogs::dlg_input(GUI = EBA, paste0("Enter Equivalent beam angle in dB for ", unique.transducer[x], ":"))$res)
      df$psi <- 10^(df$EBA/10)
      psi.df <- rbind(psi.df, df)
      x <- x+1
    }
  return(psi.df)
  }




