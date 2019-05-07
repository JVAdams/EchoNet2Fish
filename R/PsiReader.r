#' Get transducer equivalent beam angle (EBA, dB) for all transducers
#' in the input Sv data and convert to psi 10^(EBA/10) for use in estNv
#' This implementation requires the user to respond to prompts, with
#' the number of prompts = length of list of unique transducers.
#' @param sv
#'   Data frame of Sv data that is the result of readAll().
#' @param EVfolder
#'   Vector of base folder names in which the EV files used to generate the
#'   data are location. Length is the same as the number of rows in \code{sv}.
#' @param EBA
#'   Numeric scalar of the equivalent beam angle. Must be negative and in dB.
#'   Unknown what will happen if otherwise.
#' @details
#'   If \code{EBA} is positive the calculated psi will be incorrect.
#' @return
#'   A data frame with three columns (dat.source, psi, and EBA) and as many rows as
#'   the length of the number of unique transducers in the Sv data.
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
    while(x<=length(ev.psi.lab)) {
      df <- data.frame(dat.source = NA, psi = NA)
      dat.source <- unique.transducer[x]
      df$dat.source <- dat.source
      df$EBA <- as.numeric(readline(prompt=paste0("Enter Equivalent beam angle in dB for ", unique.transducer[x], ":")))
      df$psi <- 10^(df$EBA/10)
      psi.df <- rbind(psi.df, df)
      x <- x+1
    }
  return(psi.df)
  }

psi.read <- ReadPsi()



