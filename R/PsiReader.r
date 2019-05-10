
#' PsiReader
#'
#' Read EBA (equivalent beam angle, dB) provided by the user and convert
#' to psi.
#'
#' @param sv
#' Sv data set resulting from readAll().
#' @return
#' Creates a data frame with as many rows as unique transducers and the columns
#' 'EV file folder name - frequency kHz', psi, and EBA.
#' @export
#'

  ReadPsi <- function(sv) {
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




