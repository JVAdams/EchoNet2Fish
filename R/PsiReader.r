
#' PsiReader
#'
#' Read EBA (equivalent beam angle, dB) provided by the user and convert
#' to psi.
#'
#' Sv data set resulting from readAll().
#' @return
#' Creates a data frame with as many rows as unique transducers and the columns
#' 'EV file folder name - frequency kHz', psi, and EBA.
#' @export
#'
#' @import svDialogs


  ReadPsi <- function() {
    x <- 1
    psi.df = data.frame()
    EV_filename_parts <- strsplit(sv$EV_filename, "[\\]")
    parts_length <- sum(count(unlist(EV_filename_parts))$freq)
    sv$EVfolder <- sapply(strsplit(EV_filename_parts[[1]][parts_length], "[/]"),  "[", 1)
    ev.source.freq <- unique(sv[c("EVfolder", "Frequency")])
    unique.transducer <- paste0(ev.source.freq$EVfolder, " - ", ev.source.freq$Frequency, " kHz" )
    while(x<=length(unique.transducer)) {
      df <- data.frame(dat.source = NA, psi = NA)
      dat.source <- unique.transducer[x]
      df$dat.source <- dat.source
      df$EBA <- as.numeric(dlg_input(GUI = EBA, paste0("Enter Equivalent beam angle in dB for ", unique.transducer[x], ":"))$res)
      df$psi <- 10^(df$EBA/10)
      psi.df <- rbind(psi.df, df)
      x <- x+1
    }
  return(psi.df)
  }




