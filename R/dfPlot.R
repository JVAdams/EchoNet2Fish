#' Plot All Variables of a Data Frame
#'
#' Plot all the variables of a data frame.
#' @param df
#'   A data frame to be plotted
#' @param mcex
#'   A numeric scalar giving the amount by which plot titles
#'   will be magnified, default 1.2.
#' @param cex
#'   A numeric scalar giving the amount by which plotting text and symbols
#'   will be magnified, default 0.8.
#' @param ...
#'   Additional arguments to the \code{\link{par}} function.
#' @import graphics
#' @export
#' @details
#'   An individual graph is generated for each variable.  Bar plots are drawn
#'   for factors, characters with up to 50 unique values, and numerics
#'   with up to 10 unique values.  Otherwise finite values are plotted versus
#'   row number in \code{df}.  An empty plot is drawn for numerics with
#'   fewer than 2 finite values and characters with more than 50 unique values.
#' @examples
#' mydat <- data.frame(a=c(1:5, 10:15, 6:9),
#'  b=as.factor(rep(c("cat", "dog", "frog", "cat", "dog"), 3)),
#'  c=rep(c("a", "b", "a", "a", "b"), 3),
#'  stringsAsFactors=FALSE)
#' dfPlot(mydat)
#'
dfPlot <- function(df, mcex=1.2, cex=0.8, ...) {
  par(cex=cex, ...)
  # plot the columns of a data frame
  for(i in 1:dim(df)[2]) {
    x <- df[[i]]
    if(sum(!is.na(x)) > 0) {
      name <- names(df)[i]
      if(is.factor(x)) {
        plot(x, main=name, cex.main=mcex)
      } else {
        if(is.character(x) & length(unique(x)) <= 50) {
          plot(as.factor(x), main=name, cex.main=mcex)
        } else {
          if(is.numeric(x) & sum(is.finite(x)) < 2) {
            plot(1, 1, type="n", xlab="", ylab="", axes=F,
              main=paste0(name, ": numeric"), cex.main=mcex)
          } else {
            if(is.numeric(x) & length(unique(x)) <= 10) {
              plot(as.factor(x), main=name, cex.main=mcex)
            } else {
              if(is.numeric(x)) {
                plot.default(x[is.finite(x)], xlab="Row", ylab="",
                  main=name, cex.main=mcex)
              } else {
                if(is.character(x)) {
                  plot(1, 1, type="n", xlab="", ylab="", axes=F,
                    main=paste0(name, ": character"), cex.main=mcex)
                }
              }
            }
          }
        }
      }
    }
  }
  invisible()
}
