#' Brief Summary of a Data Frame
#'
#' Brief summary of a data frame, number of unique and missing values.
#' @param df
#'   A data frame to be summarized
#' @return
#'   A matrix with a row for each variable (column) in \code{df} and three
#'   columns: \code{no.unique} the number of unique values,
#'   \code{no.entered} the number of rows in \code{df} with non-missing values,
#'   and \code{no.missing} the number of rows in \code{df} with missing values.
#' @export
#' @examples
#' mydat <- data.frame(a=c(1, 1, NA, 2, 2),
#'  b=as.factor(c("cat", "dog", "frog", "", "dog")),
#'  c=c("a", "", "", "a", "b"),
#'  stringsAsFactors=FALSE)
#' dfSmry(mydat)
#'
dfSmry <- function(df) {
  results <- sapply(df, function(x) {
    if(class(x)[1]=="character") {
      c(no.unique=length(unique(x[x!=""])),
        no.entered=sum(x!=""),
        no.missing=sum(x==""))
    } else {
      c(no.unique=length(unique(x[!is.na(x)])),
        no.entered=sum(!is.na(x)),
        no.missing=sum(is.na(x)))
    }
  })
  t(results)
}


