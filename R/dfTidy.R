#' Tidy Up a Data Frame
#'
#' Tidy up a data frame with respect to missing values, internal quotes,
#' and white space.
#' @param df
#'   A data frame to be tidied.
#' @param missNums
#'   A numeric vector of numbers representing missing values in \code{df},
#'   default c(-9999, -999.9, -999, 999, 9999).
#' @param missChars
#'   A character vector of strings representing missing values in \code{df},
#'   default c("NA", "NULL", ".", " ", "  ").
#' @importFrom simsalapar tryCatch.W.E
#' @return
#'   A tidied version of the original data frame.
#' @export
#' @details
#'   Missing values in \code{df} are replaced with NA for numeric vectors
#'   and "" for character vectors.
#'   Internal quotes and leading and trailing white space
#'   in character vectors are removed.
#'   Attempt is made to convert any character vectors of numbers to numeric.
#' @examples
#' mydat <- data.frame(a=c(1, 2, -999, 4, 5),
#'  b=as.factor(c("cat ", " dog", "frog", ".", " ant")),
#'  c=c("1.2", "NULL", "3.4", "5.6", "7.8"),
#'  stringsAsFactors=FALSE)
#' dfTidy(mydat)
#'
dfTidy <- function(df, missNums=c(-9999, -999.9, -999, 999, 9999),
  missChars=c("NA", "NULL", ".")) {
  # convert factors to character
  fac.colz <- sapply(df, class) %in% "factor"
	df[, fac.colz] <- lapply(df[, fac.colz, drop=FALSE], as.character)
	# fix numerics
	number.colz <- sapply(df, class) %in% c("integer", "numeric")
	df[, number.colz] <- lapply(df[, number.colz, drop=FALSE],
	  function(x) {
  		x[x %in% missNums] <- NA
  		x
  	})
	# fix characters
	char.colz <- sapply(df, class) %in% c("character")
	df[, char.colz] <- lapply(df[, char.colz, drop=FALSE],
	  function(x) {
  	  # remove internal quotes
  	  x <- gsub("\"", "", x)
  	  # remove leading and trailing white space
    	x <- gsub("^[ \t]+|[ \t]+$", "", x)
  		x[x %in% missChars] <- ""
  		xwe <- simsalapar::tryCatch.W.E(as.numeric(x))
  		if(is.null(xwe$warning)) x <- xwe$value
  		x
  	})
	df
}
