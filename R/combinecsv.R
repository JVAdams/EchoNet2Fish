#' Combine Several Comma Delimited Files into a Single Data Frame
#'
#' Combine all csv files in a given directory into a single data frame.
#' @param myDir
#'   A character scalar naming the directory in which the csv files
#'   are stored.  Should end in a forward slash, e.g., "C:/temp/".
#'   All the csv files should have the same number of columns with the same
#'   header row of column names.
#' @param addSource
#'   A logical scalar indicating whether a new column, named "source", should
#'   be added to the data frame identifying the source file, default TRUE.
#' @param column1name
#'   A character scalar assigning a name to the first column in the data
#'   frame (writing over whatever name is there already), default "Dummy_ID".
#' @return
#'   A data frame with the information from all the csv files combined.
#' @details
#' 	 The \code{column1name} argument is needed to handle occasional problems
#' 	 with byte order marks at the beginning of the csv files, which can result
#' 	 in strange characters being added to the name of the first column.
#' 	 See, for example, this
#' 	 \href{http://stackoverflow.com/a/15399003/2140956}{link}.
#'
#'   Any variable names starting with "X." and ending with a number
#'   are changed to ensure the number part of the name is rounded to the
#'   nearest whole number, e.g., "X.76.000000" is renamed "X.76".
#' @importFrom plyr rbind.fill
#' @import utils
#' @export

combinecsv <- function(myDir, addSource=TRUE, column1name="Dummy_ID") {
	# combine all csv files in a given directory into a single data frame
	# file names
	filenames <- list.files(myDir)[grep(".csv$", list.files(myDir))]
	nfiles <- length(filenames)
	# create an empty list where all the files will be stored
	files.list <- vector(mode="list", length=nfiles)
	for(i in 1:nfiles) {
		# read the data into a temporary file
		temp <- read.csv(paste(myDir, filenames[i], sep=""), as.is=TRUE)
		# combat potential problems with byte order marks
		# http://stackoverflow.com/a/15399003/2140956
		names(temp)[1] <- column1name
		# add a new column identifying the source file
		if(addSource) temp$source <- filenames[i]
    # make sure numbered names are rounded to whole numbers
    namez <- names(temp)
    sel <- substring(namez, 1, 2)=="X."
    names(temp)[sel] <-
      paste0("X.", round(as.numeric(substring(namez[sel], 3, 5))))
    # eliminate columns named "X"
    xcols <- names(temp)=="X"
    temp <- temp[, !xcols]
		# put the data into the list
		files.list[[i]] <- temp
	}
	# if more than n, paste first n with ellipses
	shorten <- function(x, n=5) {
	  if(length(x)>n) {
	    y <- paste(c(x[1:n], "..."), collapse=", ")
	  } else {
	    y <- paste(x, collapse=", ")
	  }
	  y
	}
	if(nfiles > 1) {
  	# check for name consistency
  	errors <- 0
  	for(i in 2:nfiles) {
  	  x1 <- names(files.list[[i-1]])
  	  x2 <- names(files.list[[i]])
  	  y1 <- setdiff(x1, x2)
  	  y2 <- setdiff(x2, x1)
  	  if(length(y1) + length(y2) > 0) {
  	    errors <- errors+1
  	    cat("\nFor each pair of files,",
          " these are column names present in one and not the other:\n",
  	      "  ", filenames[i-1], " has\n    ", shorten(y1), "\n",
          "  ", filenames[i], " has\n    ", shorten(y2), "\n", sep="")
  	  }
  	}
  	if(errors>0) warning("Column names mismatch")
  	# combined each of the files from the list into one single file
  	out <- do.call(plyr::rbind.fill, files.list)
	} else {
	  out <- files.list[[1]]
	}
	return(out)
}
