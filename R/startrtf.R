#' Create an RTF Document
#'
#' Create an rtf (rich text format) document.
#' @param file
#'   Character scalar name of document, default "RGeneratedDocument" with
#'   \code{\link{Sys.Date}()} suffix.
#' @param dir
#'   Character scalar name of directory where document should be stored,
#'   default \code{\link{getwd}()}.
#' @param width
#'   Numeric scalar width of document page in inches, default 8.5.
#' @param height
#'   Numeric scalar height of document page in inches, default 11.
#' @param omi
#'   Numeric vector, length 4, width of document page margins in inches
#'   (bottom, left, top, right), default c(1, 1, 1, 1).
#' @param quiet
#'   Logical scalar indicating if name of new rtf document should be printed to
#'   command line, default FALSE.
#' @return
#'   An rtf file is created in the specified directory.
#'   An object of class rtf is created.  This object is referred to in
#'   other functions to write to the file.
#'   In addition, two numeric vectors of length 1, \code{tabcount} and
#'   \code{figcount}, are written to the working directory to keep track
#'   of the number of tables and figures written to the rtf document, and
#'   label the captions accordingly.
#' @details
#'   The rtf file may be written to until the \code{\link{endrtf}()} function
#'   is run.
#'   If you assign your rtf file to an object called \code{doc},
#'   you can use the default settings in other \pkg{GLFC}
#'   rtf functions.
#' @references
#'   This is a copy of the \code{startrtf} function from the
#'   \href{https://github.com/JVAdams/GLFC}{[GLFC]} package.
#' @seealso
#'   \code{\link{heading}}, \code{\link{para}}, \code{\link{tabl}},
#'   \code{\link{figu}}, \code{\link{endrtf}},
#'   \code{\link[rtf]{RTF}}.
#' @import
#'  rtf
#' @export
#' @examples
#' \dontrun{
#' # open a Word-friendly rtf file
#' today <- Sys.Date()
#' doc <- startrtf(file=paste("Example", today))
#' # add headings
#' heading("Title")
#' heading(paste("Author", today, sep=" - "), 2)
#' # add a paragraph
#' para("This is how write a paragraph.")
#' # reference a table
#' para("This is how you reference a table (Table ", EchoEnv$tabcount, ").")
#' # add the table
#' tab <- matrix(sample(20), ncol=5,
#'  dimnames=list(paste("Row", 1:4), paste("Column", 1:5)))
#' tabl("A silly table.")
#' # reference a figure
#' para("And this is how you reference a figure (Figure ",
#'  EchoEnv$figcount, ").")
#' # add the figure
#' fig <- function() {
#'   par(mar=c(4, 4, 1, 1))
#'   plot(1:10, 1:10, xlab="X", ylab="Y")
#' }
#' figu("A silly plot.", h=4, w=4)
#' # save the rtf file
#' endrtf()
#' }

startrtf <- function(file=NULL, dir=getwd(), width=8.5, height=11,
    omi=c(1, 1, 1, 1), quiet=FALSE) {
  # create a new RTF file readable by Word
  # create two new variables to keep count of tables and figures
  EchoEnv$tabcount <- 1
  EchoEnv$figcount <- 1
  if (is.null(file)) {
    file <- paste0("RGeneratedDocument", Sys.Date())
  }
  dirfiledoc <- if (length(grep(".doc", file))>0) {
    paste(dir, file, sep="/")
  } else {
    paste(dir, paste0(file, ".doc"), sep="/")
  }
  if (!quiet) {
    cat(paste0("New RTF document created, ", dirfiledoc, "\n"))
  }
  RTF(dirfiledoc, width=width, height=height, omi=omi)
}
