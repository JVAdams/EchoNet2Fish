#' Add a Heading to an RTF Document
#'
#' Add a text heading to an rtf (rich text format) document.
#' @param words
#'   Character scalar text of heading to add to document.
#' @param htype
#'   Integer scalar heading type, 1=bold and font size 12, 2=bold and
#'   font size 10, 3=italics and font size 10, default 1.
#' @param rtf
#'   An rtf object, default \code{doc}.
#' @details
#'   The specified heading is written to the rtf file.
#' @references
#'   This is a copy of the \code{heading} function from the
#'   \href{https://github.com/JVAdams/GLFC}{[GLFC]} package.
#' @seealso
#'   \code{\link{startrtf}} for an example, \code{\link{para}},
#'   \code{\link{tabl}}, \code{\link{figu}},
#'   \code{\link{endrtf}},
#'   \code{\link[rtf]{RTF}}.
#' @import
#'   rtf
#' @export

heading <- function(words, htype=1, rtf=doc) {
  if (htype==1) {
    addHeader(this=rtf, title=words, font.size=12)
  } else {
    startParagraph(this=rtf)
    addText(this=rtf, words, bold=c(TRUE, FALSE)[htype-1],
      italic=c(FALSE, TRUE)[htype-1])
    endParagraph(this=rtf)
    addNewLine(this=rtf)
  }
}
