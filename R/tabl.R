#' Add a Table to an RTF Document
#'
#' Add a table to an rtf (rich text format) document.
#' @param ...
#'   One or more character scalars (separated by commas) of text to use for
#'   the table caption.
#' @param TAB
#'   A matrix, data frame, or table to be added to the document as a table,
#'   default \code{tab}.
#' @param rtf
#'   An rtf object, default \code{doc}.
#' @param fontt
#'   Numeric scalar font size for table caption, default 8.
#' @param row.names
#'   Logical scalar whether to include the row.names of \code{TAB} in the table,
#'   default TRUE.
#' @param tabc
#'   Numeric scalar table number to use in caption, default
#'   \code{EchoEnv$tabcount}.
#' @param boldt
#'   Logical scalar indicating if table number should use bold font,
#'   default TRUE.
#' @param newpage
#'   Character scalar indicating if the table should start on a new page in
#'   the document "port" for a new portrait page,
#'   "land" for a new landscape page, and "none" for no new page (the default).
#' @param omi
#'   Numeric vector, length 4, width of document page margins in inches
#'   (bottom, left, top, right), default c(1, 1, 1, 1).
#' @return
#'   A 1 is added to the numeric vector of length 1, \code{EchoEnv$tabcount},
#'   stored in the working directory to keep track of the number of tables
#'   written to the rtf document, and label the captions accordingly.
#' @details
#'   The table and caption are written to the rtf file.
#'   The size of a new page is assumed to be 8.5 by 11 inches.
#' @references
#'   This is a copy of the \code{tabl} function from the
#'   \href{https://github.com/JVAdams/GLFC}{[GLFC]} package.
#' @seealso
#'   \code{\link{startrtf}} for an example, \code{\link{heading}},
#'   \code{\link{para}}, \code{\link{figu}},
#'   \code{\link{endrtf}},
#'   \code{\link[rtf]{RTF}}.
#' @import
#'   rtf
#' @export

tabl <- function(..., TAB=tab, rtf=doc, fontt=8, row.names=TRUE,
    tabc=EchoEnv$tabcount, boldt=TRUE, newpage="none", omi=c(1, 1, 1, 1)) {
  if (newpage=="port") {
    addPageBreak(this=rtf, width=8.5, height=11, omi=omi)
  }
  if (newpage=="land") {
    addPageBreak(this=rtf, width=11, height=8.5, omi=omi)
  }
  startParagraph(this=rtf)
  addText(this=rtf, paste0("Table ", tabc, ".  "), bold=boldt)
  addText(this=rtf, ...)
  addNewLine(this=rtf)
  endParagraph(this=rtf)
  addTable(this=rtf, TAB, font.size=fontt, row.names=row.names)
  addNewLine(this=rtf)
  EchoEnv$tabcount <- tabc + 1
  }
