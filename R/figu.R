#' Add a Figure to an RTF Document
#'
#' Add a figure to an rtf (rich text format) document.
#' @param ...
#'   One or more character scalars (separated by commas) of text to use for
#'   the figure caption.
#' @param FIG
#'   A function to create a figure which will be added to the document,
#'   default \code{fig}.
#' @param rtf
#'   An rtf object, default \code{doc}.
#' @param figid
#'   Character scalar of caption identifier, default "Figure ".
#' @param fign
#'   Numeric scalar of figure number to use in caption,
#'   default \code{EchoEnv$figcount}.
#' @param boldt
#'   Logical scalar indicating if figure number should use bold font,
#'   default TRUE.
#' @param capunder
#'   Logical scalar indicating if caption should appear
#'   under the figure (TRUE, the default) or on top of the figure (FALSE).
#' @param w
#'   Numeric scalar width of figure in inches, default 6.5.
#' @param h
#'   Numeric scalar height of figure in inches, default 8.
#' @param rf
#'   Numeric scalar resolution of figure, default 300.
#' @param newpage
#'   Character scalar indicating if the figure should start on a new page in
#'   the document "port" for a new portrait page, "land" for
#'   a new landscape page, and "none" for no new page (the default).
#' @param omi
#'   Numeric vector, length 4, width of document page margins in inches
#'   (bottom, left, top, right), default c(1, 1, 1, 1).
#' @return
#'   A 1 is added to the numeric vector of length 1, \code{EchoEnv$figcount},
#'   stored in the working directory to keep track of the number of figures
#'   written to the rtf document, and label the captions accordingly.
#' @details
#'   The figure and caption are written to the rtf file.
#'   The size of a new page is assumed to be 8.5 by 11 inches.
#' @references
#'   This is a copy of the \code{figu} function from the
#'   \href{https://github.com/JVAdams/GLFC}{[GLFC]} package.
#' @seealso
#'   \code{\link{startrtf}} for an example, \code{\link{heading}},
#'   \code{\link{para}}, \code{\link{tabl}},
#'   \code{\link{endrtf}},
#'   \code{\link[rtf]{RTF}}.
#' @import
#'   rtf
#' @export

figu <- function(..., FIG=fig, rtf=doc, figid="Figure ",
    fign=EchoEnv$figcount, boldt=TRUE, capunder=TRUE, w=NULL, h=NULL,
    rf=300, newpage="none", omi=c(1, 1, 1, 1)) {
  wf <- if (is.null(w)) {
    6.5
  } else {
    w
  }
  hf <- if (is.null(h)) {
    8
  } else {
    h
  }
  if (newpage=="port") {
    addPageBreak(this=rtf, width=8.5, height=11, omi=omi)
  }
  if (newpage=="land") {
    wf <- if (is.null(w)) {
      9
    } else {
      w
    }
    hf <- if (is.null(h)) {
      5.5
    } else {
      h
    }
    addPageBreak(this=rtf, width=11, height=8.5, omi=omi)
  }
  if (capunder) {
    addPlot(this=rtf, plot.fun=FIG, width=wf, height=hf, res=rf)
    addNewLine(this=rtf)
  }
  startParagraph(this=rtf)
  addText(this=rtf, paste0(figid, fign, ".  "), bold=boldt)
  addText(this=rtf, ...)
  endParagraph(this=rtf)
  if (!capunder) {
    addPlot(this=rtf, plot.fun=FIG, width=wf, height=hf, res=rf)
  }
  addNewLine(this=rtf)
  EchoEnv$figcount <- fign + 1
}
