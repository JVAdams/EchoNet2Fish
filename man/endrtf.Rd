% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/endrtf.R
\name{endrtf}
\alias{endrtf}
\title{Write and Close an RTF Document}
\usage{
endrtf(rtf = doc, details = TRUE, ...)
}
\arguments{
\item{rtf}{An rtf object, default \code{doc}.}

\item{details}{Logical scalar indicating if session details should be added to
the end of the document, default FALSE.}

\item{...}{Additional parameters to \code{rtf::addPageBreak}.}
}
\description{
Write and close an rtf (rich text format) document.
}
\references{
This is a copy of the \code{endrtf} function from the
  \href{https://github.com/JVAdams/GLFC}{[GLFC]} package.
}
\seealso{
\code{\link{startrtf}} for an example, \code{\link{heading}},
  \code{\link{para}}, \code{\link{tabl}},
  \code{\link{figu}},
  \code{\link[rtf]{RTF}}, \code{rtf::addPageBreak}.
}
