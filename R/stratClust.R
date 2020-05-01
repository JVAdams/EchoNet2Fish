#' Estimation from a Stratified Survey Design
#'
#' Estimate the population response from a stratified survey design in which
#' single stage cluster samples were taken from each stratum, and the size of
#' each stratum is known.
#' @param elementdf
#'   A data frame with one row for each sample unit (element),
#'   containing variables identifying the stratum, cluster, and
#'   response value for each element.
#' @param stratum
#'   A character scalar giving the name of the variable in \code{elementdf}
#'   that identifies the strata.
#' @param cluster
#'   A character scalar giving the name of the variable in \code{elementdf}
#'   that identifies the clusters.
#' @param response
#'   A character scalar giving the name of the variable in \code{elementdf}
#'   that contains the response values.
#' @param sizedf
#'   A data frame with one row for each stratum.
#'   It should contain variables identifying the stratum and the
#'   size of each stratum.
#' @param stratum2
#'   A character scalar giving the name of the variable in \code{sizedf}
#'   that identifies the strata, default is \code{stratum},
#'   the same name as in \code{elementdf}.
#' @param size
#'   A character scalar giving the name of the variable in \code{sizedf}
#'   that contains the sizes of the strata.
#' @return
#'   A list of three data frames (tibbles, actually).
#'   \code{Cluster} has a row for each cluster with four variables:
#'   \itemize{
#'     \item \code{h} = stratum (may be character, factor, or numeric)
#'     \item \code{i} = cluster (may be character, factor, or numeric)
#'     \item \code{m_hi} = number of elements in stratum h, cluster i (numeric)
#'     \item \code{y_hi} = sum of the response for stratum h, cluster i (numeric)
#'   }
#'   \code{Stratum} has a row for each stratum with eight variables:
#'   \itemize{
#'     \item \code{h} = stratum (may be a character, factor, or numeric)
#'     \item \code{m_h} = number of elements in stratum h (numeric)
#'     \item \code{y_h} = sum of response in stratum h (numeric)
#'     \item \code{n_h} = number of clusters in stratum h (numeric)
#'     \item \code{ybar_h} = mean response for stratum h (numeric)
#'     \item \code{s_ybar_h} = standard deviation of ybar_h (numeric)
#'     \item \code{A_h} = size of stratum h (numeric)
#'     \item \code{W_h} = relative size of stratum h (numeric)
#'   }
#'   \code{Population} has one row with five numeric variables:
#'   \itemize{
#'     \item \code{A} = total size of all strata in population
#'     \item \code{ybar_str} = mean response for population per unit of size
#'     \item \code{s_ybar_str} = standard deviation of ybar_str
#'     \item \code{ytot_str} = total response for population
#'     \item \code{s_ytot_str} = standard deviation of ytot_str
#'   }
#'
#' @references
#' Cochran, W.G. 1977. \href{https://archive.org/details/Cochran1977SamplingTechniques_201703}{[Sampling Techniques]}. Wiley, New York.
#' @importFrom magrittr "%>%"
#' @import dplyr
#' @export
#' @examples
#' # Example data from a stratified survey design in which
#' # single stage cluster sampling is used in each stratum.
#' # Strata are areal regions of a lake, and the response are counts of fish.
#' counts <- data.frame(
#'  Stratum=rep(c("A", "B", "C"), c(5, 8, 8)),
#'  Cluster=rep(1:8, c(3, 2, 3, 2, 3, 2, 3, 3)),
#'  Element=c(1, 2, 3, 1, 2, 1, 2, 3, 1, 2, 1, 2, 3, 1, 2, 1, 2, 3, 1, 2, 3),
#'  Count = c(5:1, 6:21)
#' )
#' # Surface area (in hectares) corresponding to each lake stratum.
#' areas <- data.frame(
#'  Stratum=c("A", "B", "C"),
#'  A_h=c(10, 20, 40)
#' )
#' stratClust(elementdf=counts, stratum="Stratum", cluster="Cluster",
#'  response="Count", sizedf=areas, size="A_h")
#'
stratClust <- function(elementdf, stratum, cluster, response, sizedf,
  stratum2=stratum, size) {

  datj <- elementdf[, c(stratum, cluster, response)]
  names(datj) <- c("h", "i", "y_hij")
  dath <- sizedf[, c(stratum2, size)]
  names(dath) <- c("h", "A_h")

  # cluster means
  cluster <- datj %>%
    group_by(h, i) %>%
    summarise(
      m_hi = n(),
      y_hi = sum(y_hij)
    ) %>%
    ungroup()

  # stratum means
  stratum <- cluster %>%
    group_by(h) %>%
    summarise(
      m_h = sum(m_hi),
      y_h = sum(y_hi),
      n_h = n(),
      ybar_h = y_h / m_h,
      s_ybar_h = 1 / mean(m_hi) *
        #sqrt( sum((y_hi - ybar_h*m_hi)^2) / (n_h * (n_h-1)) )
        sqrt( sum((y_hi - ybar_h*m_hi)^2) / ((n_h-1)) )
    ) %>%
    ungroup() %>%
    left_join(dath, by="h") %>%
    mutate(
      W_h = A_h / sum(A_h)
    )

  # now expand up to strata
  population <- stratum %>%
    summarise(
      A = sum(A_h),
      ybar_str = sum(W_h * ybar_h),
      s_ybar_str = sum(W_h * s_ybar_h / sqrt(n_h))
    ) %>%
    mutate(
      ytot_str = A * ybar_str,
      s_ytot_str = A * s_ybar_str
    )

  list(Cluster=cluster, Stratum=stratum, Population=population)
}
