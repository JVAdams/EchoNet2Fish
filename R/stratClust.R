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
#' Scheaffer, R.L., Mendenhall III, W., Ott, R.L., Gerow, K., 2011. Elementary Survey Sampling, 7th ed. Brooks/Cole, Boston, MA.
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

  mydf <- elementdf[, c(stratum, cluster, response)]
  names(mydf) <- c("h", "i", "yhij")
  dath <- sizedf[, c(stratum2, size)]
  names(dath) <- c("h", "A_h")
  dath <- dath %>%
    mutate(
      W_h = A_h/sum(A_h)
    )

  # summarize by cluster
  smry_hc <- mydf %>%
    group_by(h, i) %>%
    summarise(
      m_hi=n(),
      y_hi=sum(yhij)
    ) %>%
    select(h, i, m_hi, y_hi)

  # summarize by stratum
  smry_h <- smry_hc %>%
    group_by(h) %>%
    summarise(
      m_h = sum(m_hi),
      mbar_h = mean(m_hi),
      y_h = sum(y_hi),
      n_h = n(),
      ybar_h = y_h/sum(m_hi),
      s2_ybar_h = sum((y_hi - ybar_h*m_hi)^2)/(n_h * mbar_h^2 * (n_h-1)),
      s_ybar_h = sqrt(s2_ybar_h),
      s2_h = n_h * s2_ybar_h,
      s_h = sqrt(s2_h)
    ) %>%
    full_join(dath, by="h") %>%
    select(h, m_h, y_h, n_h, ybar_h, s_ybar_h, A_h, W_h)

  # overall summary
  smry <- smry_h %>%
    summarise(
      A = sum(A_h),
      ybar_str = sum(W_h*ybar_h),
      # note that s_h^2 = n_h * s_ybar_h^2
      s2_ybar_str = sum((W_h^2*n_h*s_ybar_h^2)/n_h),
      s_ybar_str = sqrt(s2_ybar_str),
      ytot_str = A * ybar_str,
      s_ytot_str = A * s_ybar_str
    ) %>%
    select(A, ybar_str, s_ybar_str, ytot_str, s_ytot_str)

  list(Cluster=smry_hc, Stratum=smry_h, Population=smry)
}
