#' Calculate Nonlinear Estimates of WEIGHT-LENGTH Coefficients
#'
#' @param lake Numeric vector to select lake from RVCAT lake codes.
#' @param year Numeric vector to select year from RVCAT.
#' @param species Numeric vector to select species.
#' @param target Numeric vector to select target.
#' @param user RVCAT username.
#' @param password RVCAT password.
#' @param dbname Database name.
#' @param schma Schema to use, set as RVCAT.
#'
#' @return Coefficients for weight-length equation.
#' @export
#'
WtLenCoef <- function(lake = 2,
                      year = 2019,
                      species = 106,
                      target = c(1, 209, 210, 223, 252),
                      user = username,
                      password = password,
                      dbname = dbname,
                      schma = "RVCAT") {
  drv <- dbDriver('Oracle')
  conn <- dbConnect(
    drv = drv,
    dbname = dbname,
    user = username,
    password = password
  )
  # Create local "information" about op table structure that can
  # be used in a query later
  #
  # Now op is a database object. The command above connects to the database
  # and downloads a bare minimum of information on fields, data types, etc.
  # enough to allow manipulation of the object without physical download
  # of the data
  # (https://towardsdatascience.com/how-to-write-tidy-sql-queries-in-r-d6d6b2a3e17)
  options(stringsAsFactors = FALSE)
  drv <- dbDriver("Oracle")
  conn <- dbConnect(drv = drv, dbname = dbname, user = username,
                    password = password)
  opdata <- dplyr::tbl(conn, dbplyr::in_schema("RVCAT",
                                               "OP"))
  targetdata <- dplyr::tbl(conn, dbplyr::in_schema("RVCAT",
                                                   "OP_TARGET"))
  trfishdata <- dplyr::tbl(conn, dbplyr::in_schema("RVCAT",
                                                   "TR_FISH"))
  op <- filter(opdata, YEAR %in% year & LAKE %in% lake & SAMPLE_TYPE ==
                 1) %>% left_join(targetdata, by = "OP_ID") %>%
    filter(TARGET %in% target) %>% dplyr::collect()
  opid <- op$OP_ID
  tr_fish <- filter(trfishdata, OP_ID %in% opid & SPECIES ==
                      106) %>% dplyr::collect()


    tr_fish <- tr_fish %>% filter(!(is.na(LENGTH)) & !(is.na(WEIGHT)) & LENGTH<230 )

  unique(tr_fish$WEIGHT)
  #tr_fish$rat <- tr_fish$WEIGHT/tr_fish$LENGTH
  #tr_fish <- subset(tr_fish, rat <6)
  #tr_fish$WEIGHT[tr_fish$LENGTH < 20 & tr_fish$WEIGHT>1] <- NA
  plot(WEIGHT ~ LENGTH, data = tr_fish)
  cont <- nls.control(maxiter = 5000)
  wtlen.fit <- nls(WEIGHT ~ lwa * LENGTH^lwb, data = tr_fish,
                   start = list(lwa = 1.413254e-05, lwb = 2.867839), control = cont)
  lwa <- coef(wtlen.fit)[1]
  lwb <- coef(wtlen.fit)[2]
  mycoefs <- c(lwa, lwb)
  return(mycoefs)
}
