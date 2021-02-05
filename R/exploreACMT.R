#' Explore Acoustic and Midwater Trawl Data
#'
#' Explore acoustic and midwater trawl data.
#' @param maindir
#'   A character scalar giving the directory where output will be placed.
#'   Use forward slashes, e.g., "C:/temp/".
#' @param rdat
#'   A character scalar giving the name of the RData file in \code{maindir}
#'   with the acoustic and midwater trawl data, typically the output from
#'   \code{\link{readAll}}.
#' @param AC
#'   A logical scalar indicating if you want to explore the acoustic data,
#'   default TRUE.
#' @param MT
#'   A logical scalar indicating if you want to explore the midwater trawl
#'   data, default TRUE.
#' @param ageSp
#'   A numeric vector giving the species codes for species for which
#'   age-length keys should be used, default NULL.
#' @param TSrange
#'   A numeric vector of length 2, the target strength range of interest,
#'   minimum and maximum in dB, default c(-60, -30). Must be a whole number.
#' @param short
#'   Logical scalar, indicating aspect of map area.  If TRUE, the default,
#'   the mapped area is assumed to be wider (longitudinally) than tall.
#'   Used to better arrange multiple maps on a single page.
#' @details
#'   A rich text file (rtf) with a *.doc file extension (so that it will be
#'   opened with Word by default) is saved to \code{maindir}.
#' @importFrom lubridate today
#' @import rtf grDevices graphics
#' @export
#'

exploreACMT <- function(maindir, rdat="ACMT", AC=TRUE, MT=TRUE, ageSp=NULL,
                        short=TRUE, TSrange = c(-60, -27))  {

  load(paste0(maindir, rdat, ".RData"), envir=environment())
  LAKE <- keyvals[1]
  YEAR <- keyvals[2]

  # exploratory plots - save output to *.doc file ####

  docname <- paste0("L", LAKE, " Y", YEAR, " ACMT Explore ", lubridate::today(), ".doc")
  doc <<- startrtf(file=docname, dir=maindir)

  explore <- 10*AC + MT
  descr <- recode(explore, c(0, 1, 10, 11),
                  c("No", "Trawl", "Acoustic", "Acoustic and Trawl"))
  heading(paste0(YEAR, " Lake ", Lakenames[LAKE], " Exploration of ", descr,
                 " Data   ", lubridate::today()))

  para("Created using the R package EchoNet2Fish (https://github.com/JVAdams/EchoNet2Fish), written by Jean V. Adams for Dave Warner.")
  para(paste0(docname, " = this document."))

  heading("INPUTS", 2)
  para(paste0("maindir = ", maindir, " = main input/output directory."))
  tab <- t(inputs)
  tab <- tab[!is.na(tab), 1, drop=FALSE]
  dimnames(tab)[[2]] <- NULL
  tabl("Input directories and files from reference file.", TAB=tab)

  if(AC) {# explore Sv and TS files

    ### Sv
    heading("SV FILES", 2)

    para(paste0("The Sv files have ", dim(sv)[1], " rows and ", dim(sv)[2],
                " columns."))

    tab <- dfSmry(sv)
    tabl("Quick summary table of variables in Sv files.", TAB=tab)

    para(paste0("The following figures are exploratory plots of the Sv data",
                " that can be examined to check for potential problems.",
                "  Some figures are only printed if problems are detected,",
                " noted by the word PROBLEM in the figure caption."))

    count <- 1:dim(sv)[2]
    lcount <- split(count, ceiling(count/35))
    for(i in 1:length(lcount)) {
      fig <- function() {
        par(mfrow=c(7, 5), mar=c(3, 3, 2, 1))
        dfPlot(sv[, lcount[[i]], drop=FALSE])
      }
      figu("Plot of variables ", paste(range(lcount[[i]]), collapse="-"),
           " in the Sv files.", newpage="PORT", FIG=fig)
    }

    # lon/lat plots
    fig <- function() {
      with(sv, mapByGroup(bygroup=Region_name, lon=Lon_M, lat=Lat_M))
    }
    figu("Location of TRANSECTs in Sv files.  Colors indicate Region.",
         newpage="PORT", FIG=fig)

    # close up look at each TRANSECT - lon/lat
    fig <- function() {
      with(sv, mapMulti(Region_name, short=short, lon=Lon_M, lat=Lat_M,
                        samescale=FALSE, IDcol=as.numeric(as.factor(Region_name))))
    }
    figu("Close up look at each TRANSECT location in Sv files.", newpage="PORT",
         FIG=fig)

    # interval by layer plots
    laymid <- with(sv, -(Layer_depth_min + Layer_depth_max)/2)
    lat.r <- with(sv, tapply(Lat_M, Region_name, mean, na.rm=TRUE))
    Region_ord <- names(lat.r)[order(lat.r, decreasing=T)]
    fig <- function(x, xname) {
      with(sv, plotIntLay(Interval, laymid, Region_name, Region_ord,
                          colorVal(x), paste0("Colors indicate ", xname)))
    }
    prefix <- "Interval by layer plots for Sv files.  Colors indicate "
    figu(prefix, "Depth_mean", FIG=function() fig(-sv$Depth_mean, "Depth_mean"),
         newpage="PORT")
    figu(prefix, "Sv_mean", FIG=function() fig(sv$Sv_mean, "Sv_mean"),
         newpage="PORT")
    figu(prefix, "PRC_ABC", FIG=function() fig(sv$PRC_ABC^0.2, "PRC_ABC"),
         newpage="PORT")
    if(!is.null(sv$PRC_NASC)) {
      figu(prefix, "PRC_NASC", FIG=function() fig(sv$PRC_NASC^0.2, "PRC_NASC"),
           newpage="PORT")
    }
    if(!is.null(sv$Samples)) {
      figu(prefix, "Samples", FIG=function() fig(sv$Samples, "Samples"),
           newpage="PORT")
    }

    if(length(grep("_E", names(sv))) > 0) {

      # plots comparing extremes with middle values
      fig <- function(x, lhk=TRUE, tt=FALSE) {
        varnames <- paste(x, c("S", "E", "M"), sep="_")
        caption <<- paste0("PROBLEM:  Comparing ", paste(varnames, collapse=", "),
                           " for Sv files.")
        vars <- with(sv, sapply(varnames, function(y) eval(parse(text=y))))
        plotValues(vars[, 1], vars[, 2], vars[, 3], lowhighKnown=lhk, varname=x,
                   test=tt)
      }
      np <- fig("Ping", tt=TRUE)
      if(np) figu(caption, FIG=function() fig("Ping", lhk=TRUE), newpage="PORT")
      np <- fig("Dist", lhk=FALSE, tt=TRUE)
      if(np) figu(caption, FIG=function() fig("Dist"), newpage="PORT")
      np <- fig("Lat", lhk=FALSE, tt=TRUE)
      if(np) figu(caption, FIG=function() fig("Lat"), newpage="PORT")
      np <- fig("Lon", lhk=FALSE, tt=TRUE)
      if(np) figu(caption, FIG=function() fig("Lon"), newpage="PORT")
      if(!is.null(sv$Date_S) & !is.null(sv$Date_E)) {
        fig <- function(...) {
          with(sv, plotValues(decimal_date(Date_S), decimal_date(Date_E),
                              decimal_date(Date_M), varname="Date", lhk=TRUE, ...))
        }
        np <- fig(test=TRUE)
        if(np) figu("PROBLEM:  Comparing Date_S, Date_E, and Date_M for Sv files.",
                    newpage="PORT", FIG=fig)
      }
    }

    ### TS

    heading("TS FILES", 2)
    para(paste0("The TS files have ", dim(ts)[1], " rows and ", dim(ts)[2],
                " columns."))

    tab <- dfSmry(ts)
    tabl("Quick summary table of variables in TS files.", TAB=tab)

    sur <- sort(unique(ts$Region_name))
    lon.r <- with(ts, tapply(Lon_M, Region_name, mean, na.rm=TRUE))
    lat.r <- with(ts, tapply(Lat_M, Region_name, mean, na.rm=TRUE))

    count <- 1:dim(ts)[2]
    lcount <- split(count, ceiling(count/28))
    for(i in 1:length(lcount)) {
      fig <- function() {
        par(mfrow=c(7, 4), mar=c(3, 3, 2, 1))
        dfPlot(ts[, lcount[[i]], drop=FALSE])
      }
      figu("Plot of variables ", paste(range(lcount[[i]]), collapse="-"),
           " in the TS files.", newpage="PORT", FIG=fig)
    }

    # interval by layer plots
    # interval by layer plots
    startbin <- TSrange[[1]]
    endbin <- TSrange[[2]]
    init.start <- seq(-1*endbin, (-1*startbin) - 10, 10)
    init.end <- init.start + 10
    if(abs(startbin) - max(init.end) > 0) {
      start <- c(init.start, (max(init.start) + 10))
    } else {
      start <- init.start
    }

    init.end <- start + 10
    init.end <- init.end[init.end < abs(startbin)]
    if(max(init.end) - max(start) < 10) {
      end <- c(init.end, abs(startbin))
    } else {
      end <- start + 10
    }
    laymid <- with(ts, -(Layer_depth_min + Layer_depth_max)/2)
    Region_ord <- names(lat.r)[order(lat.r, decreasing=T)]

    for(i in seq(along=start)) {
      colz <- paste0("X.", start[i]:end[i])
      sumtargs <- apply(ts[, colz], 1, sum)
      title. <- paste("Colors indicate binned targets from, -", end[i], " to -",
                      start[i], " dB", sep="")
      fig <- function() {
        with(ts, plotIntLay(Interval, laymid, Region_name, Region_ord,
                            colorVal(sqrt(sumtargs)), title.))
      }
      figu(paste("Interval by layer plots for TS files. ", title.),
           newpage="PORT", FIG=fig)
    }


    #############################################################################
    # compare interval gaps in Sv and TS files
    heading("SV and TS FILES COMPARISON", 2)

    t1 <- table(sv$Interval, sv$Region_name)
    t2 <- table(ts$Interval, ts$Region_name)
    # create matrices with all the intervals and all the regions
    iu <- union(rownames(t1), rownames(t2))
    iu <- iu[order(as.numeric(iu))]
    ju <- union(colnames(t1), colnames(t2))
    bigt1 <- matrix(0, nrow=length(iu), ncol=length(ju), dimnames=list(iu, ju))
    bigt2 <- bigt1
    bigt1[rownames(t1), colnames(t1)] <- t1
    bigt2[rownames(t2), colnames(t2)] <- t2
    results <- matrix("", nrow=length(iu), ncol=length(ju), dimnames=list(iu, ju))

    # assign values when rows/columns don't match
    results[bigt1 < 0.5 & bigt2 > 0.5] <- "Gap in Sv"
    results[bigt1 > 0.5 & bigt2 < 0.5] <- "Gap in TS"
    res <- results[apply(results!="", 1, sum) > 0, apply(results!="", 2, sum) > 0]

    if(sum(dim(res)) > 0) {
      tab <- res
      tabl("Interval gaps in Sv and TS files don't match up.", TAB=tab)
    } else {
      para("Interval gaps in Sv and TS files match up.")
    }
  }

  if(MT) {# explore trawl files

    ### OPTROP
    heading("OP and TROP FILES", 2)

    optrop <- dfTidy(optrop)
    para(paste0("The OP/TROP files have ", dim(optrop)[1], " rows and ",
                dim(optrop)[2], " columns."))

    tab <- dfSmry(optrop)
    tabl("Quick summary table of variables in OP/TROP files.", TAB=tab)

    allcols <- names(optrop)
    pcols <- allcols[allcols %in% c("OP_ID", "VESSEL", "CRUISE", "SERIAL",
                                    "LAKE", "PORT", "BEG_DEPTH", "END_DEPTH", "Distance", "Fishing_Temp",
                                    "FISHING_DEPTH", "TRANSECT")]

    nacols <- allcols[allcols %in%
                        c("BEG_DEPTH", "END_DEPTH", "DISTANCE", "FISHING_TEMP")]
    narows <- apply(is.na(optrop[, nacols]), 1, any)
    tab <- optrop[narows, pcols]

    if(dim(tab)[1] > 0) {
      tabl("OP/TROP records with missing depth, distance, or temperature.",
           newpage="land", TAB=tab)
    } else {
      para("All OP/TROP records have depth, distance, and temperature entered.")
    }

    if(!is.null(optrop$Set_Time)) {
      set.time <- with(optrop,
                       floor(SET_TIME/100) + (Set_Time - 100*floor(SET_TIME/100))/60)
      tod <- rep("night", length(set.time))
      tod[set.time > 7 & set.time < 19] <- "day"
      tt <- table(tod)
      mostall <- names(which.max(table(tod)))

      if(length(tt) < 1.5) {
        if(mostall=="night") {
          para("All OP/TROP records were taken at night.")
        } else {
          para("All OP/TROP records were taken during the day.")
        }
      } else {
        if(mostall=="night") {
          tab <- optrop[tod=="day", c(pcols, "SET_TIME")]
          tabl("Most OP/TROP records were taken at night,",
               " but some were taken during the day.", TAB=tab)
        } else {
          tab <- optrop[tod=="night", c(pcols, "SET_TIME")]
          tabl("Most OP/TROP records were taken during the day,",
               " but some were taken at night.", TAB=tab)
        }
      }
    }

    tab <- with(optrop, optrop[!is.na(BEG_DEPTH) & !is.na(END_DEPTH) &
                                 abs(BEG_DEPTH - END_DEPTH) > 20, pcols])
    if(dim(tab)[1] > 0) {
      tabl("OP/TROP records with > 20 m difference between",
           " beginning and ending bottom depth.", TAB=tab)
    } else {
      para("All OP/TROP records have < 20 m difference between",
           " beginning and ending bottom depth.")
    }

    mind <- with(optrop, pmin(BEG_DEPTH, END_DEPTH, na.rm=T))
    tab <- with(optrop, optrop[!is.na(mind) & !is.na(FISHING_DEPTH) &
                                 FISHING_DEPTH > mind, pcols])
    if(dim(tab)[1] > 0) {
      tabl("OP/TROP records with fishing depth > beginning or",
           " ending bottom depth.", TAB=tab)
    } else {
      para("All OP/TROP records have fishing depths < beginning and",
           " ending bottom depths.")
    }

    fig <- function() {
      par(mfrow=n2mfrow(dim(optrop)[2]), mar=c(3, 3, 2, 1))
      dfPlot(optrop)
    }
    figu("Plot of variables in the OP/TROP files.", newpage="PORT", FIG=fig)

    # lon/lat plots
    fig <- function(x) {
      var <- with(optrop, eval(parse(text=x)))
      with(optrop, mapByGroup(bygroup=var, lon=Longitude, lat=Latitude,
                              colorz=colorVal(as.numeric(as.factor(var))), pch=16, cushion=0.15))
    }
    cap <- function(x) {
      paste("Identification of", x, "in OP/TROP files.")
    }
    if(!is.null(optrop$PORT)) {
      figu(cap("PORT"), FIG=function() fig("PORT"), newpage="PORT")
    }
    if(!is.null(optrop$CRUISE)) {
      figu(cap("CRUISE"), FIG=function() fig("CRUISE"), newpage="PORT")
    }
    figu(cap("TRANSECT"), FIG=function() fig("TRANSECT"), newpage="PORT")
    maxd <- with(optrop, -pmax(BEG_DEPTH, END_DEPTH, na.rm=T))
    figu(cap("maxd"), FIG=function() fig("maxd"), newpage="PORT")
    if(!is.null(optrop$Tow_Time)) {
      figu(cap("Tow_Time"), FIG=function() fig("Tow_Time"), newpage="PORT")
    }
    if("Tr_Design" %in% names(optrop)) {
      figu(cap("Tr_Design"), FIG=function() fig("Tr_Design"), newpage="PORT")
    }

    ### trcatch
    addPageBreak(doc, width=11, height=8.5)
    heading("TRCATCH FILE", 2)

    trcatch <- dfTidy(trcatch)
    para(paste0("The TRCATCH file has ", dim(trcatch)[1], " rows and ",
                dim(trcatch)[2], " columns."))

    tab <- dfSmry(trcatch)
    tabl("Quick summary table of variables in TRCATCH file.", TAB=tab)

    sus <- sort(unique(trcatch$SPECIES))
    if("BEG_DEPTH" %in% names(trcatch)) {
      tab <- with(trcatch, trcatch[is.na(BEG_DEPTH) | is.na(END_DEPTH),
                                   c("OP_ID", "YEAR", "VESSEL", "SERIAL", "LAKE", "SPECIES", "PORT_Name",
                                     "BEG_DEPTH", "END_DEPTH", "N")])
      if(dim(tab)[1] > 0) {
        tabl("TRCATCH records with missing beginning or ending depth.", TAB=tab)
      }
    }

    missop <- setdiff(trcatch$OP_ID, optrop$OP_ID)
    if(length(missop)>0) {
      tab <- trcatch[trcatch$OP_ID==missop, ]
      tabl("TRCATCH records OP_IDs not in OPTROP.", TAB=tab)
    } else {
      para("All TRCATCH OP_IDs are in OPTROP.")
    }

    fig <- function() {
      par(mfrow=n2mfrow(dim(trcatch)[2]+3), mar=c(3, 3, 2, 1))
      dfPlot(trcatch)
      with(trcatch, {
        plotSPECIES(N, "N", x=SPECIES)
        plotSPECIES(WEIGHT, "WEIGHT", x=SPECIES)
        plotSPECIES(WEIGHT/N, "WEIGHT/N", x=SPECIES)
      })
    }
    figu("Plot of variables in the TRCATCH file.", newpage="PORT", FIG=fig)

    ### trlf
    heading("TRLF FILE", 2)

    trlf <- dfTidy(trlf)
    para(paste0("The TRLF file has ", dim(trlf)[1], " rows and ",
                dim(trlf)[2], " columns."))

    tab <- dfSmry(trlf)
    tabl("Quick summary table of variables in TRLF file.", TAB=tab)

    missop <- setdiff(trlf$OP_ID, optrop$OP_ID)
    showcols <- min(10, dim(trlf)[[2]])
    if(length(missop)>0) {
      tab <- trlf[trlf$OP_ID==missop, 1:showcols]
      tabl("TRLF records OP_IDs not in OPTROP.", TAB=tab)
    } else {
      para("All TRLF OP_IDs are in OPTROP.")
    }

    trlfmiss <- with(trlf,
                     is.na(OP_ID) | is.na(SPECIES) | is.na(LENGTH) | is.na(N))
    if(sum(trlfmiss) > 0) {
      warning("TRLF is missing some data.  See output rePORT for details.",
              call.=FALSE)
      tabl("TRLF records with missing data.",
           "  These records are excluded from the following plots and tables.",
           TAB=trlf[trlfmiss, ])
      trlf <- trlf[!trlfmiss, ]
    }

    fig <- function() {
      par(mfrow=n2mfrow(dim(trlf)[2]), mar=c(3, 3, 2, 1))
      dfPlot(trlf)
    }
    figu("Plot of variables in the TRLF file.", newpage="PORT", FIG=fig)

    fig <- function() {
      with(trlf, histMulti(x=LENGTH, freq=N, bygroup=SPECIES,
                           xlab="LENGTH  (mm)", samescale=FALSE))
    }
    figu("LENGTH frequency histograms of species in the TRLF file.",
         "  Vertical red lines indicate the minimum and maximum lengths recorded.",
         newpage="PORT", FIG=fig)

    if(!is.null(ageSp)) {

      for(i in seq_along(ageSp)) {
        sp <- ageSp[i]
        heading(paste("Age-LENGTH Key for SPECIES", sp), 2)

        key <- eval(parse(text=paste0("key", i)))
        m <- key[, grep("age", names(key), ignore.case=TRUE)]
        dimnames(m)[[1]] <- key[, grep("mm", names(key), ignore.case=TRUE)]
        dimnames(m)[[2]] <- gsub("age", "", dimnames(m)[[2]], ignore.case=TRUE)
        m <- as.matrix(m)
        m2 <- m[apply(m, 1, sum) > 0, apply(m, 2, sum) > 0]
        para(paste0("The age-length key for species ", sp, " has ", dim(m2)[1],
                    " length categories and ", dim(m2)[2], " age categories."))
        tab <- m2
        tabl(paste0("Age-length key for species ", sp, "."), TAB=tab)

        fig <- function() {
          par(mar=c(4, 4, 2, 1), cex=1.5)
          plotAgeLen(m2, inc=0.2, xlab="LENGTH  (mm)", ylab="Age",
                     main=paste("Age-length key for species", sp))
        }
        figu("Age-length key for species ", sp,
             ".  Circle size is proPORTional to",
             " probability of age, given length.",
             "  Probabilities for all ages of a given length sum to one.",
             newpage="PORT", FIG=fig)
      }
    }

  }

  endrtf()

}
