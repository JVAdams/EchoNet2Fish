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
#' @param short
#'   Logical scalar, indicating aspect of map area.  If TRUE, the default,
#'   the mapped area is assumed to be wider (longitudinally) than tall.
#'   Used to better arrange multiple maps on a single page.
#' @details
#'   A rich text file (rtf) with a *.doc file extension (so that it will be
#'   opened with Word by default) is saved to \code{maindir}.
#' @import rtf lubridate
#' @export
#'
exploreACMT <- function(maindir, rdat="ACMT", AC=TRUE, MT=TRUE, ageSp=NULL,
  short=TRUE) {

  load(paste0(maindir, rdat, ".RData"), envir=environment())
  LAKE <- keyvals[1]
  YEAR <- keyvals[2]

  # exploratory plots - save output to *.doc file ####

  docname <- paste0("L", LAKE, " Y", YEAR, " ACMT Explore ", today(), ".doc")
  doc <<- startrtf(file=docname, dir=maindir)

  explore <- 10*AC + MT
  descr <- recode(explore, c(0, 1, 10, 11),
    c("No", "Trawl", "Acoustic", "Acoustic and Trawl"))
  heading(paste0(YEAR, " Lake ", Lakenames[LAKE], " Exploration of ", descr,
    " Data   ", today()))

  para("R code written by Jean Adams for Dave Warner.")
  para(paste0(docname, " = this document."))
  rm(descr, docname)

  heading("INPUTS", 2)
  para(paste0("maindir = ", maindir, " = main input/output directory."))
  tab <- t(inputs)
  dimnames(tab)[[2]] <- NULL
  tabl("Input directories and files from reference file.", row.names=FALSE,
    TAB=tab)

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

    attach(sv)

    count <- 1:dim(sv)[2]
    lcount <- split(count, ceiling(count/35))
    for(i in 1:length(lcount)) {
      fig <- function() {
      	par(mfrow=c(7, 5), mar=c(3, 3, 2, 1))
      	dfPlot(sv[, lcount[[i]], drop=FALSE])
      }
      figu("Plot of variables ", paste(range(lcount[[i]]), collapse="-"),
        " in the Sv files.", newpage="port", FIG=fig)
    }

    # lat/lon plots
    fig <- function() {
    	mapSymbols(Lat_M, Lon_M, as.numeric(as.factor(Region_name)),
    	  "Colors indicate Region")
    	mapText(Lat_M, Lon_M, Region_name)
    }
    figu("Location of transects in Sv files.  Colors indicate Region.",
      newpage="port", FIG=fig)

    # close up look at each transect - lat/lon
    fig <- function() {
      mapMulti(Region_name, short=short, lon=Lon_M, lat=Lat_M,
        samescale=FALSE, IDcol=as.numeric(as.factor(Region_name)))
    }
    figu("Close up look at each transect location in Sv files.", newpage="port",
      FIG=fig)

    # interval by layer plots
    laymid <- -(Layer_depth_min + Layer_depth_max)/2
    lat.r <- tapply(Lat_M, Region_name, mean, na.rm=TRUE)
    Region_ord <- names(lat.r)[order(lat.r, decreasing=T)]
    fig <- function(x, xname) {
      plotIntLay(Interval, laymid, Region_name, Region_ord,
        colorVal(x), paste0("Colors indicate ", xname))
    }
    prefix <- "Interval by layer plots for Sv files.  Colors indicate "
    figu(prefix, "Depth_mean", FIG=function() fig(-Depth_mean, "Depth_mean"),
      newpage="port")
    figu(prefix, "Sv_mean", FIG=function() fig(Sv_mean, "Sv_mean"),
      newpage="port")
    figu(prefix, "PRC_ABC", FIG=function() fig(PRC_ABC^0.2, "PRC_ABC"),
      newpage="port")
    figu(prefix, "PRC_NASC", FIG=function() fig(PRC_NASC^0.2, "PRC_NASC"),
      newpage="port")
    figu(prefix, "Samples", FIG=function() fig(Samples, "Samples"),
      newpage="port")

    # plots comparing extremes with middle values
    fig <- function(x, lhk=TRUE, tt=FALSE) {
      varnames <- paste(x, c("S", "E", "M"), sep="_")
      caption <<- paste0("PROBLEM:  Comparing ", paste(varnames, collapse=", "),
        " for Sv files.")
      vars <- sapply(varnames, function(y) eval(parse(text=y)))
      plotValues(vars[, 1], vars[, 2], vars[, 3], lowhighKnown=lhk, varname=x,
        test=tt)
    }
    np <- fig("Ping", tt=TRUE)
  	if(np) figu(caption, FIG=function() fig("Ping", lhk=TRUE), newpage="port")
    np <- fig("Dist", lhk=FALSE, tt=TRUE)
  	if(np) figu(caption, FIG=function() fig("Dist"), newpage="port")
    np <- fig("Lat", lhk=FALSE, tt=TRUE)
  	if(np) figu(caption, FIG=function() fig("Lat"), newpage="port")
    np <- fig("Lon", lhk=FALSE, tt=TRUE)
  	if(np) figu(caption, FIG=function() fig("Lon"), newpage="port")
    fig <- function(...) {
      plotValues(decimal_date(Date_S), decimal_date(Date_E),
        decimal_date(Date_M), varname="Date", lhk=TRUE, ...)
    }
  	np <- fig(test=TRUE)
  	if(np) figu("PROBLEM:  Comparing Date_S, Date_E, and Date_M for Sv files.",
  	  newpage="port", FIG=fig)

    detach(sv)
    rm(np)

    ### TS

    heading("TS FILES", 2)
    para(paste0("The TS files have ", dim(ts)[1], " rows and ", dim(ts)[2],
      " columns."))

    tab <- dfSmry(ts)
    tabl("Quick summary table of variables in TS files.", TAB=tab)

    attach(ts)
    sur <- sort(unique(Region_name))
    lon.r <- tapply(Lon_M, Region_name, mean, na.rm=TRUE)
    lat.r <- tapply(Lat_M, Region_name, mean, na.rm=TRUE)

    count <- 1:dim(ts)[2]
    lcount <- split(count, ceiling(count/28))
    for(i in 1:length(lcount)) {
      fig <- function() {
      	par(mfrow=c(7, 4), mar=c(3, 3, 2, 1))
      	dfPlot(ts[, lcount[[i]], drop=FALSE])
      }
      figu("Plot of variables ", paste(range(lcount[[i]]), collapse="-"),
        " in the TS files.", newpage="port", FIG=fig)
    }

    # interval by layer plots
    start <- seq(16, 66, 10)
    end <- start+10
    laymid <- -(Layer_depth_min + Layer_depth_max)/2
    Region_ord <- names(lat.r)[order(lat.r, decreasing=T)]

    for(i in seq(along=start)) {
    	colz <- paste0("X.", start[i]:end[i])
    	sumtargs <- apply(ts[, colz], 1, sum)
    	title. <- paste("Colors indicate binned targets from, -", end[i], " to -",
    	  start[i], " dB", sep="")
    	fig <- function() {
  	    plotIntLay(Interval, laymid, Region_name, Region_ord,
          colorVal(sqrt(sumtargs)), title.)
    	}
  		figu(paste("Interval by layer plots for TS files. ", title.),
  		  newpage="port", FIG=fig)
    }

    detach(ts)
    rm(sur, lon.r, lat.r, start, end, i, colz, sumtargs, title.)

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

    rm(t1, t2, iu, ju, bigt1, bigt2, results, res)

  }

  if(MT) {# explore trawl files

    ### OPTROP
    heading("OP and TROP FILES", 2)

    optrop <- dfTidy(optrop)
    para(paste0("The OP/TROP files have ", dim(optrop)[1], " rows and ",
      dim(optrop)[2], " columns."))

    tab <- dfSmry(optrop)
    tabl("Quick summary table of variables in OP/TROP files.", TAB=tab)

    attach(optrop)
    pcols <- c("Op.Id", "Vessel", "Cruise", "Serial", "Lake", "Port",
    	"Beg.Depth", "End.Depth", "Distance", "Fishing_Temp", "Fishing_Depth",
      "Transect")

    tab <- optrop[is.na(Beg.Depth) | is.na(End.Depth) | is.na(Distance) |
        is.na(Fishing_Temp), pcols]
    if(dim(tab)[1] > 0) {
    	tabl("OP/TROP records with missing depth, distance, or temperature.",
    	  newpage="land", TAB=tab)
    } else {
    	para("All OP/TROP records have depth, distance, and temperature entered.")
    }

    set.time <- floor(Set_Time/100) + (Set_Time - 100*floor(Set_Time/100))/60
    tod <- rep("night", length(set.time))
    tod[set.time > 7 & set.time < 19] <- "day"
    tt <- table(tod)
    mostall <- names(which.max(table(tod)))

    if(length(tt)<1.5) {
    	if(mostall=="night") {
    		para("All OP/TROP records were taken at night.")
    	} else {
    		para("All OP/TROP records were taken during the day.")
    	}
    } else {
    	if(mostall=="night") {
    		tab <- optrop[tod=="day", c(pcols, "Set_Time")]
    		tabl("Most OP/TROP records were taken at night,",
          " but some were taken during the day.", TAB=tab)
    	} else {
    		tab <- optrop[tod=="night", c(pcols, "Set_Time")]
    		tabl("Most OP/TROP records were taken during the day,",
          " but some were taken at night.", TAB=tab)
    	}
    }

    tab <- optrop[!is.na(Beg.Depth) & !is.na(End.Depth) &
        abs(Beg.Depth - End.Depth) > 20, pcols]
    if(dim(tab)[1] > 0) {
    	tabl("OP/TROP records with > 20 m difference between",
        " beginning and ending bottom depth.", TAB=tab)
    } else {
    	para("All OP/TROP records have < 20 m difference between",
        " beginning and ending bottom depth.")
    }

    mind <- pmin(Beg.Depth, End.Depth, na.rm=T)
    tab <- optrop[!is.na(mind) & !is.na(Fishing_Depth) &
        Fishing_Depth > mind, pcols]
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
    figu("Plot of variables in the OP/TROP files.", newpage="port", FIG=fig)

    # lat/lon plots
    fig <- function(x) {
      var <- eval(parse(text=x))
    	mapSymbols(Latitude, Longitude, colorVal(as.numeric(as.factor(var))),
    	  paste("Colors indicate", x), pch=16, cushion=0.15)
    	mapText(Latitude, Longitude, var)
    }
    cap <- function(x) {
    	paste("Identification of", x, "in OP/TROP files.")
    }
    figu(cap("Port"), FIG=function() fig("Port"), newpage="port")
    figu(cap("Cruise"), FIG=function() fig("Cruise"), newpage="port")
    figu(cap("Transect"), FIG=function() fig("Transect"), newpage="port")
    maxd <- -pmax(Beg.Depth, End.Depth, na.rm=T)
    figu(cap("maxd"), FIG=function() fig("maxd"), newpage="port")
    figu(cap("Tow_Time"), FIG=function() fig("Tow_Time"), newpage="port")
    if("Tr_Design" %in% names(optrop)) {
      figu(cap("Tr_Design"), FIG=function() fig("Tr_Design"), newpage="port")
    }

    detach(optrop)

    rm(set.time, tod, tt, mostall, mind, maxd)

    ### trcatch
    addPageBreak(doc, width=11, height=8.5)
    heading("TRCATCH FILE", 2)

    trcatch <- dfTidy(trcatch)
    para(paste0("The TRCATCH file has ", dim(trcatch)[1], " rows and ",
      dim(trcatch)[2], " columns."))

    tab <- dfSmry(trcatch)
    tabl("Quick summary table of variables in TRCATCH file.", TAB=tab)

    attach(trcatch)
    sus <- sort(unique(Species))
    if("Beg.Depth" %in% names(trcatch)) {
    	tab <- trcatch[is.na(Beg.Depth) | is.na(End.Depth),
    		c("Op.Id", "Year", "Vessel", "Serial", "Lake", "Species", "Port_Name",
    		  "Beg.Depth", "End.Depth", "N")]
    	if(dim(tab)[1] > 0) {
    		tabl("TRCATCH records with missing beginning or ending depth.", TAB=tab)
    	}
    }

    missop <- setdiff(Op.Id, optrop$Op.Id)
    if(length(missop)>0) {
    	tab <- trcatch[Op.Id==missop, ]
    	tabl("TRCATCH records Op.Ids not in OPTROP.", TAB=tab)
    } else {
    		para("All TRCATCH Op.Ids are in OPTROP.")
    }

    fig <- function() {
    	par(mfrow=n2mfrow(dim(trcatch)[2]+3), mar=c(3, 3, 2, 1))
    	dfPlot(trcatch)
    	plotSpecies(N, "N")
    	plotSpecies(Weight, "Weight")
    	plotSpecies(Weight/N, "Weight/N")
    }
    figu("Plot of variables in the TRCATCH file.", newpage="port", FIG=fig)

    detach(trcatch)

    rm(sus, missop)

    ### trlf
    heading("TRLF FILE", 2)

    trlf <- dfTidy(trlf)
    para(paste0("The TRCATCH file has ", dim(trlf)[1], " rows and ",
      dim(trlf)[2], " columns."))

    tab <- dfSmry(trlf)
    tabl("Quick summary table of variables in TRLF file.", TAB=tab)

    attach(trlf)

    missop <- setdiff(Op.Id, optrop$Op.Id)
    showcols <- min(10, dim(trlf)[[2]])
    if(length(missop)>0) {
    	tab <- trlf[Op.Id==missop, 1:showcols]
    	tabl("TRLF records Op.Ids not in OPTROP.", TAB=tab)
    } else {
    		para("All TRLF Op.Ids are in OPTROP.")
    }

    fig <- function() {
    	par(mfrow=n2mfrow(dim(trlf)[2]), mar=c(3, 3, 2, 1))
    	dfPlot(trlf)
    }
    figu("Plot of variables in the TRLF file.", newpage="port", FIG=fig)

    fig <- function() {
      histMulti(x=Length, freq=N, bygroup=Species, xlab="Length  (mm)",
        samescale=FALSE)
    }
    figu("Length frequency histograms of species in the TRLF file.",
      "  Vertical red lines indicate the minimum and maximum lengths recorded.",
    	newpage="port", FIG=fig)

    detach(trlf)

    rm(missop, showcols) #, i, sel, sul, a, xr)

    if(!is.null(ageSp)) {

      for(i in seq_along(ageSp)) {
        sp <- ageSp[i]
        heading(paste("Age-Length Key for Species", sp), 2)

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
        	plotAgeLen(m2, inc=0.2, xlab="Length  (mm)", ylab="Age",
        	  main=paste("Age-length key for species", sp))
        }
        figu("Age-length key for species ", sp,
          ".  Circle size is proportional to",
          " probability of age, given length.",
        	"  Probabilities for all ages of a given length sum to one.",
          newpage="port", FIG=fig)

        rm(m, m2)
      }
    }

  }

  addSessionInfo(doc, locale=FALSE)

  endrtf()

}
