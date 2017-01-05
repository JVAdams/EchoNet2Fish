#' Estimate Fish Density and Biomass from Acoustic and Midwater Trawl Data
#'
#' Estimate fish density (number per ha) and biomass (kg per ha) from
#' acoustic and midwater trawl data.
#' @param maindir
#'   A character scalar giving the directory where \code{rdat} is located
#'   and where output will be placed.  Use forward slashes, e.g., "C:/temp/".
#' @param rdat
#'   A character scalar giving the name of the RData file in \code{maindir}
#'   with the acoustic and midwater trawl data, typically the output from
#'   \code{\link{readAll}}.
#' @param ageSp
#'   A numeric vector giving the species codes for species for which
#'   age-length keys should be used, default NULL.
#' @param TSrange
#'   A numeric vector of length 2, the target strength range of interest,
#'   minimum and maximum in dB, default c(-60, -30).
#' @param psi
#'   A numeric scalar, the transducer-specific two-way equivalent beam angle
#'   in steradians, default 0.007997566.
#' @param soi
#'   A numeric vector, codes of fish species for which estimates will be
#'   generated, default c(106, 109, 203, 204).
#' @param region
#'   A character vector, names of regions used in laying out sampling design.
#' @param regArea
#'   A numeric vector, corresponding areas (in ha) of \code{region}.
#' @param spInfo
#'   A data frame with five variables
#'   \itemize{
#'     \item \code{sp} = numeric, species code, must include all codes listed
#'       in \code{soi}, may include codes not listed in \code{soi}
#'     \item \code{spname} = character, species name
#'     \item \code{lcut} = numeric, the length cut off (in mm) at which to
#'       divide the corresponding species data into two groups (those with fish
#'       lengths <= lcut and > lcut) for estimation,
#'       use 0 for species with no length cut offs
#'     \item \code{lwa} and \code{lwb} = numeric parameters of length-weight
#'       relations, Wg = \code{lwa} * Lmm ^ \code{lwb}, where Wg is the weight
#'       (in g) and Lmm is the total length (in mm)
#'   }
#' @param short
#'   Logical scalar, indicating aspect of map area.  If TRUE, the default,
#'   the mapped area is assumed to be wider (longitudinally) than tall.
#'   Used to better arrange multiple maps on a single page.
#' @inheritParams sliceCat
#'
#' @details
#'   A rich text file (rtf) with a *.doc file extension (so that it will be
#'   opened with Word by default) is saved to \code{maindir}.
#'
#'   The sigma for each acoustic cell is estimated as the mean of the
#'   linearized target strength (TS) weighted by the number of targets in
#'   each dB bin using the TS frequency distribution.
#'
#'   The number of scatterers per unit volume, Nv,
#'   is estimated according to Sawada et al. (1993) (see \code{\link{estNv}}).
#'   So called "biased" sigmas where Nv > 0.1 are replaced
#'   with mean "unbiased" sigmas from cells in the same layer
#'   and (if possible) transect.  Then, Nv is recalculated.
#'
#'   Eight different data frames of estimates are saved as objects in an Rdata
#'   file and are written to csv files:
#'     lake-wide totals \code{laketots_millions} and \code{laketots_t}
#'       (in millions and t),
#'     lake-wide means \code{lakemeans_nph} and \code{lakemeans_gph}
#'       (in numbers and g per ha), and
#'     interval means \code{intmeans_nph} and \code{intmeans_gph}
#'       (in numbers and g per ha).
#'     interval and layer means \code{intlaymeans_nph} and
#'       \code{intlaymeans_gph} (in numbers and g per ha).
#'   The Rdata and csv files are named using the lake and the year.
#'
#'   The data frames of lake-wide totals and lake-wide means have a row
#'   for each species group, a column for each slice, and
#'   additional columns for the total (of all slices) with the standard
#'   error and relative standard error.
#'
#'   The data frames of the interval means have a row for each region and
#'   interval, a column for each species group, and additional columns for
#'   region area, and the interval bottom depth, latitude and longitude.
#'
#'   The data frames of the interval and layer means have a row for each
#'   region, interval, and layer, a column for each species group, and
#'   many additional columns.
#'
#' @importFrom class knn1
#' @importFrom RColorBrewer brewer.pal
#' @importFrom survey svydesign svytotal svymean svyby
#' @import lubridate rtf graphics stats utils
#' @export
#'
estimateACMT <- function(maindir, rdat="ACMT", ageSp=NULL, region, regArea,
  TSrange=c(-60, -30), psi=0.007997566, soi=c(106, 109, 203, 204),
  spInfo, sliceDef, short=TRUE) {


  # 1.  Initial stuff ####

  load(paste0(maindir, rdat, ".RData"), envir=environment())
  LAKE <- keyvals[1]
  YEAR <- keyvals[2]

  old_options <- options(stringsAsFactors=FALSE, survey.lonely.psu="remove")
  on.exit(options(old_options))

  # make sure selected lake and year is represented in data provided
  ly <- LAKE %in% optrop$Lake & YEAR %in% optrop$Year
  if(length(ly)<1) stop(paste0("\nNo information from ",
    Lakenames[LAKE], " in ", YEAR, " in RVCAT data.\n\n"))

  # make sure species names are character (not factor)
  spInfo$spname <- as.character(spInfo$spname)

  # make sure all species of interest are listed in info file
  xtra <- setdiff(soi, spInfo$sp)
  if(length(xtra)>0) stop(paste0("\nThere is at least one species listed in soi= that has no information in spInfo=: ",
    paste(xtra, collapse=", "), "."))

  # create rtf document to save printed output (tables and figures)
  docname <- paste0("L", LAKE, " Y", YEAR, " ACMT Estimate ", today(), ".doc")
  doc <<- startrtf(file=docname, dir=maindir)
  heading(paste0(YEAR, " Lake ", Lakenames[LAKE],
    " Estimation from Acoustic and Trawl Data   ", today()))
  para("Created using the R package EchoNet2Fish (https://github.com/JVAdams/EchoNet2Fish), written by Jean V. Adams for Dave Warner.")
  para(paste0(docname, " = this document."))
  heading("INPUTS", 2)
  para(paste0("maindir = ", maindir, " = main input/output directory."))
  para(paste0("TSrange = ", TSrange[1], " to ", TSrange[2],
    " = TS range of interest."))
  para(paste0("psi = ", psi,
    " = the transducer-specific two-way equivalent beam angle in steradians."))

  # make sure we have age-length keys for the species that need it
  if(is.null(ageSp)) {
    para("Ages will NOT be used.")
  } else {
    ageSp <- sort(ageSp)
    para("Ages will be used for ",
      paste(with(spInfo, spname[sp %in% ageSp]), collapse=", "), ".")
    if(exists("key1")) {
      if(exists("key2")) {
        ageksp <- c(key1$sp, key2$sp)
        agekey <- list(key1, key2)
        sdf <- setdiff(ageSp, ageksp)
        if(length(sdf)>0) stop("No age-length key available for ", sdf)
      } else {
        ageksp <- key1$sp
        agekey <- list(key1)
        sdf <- setdiff(ageSp, ageksp)
        if(length(sdf)>0) stop("No age-length key available for ", sdf)
      }
    } else {
      stop("No age-length key(s) available.")
    }
  }


  # 2.  Estimate sigma for each cell using TS frequency dist file ####
  ts$sigma <- sigmaAvg(TSdf=ts, TSrange=TSrange)


  # 3.  Merge Sv and sigma data ####

  # use region.interval.layer as unique identifier
  sv$UID <- interaction(gsub(" ", "", sv$Region_name), sv$Interval, sv$Layer)
  sv$source.sv <- sv$source

  ts$UID <- interaction(gsub(" ", "", ts$Region_name), ts$Interval, ts$Layer)
  ts$source.ts <- ts$source

  svdup <- sv[sv$UID %in% sv$UID[duplicated(sv$UID)], ]
  tsdup <- ts[ts$UID %in% ts$UID[duplicated(ts$UID)], ]

  if(dim(svdup)[1]>0) {
    print(svdup[, c("Region_name", "Interval", "Layer", "source.sv")])
    stop("There should be only one row in SV for each Region_name, Interval, and Layer.")
  }

  if(dim(tsdup)[1]>0) {
    print(tsdup[, c("Region_name", "Interval", "Layer", "source.ts")])
    stop("There should be only one row in TS for each Region_name, Interval, and Layer.")
  }

  # merge sv and ts files
  svts <- merge(sv[, c("UID", "Region_name", "Interval", "Layer",
    "Layer_depth_min", "Layer_depth_max", "Lat_M", "Lon_M", "year", "Date_M",
    "Sv_mean", "Depth_mean", "PRC_ABC", "source.sv")],
  	ts[, c("UID", "source.ts", "sigma")],
  	by="UID", all=TRUE)

  # get rid of blanks in Region_name
  svts$Region_name <- gsub(" ", "", svts$Region_name)

  svx <- setdiff(sv$UID, ts$UID)
  tsx <- setdiff(ts$UID, sv$UID)

  if(length(svx)>0) {
    sel <- svts$UID %in% svx
    tab <- svts[sel, c("Region_name", "Interval", "Layer", "source.sv")]
  	tabl("There is at least one region-interval-layer combination that occurs",
      " in the SV data but not in the TS data.",
  		"  These data will be removed from further calculations.", TAB=tab)
  	svts <- svts[!sel, ]
  }

  if(length(tsx)>0) {
    sel <- svts$UID %in% tsx
    tab <- svts[sel, c("Region_name", "Interval", "Layer", "source.ts")]
  	tabl("There is at least one region-interval-layer combination that occurs",
      " in the TS data but not in the SV data.",
  		"  These data will be removed from further calculations.", TAB=tab)
  	svts <- svts[!sel, ]
  }

  # if there are more rows in the merged data frame than in
  # the original sv file, something's wrong
  # if(dim(svts)[1] > dim(sv)[1]) {
 	# sel <- is.na(svts$Interval)
 	# tab <- ts[ts$UID %in% svts$UID[sel], c("Region_name", "Interval", "Layer",
 	#   "source.ts")]
  # 	tab <- ts[ts$UID %in% setdiff(svts$UID, ts$UID),
  # 	  c("Region_name", "Interval", "Layer", "source.ts")]
  # 	tabl("There is at least one region-interval-layer combination that occurs",
  #     " in the TS data but not in the SV data.",
  # 		"  These data will be removed from further calculations.", TAB=tab)
  # 	svts <- svts[!sel, ]
  # }

  # before making changes to sigma, keep the original value for later reference
  svts$sigma.orig <- svts$sigma


  # 4.  Estimate Nv ####

  svts$n1 <- with(svts, estrhov(Sv=Sv_mean, sigma=sigma))
  svts$nv <- with(svts, estNv(psi=psi, R=Depth_mean, rhov=n1))


  # 5.  Replace "biased" sigmas where Nv>0.1 with mean "unbiased" sigma ####
  # from cells in the same layer and (if possible) transect

  svts$sigma <- replaceBiasedSigma(df=svts, varNv="nv", varsigmabs="sigma",
    varTranLay=c("Region_name", "Layer", "year"))

  sel <- is.na(svts$sigma)
  if(sum(sel)>0) {
  	look3 <- svts[sel, ]
  	tab <- table(look3$Region_name, look3$Layer)
  	tabl("Frequency of observations with missing sigmas by transect (row) and",
      " layer (column).",
  		"  These are layers that had no targets in any transect.",
  		"  They will be removed from further calculations.",
  		"  ", TAB=tab)
  	svts <- svts[!sel, ]
  }

  # assign the value of zero to sigmas where there were no single targets
  # There will be cells without single targets, so not all rows of Sv can
  #   get sigma.
  # I assign these a fish density of zero, because I never have zero targets
  #   because of high-density inability to detect targets.
  svts$sigma[is.na(svts$sigma)] <- 0


  # 6.  Recalculate Nv and estimate density ####

  svts$n1 <- with(svts, estrhov(Sv=Sv_mean, sigma=sigma))
  svts$nv <- with(svts, estNv(psi=psi, R=Depth_mean, rhov=n1))
  svts$fish_ha <- with(svts, paDens(sigma, PRC_ABC, hectare=TRUE))


  # 7.  Add classifiers to acoustic data ####

  # bottom depth range in each interval
  depth.botmin <- aggregate(Layer_depth_min ~ Interval + Region_name, max,
    data=svts)
  names(depth.botmin)[names(depth.botmin)=="Layer_depth_min"] <- "depth.botmin"
  depth.botmax <- aggregate(Layer_depth_max ~ Interval + Region_name, max,
    data=svts)
  names(depth.botmax)[names(depth.botmax)=="Layer_depth_max"] <- "depth.botmax"
  depth.bot <- merge(depth.botmin, depth.botmax, all=TRUE)
  svts4 <- merge(svts, depth.bot, all=TRUE)
  svts4$depth_botmid <- (svts4$depth.botmin + svts4$depth.botmax)/2

  # define slice
  svts5 <- data.frame(svts4,
    slice=sliceCat(sliceDef, fdp=svts4$Depth_mean, bdp=svts4$depth_botmid,
      lon=svts4$Lon_M, lat=svts4$Lat_M, reg=substring(svts4$Region_name, 1, 2)))


  # 8.  Add classifiers to trawl data so they match those in acoustic data ####

  # bottom depth interval
  optrop$depth.botmin <- 10*floor(pmin(optrop$Beg.Depth, optrop$End.Depth)/10)
  optrop$depth.botmax <- 10*ceiling(pmax(optrop$Beg.Depth, optrop$End.Depth)/10)
  optrop$depth_botmid <- (optrop$Beg.Depth + optrop$End.Depth)/2
  # vertical layer
  overallmaxdep <-
    max(depth.bot$depth.botmax, optrop$depth.botmax, na.rm=TRUE) + 10
  optrop$layer <- cut(optrop$Fishing_Depth, seq(0, overallmaxdep, 10), right=FALSE)

  # define slice
  optrop <- data.frame(optrop,
    slice=sliceCat(sliceDef, fdp=optrop$Fishing_Depth, bdp=optrop$depth_botmid,
      lon=optrop$Longitude, lat=optrop$Latitude,
      reg=substring(optrop$Transect, 1, 2)))


  # 9.  Calculate mean proportion and mean weight of catch for trawl data ####

  # summarize trcatch by species and op.id
  trcatch2 <- aggregate(cbind(N, Weight) ~ Op.Id + Species, sum, data=trcatch)

  # estimate weight from length for each fish
  indx <- match(trlf$Species, spInfo$sp)
  trlf$estfw <- estWeight(trlf$Length, spInfo$lwa[indx], spInfo$lwb[indx])

  # calculate proportion of catch and mean weight for each MT and each
  # species-age-length group

  # determine ages of measured fish first, if necessary
  if(!is.null(ageSp)) {
  	allspsel <- c(ageSp, soi)
  	# create vector of ops for all selected species
  	# allops <- aggregate(N ~ Op.Id, sum,
  	#   data=trcatch2[trcatch2$Species %in% allspsel, ])$Op.Id
  	# create vector of all ops (not just those w/ selected species)
  	allops <- sort(unique(trcatch2$Op.Id))

  	# create list for results of all selected species
  	sum.n <- vector("list", length(allspsel))
  	names(sum.n) <- allspsel
  	mean.w <- sum.n
  	add.sp <- length(ageSp)

  	tidyup <- function(x, uniq) {
  		y <- x[match(uniq, dimnames(x)[[1]]), , drop=FALSE]
  		dimnames(y)[[1]] <- uniq
  		y[is.na(y)] <- 0
  		y[, apply(y, 2, sum)>0]
  	}

  	for(i in seq_along(ageSp)) {
    	# tally up lengths by mmgroup
    	lfa <- trlf[trlf$Species %in% ageSp[i], ]
    	lfa$mmgroup <- 10*round((lfa$Length+5)/10)-5
    	# total count and mean weight
    	ga <- aggregate(cbind(N, estfw) ~ Op.Id + mmgroup, sum, data=lfa)
    	gkeya <- merge(ga, agekey[[i]], all.x=TRUE)
    	# rename ages
    	agecolz <- grep("Age", names(gkeya))
    	names(gkeya)[agecolz] <-
    	  paste0(ageSp[i], ".A", substring(names(gkeya)[agecolz], 4, 10))
    	# apply probabilities from key to both counts and weights
    	# total numbers and mean weight by age group
    	tot.n <- apply(gkeya$N * gkeya[, agecolz], 2, tapply, gkeya$Op.Id, sum)
    	m.w <- apply(gkeya$estfw * gkeya[, agecolz], 2,
    	  tapply, gkeya$Op.Id, sum)/tot.n
    	sum.n[[i]] <- tidyup(tot.n, allops)
    	mean.w[[i]] <- tidyup(m.w, allops)
  	}

  } else {
  	allspsel <- soi
  	# create vector of ops for all selected species
  	# allops <- aggregate(N ~ Op.Id, sum,
  	#   data=trcatch2[trcatch2$Species %in% allspsel, ])$Op.Id
  	# create list for results of all selected species
  	# create vector of all ops (not just those w/ selected species)
  	allops <- sort(unique(trcatch2$Op.Id))

  	sum.n <- vector("list", length(soi))
  	names(sum.n) <- soi
  	mean.w <- sum.n
  	add.sp <- 0
  }

  tidyup2 <- function(x, uniqops, uniqlens) {
  	m <- array(0, dim=c(length(uniqops), length(lclong)),
  	  dimnames=list(uniqops, paste0(sp, ".L", lclong)))
  	if(dim(x)[1] > 0) {
  	  m[match(dimnames(x)[[1]], uniqops),
  	    match(dimnames(x)[[2]], uniqlens)] <- x
  	}
  	m
  }

  # determine groupings of other fish
  allops <- sort(unique(optrop$Op.Id))
  for(i in seq(soi)) {
  	sp <- soi[i]
  	lc <- spInfo$lcut[spInfo$sp==sp]
  	lclong <- unique(c(0, lc))
  	# tally up lengths by length group
  	lf <- trlf[trlf$Species==sp, ]
  	lf$mmgroup <- lc*(lf$Length > lc)
  	# total up numbers and weights by length group
  	tot.n <- tapply(lf$N, list(lf$Op.Id, lf$mmgroup), sum)
  	tot.n[is.na(tot.n)] <- 0
  	m.w <- tapply(lf$estfw, list(lf$Op.Id, lf$mmgroup), sum)/tot.n
  	m.w[is.na(m.w)] <- 0
  	sum.n[[add.sp+i]] <- tidyup2(tot.n, allops, lclong)
  	mean.w[[add.sp+i]] <- tidyup2(m.w, allops, lclong)
  }

  # Report the proportion of "other" by number and weight for each trawl ...
  # in case it's too large
  if(length(setdiff(unique(trcatch2$Species), soi))>0) {
    sumbyspec <- tapply(trcatch2$N,
      list(trcatch2$Op.Id, trcatch2$Species %in% soi), sum)
    sumbyspec[is.na(sumbyspec)] <- 0
    propother <- sumbyspec[, 1]/apply(sumbyspec, 1, sum)
    sel <- propother>0.1 & !is.na(propother)
    if(sum(sel)>0) {
    # 	look <- trcatch2[trcatch2$Op.Id %in% names(propother)[sel], ]
    # 	look$Other <- ifelse(look$Species %in% soi, "", "*")
    # 	tab <- look[order(look$Op.Id, look$Other=="", -look$N, look$Species),
    #     c("Op.Id", "Species", "N", "Weight")]
    	look <- trcatch2[trcatch2$Op.Id %in% names(propother)[sel] &
    	    !(trcatch2$Species %in% soi), ]
    	tab <- look[order(look$Op.Id, -look$N, look$Species),
    	  c("Op.Id", "Species", "N", "Weight")]
    	tabl("Species other than those selected (", paste(soi, collapse=", "),
    		") are ignored when calculating proportions, but other species make up",
        " > 10% of the *NUMBER* in at least one trawl haul.",
    		"  The locations of these tows are highlighted in Figure 1.", TAB=tab)
    	mtops <- names(propother)[sel]
    }
    sumbyspec <- tapply(trcatch2$Weight,
      list(trcatch2$Op.Id, trcatch2$Species %in% soi), sum)
    sumbyspec[is.na(sumbyspec)] <- 0
    propother <- sumbyspec[, 1]/apply(sumbyspec, 1, sum)
    sel <- propother>0.1 & !is.na(propother)
    if(sum(sel)>0) {
    # 	look <- trcatch2[trcatch2$Op.Id %in% names(propother)[sel], ]
    # 	look$Other <- ifelse(look$Species %in% soi, "", "*")
    # 	tab <- look[
    #     order(look$Op.Id, look$Other=="", -look$Weight, look$Species),
    # 	  c("Op.Id", "Other", "Species", "N", "Weight")]
    	look <- trcatch2[trcatch2$Op.Id %in% names(propother)[sel] &
    	    !(trcatch2$Species %in% soi), ]
    	tab <- look[order(look$Op.Id, -look$Weight, look$Species),
    	  c("Op.Id", "Species", "N", "Weight")]
    	tabl("Species other than those selected (", paste(soi, collapse=", "),
    		") are ignored when calculating proportions, but other species make up",
        " > 10% of the *WEIGHT* in at least one trawl haul.",
    		"  The locations of these tows are highlighted in Figure 1.", TAB=tab)
    	mtops <- if(exists("mtops")) c(mtops, names(propother)[sel]) else
    	  names(propother)[sel]
    }
  }

  # bring together total counts and mean weights
  ord <- order(names(sum.n))
  counts <- do.call(cbind, sum.n[ord])
  mnwts <- do.call(cbind, mean.w[ord])

  # calculate proportions by number
  # don't double count a species if it's in by both length and age
  # define the group type for each column of counts and wts as "A" for age
  # and "L" for length
  sp.grps <- dimnames(counts)[[2]]
  grp.sp <- sapply(strsplit(sp.grps, "\\."), "[", 1)
  grp.type <- substring(sapply(strsplit(sp.grps, "\\."), "[", 2), 1, 1)
  if(!is.null(ageSp) & any(sapply(ageSp, function(x) x %in% soi))) {
    sum.counts <- apply(counts[, grp.type=="L"], 1, sum)
    } else {
    sum.counts <- apply(counts, 1, sum)
    }
  nprops <- sweep(counts, 1, sum.counts, "/")
  nprops[is.na(nprops)] <- 0


  # 10. Find the nearest midwater trawl to each acoustic cell within slice ####

  # subset only the MT data with selected species captured
  opsub <- optrop[match(allops, optrop$Op.Id), ]
  # convert from lon/lat to UTM
  MTutm <- with(opsub, latlon2utm(Longitude, Latitude))
  ACutm <- with(svts5, latlon2utm(Lon_M, Lat_M))
  # unique slice in AC and MT data
  sus <- names(sliceDef)

  # determine nearest trawl
  svts5$nearmt <- NA
  for(i in seq(sus)) {
  	# select records from the selected slice
  	# exclude any records with missing slice or missing lon/lat info
  	selm <- opsub$slice==sus[i] & !is.na(opsub$slice) &
  	  !apply(is.na(MTutm), 1, any)
  	sela <- svts5$slice==sus[i] & !is.na(svts5$slice)&
  	  !apply(is.na(ACutm), 1, any)
  	# determine the nearest MT
  	if(sum(selm)) {
  		if(sum(selm) > 1) {
  			svts5$nearmt[sela] <- as.numeric(as.character(class::knn1(MTutm[selm, ],
  			  ACutm[sela, ], allops[selm])))
  		} else {
  			svts5$nearmt[sela] <- allops[selm]
  		}
  	}
  }

  # plot of apportionment ####

  fig <- function() {
    with(opsub, mapMulti(bygroup=slice, sug=names(sliceDef), plottext=TRUE,
      ID=Op.Id, short=short, lon=Longitude, lat=Latitude,
      rlon=range(Longitude, svts5$Lon_M, na.rm=TRUE),
      rlat=range(Latitude, svts5$Lat_M, na.rm=TRUE), misstext=" - No tows"))
  }
  figu("Location of midwater trawl hauls in slices.",
  	"  Numbers identify the OP_ID of each tow.  Colors are the same as",
    " in the next figure.",
  	"  Tows with > 10% of their catch (by number or weight) in 'other' species",
    " are shown in large, bold font.", FIG=fig, h=8, newpage="port")

  fig <- function() {
    mapAppor(MTgroup=opsub$slice, ACgroup=svts5$slice, sug=names(sliceDef),
      MTID=opsub$Op.Id, ACID=svts5$nearmt, short=short,
      MTlon=opsub$Longitude, MTlat=opsub$Latitude,
      AClon=svts5$Lon_M, AClat=svts5$Lat_M, misstext=" - No tows")
  }
  figu("Apportionment using slices.",
  	"  Each MT tow is shown as a white circle (o).",
  	"  Each AC interval is shown as a colored plus sign (+).",
  	"  Dotted lines encircle all the AC intervals (given the same color) that",
    " used each MT tow for apportionment.", FIG=fig, h=8, newpage="port")

  # plot of AC and MT data by slice ####
  if(length(unique(c(opsub$slice, ACgroup=svts5$slice))) > 4) {
  	orient <- "port"
  } else {
  	orient <- "land"
  }

  fig <- function() {
    plotACMTslice(MTgroup=opsub$slice, ACgroup=svts5$slice,
      MTbd=opsub$depth_botmid, ACbd=svts5$depth_botmid,
      MTwd=opsub$Fishing_Depth, ACwd=svts5$Depth_mean)
  }
  figu("Acoustic (left) and midwater trawl (right) data by slice.",
    FIG=fig, newpage=orient)


  # 11. Assign transects to regions (design strata) using transect names ####
  svts5$region <- substring(svts5$Region_name, 1, 2)
  svts5$regarea <- regArea[match(svts5$region, region)]

  # make sure that design strata match up with sampled strata
  sur <- sort(unique(svts5$region))
  if(!identical(sort(region), sur)) warning(
    paste0("\nStrata used in laying out the sampling design (",
      paste(sort(region), collapse=", "),
  	") do not match up with the strata actually sampled (",
      paste(sur, collapse=", "), ").\n\n"))

  if(short) {
  	orient <- "land"
  } else {
  	orient <- "port"
  }

  fig <- function() {
    mapByGroup(bygroup=svts5$region, lon=svts5$Lon_M, lat=svts5$Lat_M)
  }

  figu("Acoustic transect data, color coded by design-based strata.",
    FIG=fig, newpage=orient)

  look <- tapply(svts5$Region_name, svts5$region, function(x) sort(unique(x)))
  if(sum(sapply(look, length) < 2)) {
  	tab <- cbind(names(look), sapply(look, paste, collapse=", "))
  	tabl("Only one transect in at least one region.",
      "  Variance will be estimated with this region(s) removed.", TAB=tab)
  }


  # 12. Generate estimates for the species groups. ####

  # apply species group proportions to AC densities
  nph <- svts5$fish_ha * nprops[match(svts5$nearmt, allops), ]
  gph <- nph * mnwts[match(svts5$nearmt, allops), ]

  rownames(nph) <- NULL
  intlaymeans_nph <- cbind(svts5, nph)
  rownames(gph) <- NULL
  intlaymeans_gph <- cbind(svts5, gph)

  # summary of density by interval (summed densities over layers)
  intmeans_nph <- aggregate(nph ~ region + regarea + Region_name + Interval +
      depth_botmid + Lat_M + Lon_M, sum, data=svts5)
  names(intmeans_nph)[is.na(names(intmeans_nph))] <- sp.grps
  intmeans_gph <- aggregate(gph ~ region + regarea + Region_name + Interval +
      depth_botmid + Lat_M + Lon_M, sum, data=svts5)
  names(intmeans_gph)[is.na(names(intmeans_gph))] <- sp.grps

  intmeans_nph.domain <- aggregate(nph ~ region + regarea + Region_name + Interval +
      depth_botmid + Lat_M + Lon_M + slice, sum, data=svts5)
  names(intmeans_nph.domain)[is.na(names(intmeans_nph.domain))] <- sp.grps
  intmeans_gph.domain <- aggregate(gph ~ region + regarea + Region_name + Interval +
      depth_botmid + Lat_M + Lon_M + slice, sum, data=svts5)
  names(intmeans_gph.domain)[is.na(names(intmeans_gph.domain))] <- sp.grps

  ncols <- grep("\\.", names(intmeans_nph))
  fig <- function() {
    mapBy2Groups(df=intmeans_nph[, ncols], lon=intmeans_nph$Lon_M, lat=intmeans_nph$Lat_M,
      nrows=c(3, 4)[short+1])
  }
  figu("Acoustic density for each species group.  Groups are defined by",
    " length cut offs (L) in mm or ages (A).",
  	"  Darker and larger circles indicate higher density.",
    FIG=fig, newpage="port")

  gcols <- grep("\\.", names(intmeans_gph))
  fig <- function() {
    mapBy2Groups(df=intmeans_gph[, gcols], lon=intmeans_gph$Lon_M, lat=intmeans_gph$Lat_M,
      nrows=c(3, 4)[short+1])
  }
  figu("Acoustic biomass for each species group.  Groups are defined by",
    " length cut offs (L) in mm or ages (A).",
  	"  Darker and larger circles indicate greater biomass.",
    FIG=fig, newpage="port")


  # 13. Calculate lakewide totals based on stratified cluster sampling design ####

  # stratified cluster design ... regions are strata, transects are clusters
  # (nested in regions)
  SCD.n <- svydesign(ids=~Region_name, strata=~region, variables=intmeans_nph[, ncols],
    data=intmeans_nph, nest=TRUE, weights=~regarea)
  SCD.n2 <- as.data.frame(svytotal(as.matrix(intmeans_nph[, ncols]/1000000), SCD.n))
  SCD.n2ph <- as.data.frame(svymean(as.matrix(intmeans_nph[, ncols]), SCD.n))

  SCD.g <- svydesign(ids=~Region_name, strata=~region, variables=intmeans_gph[, gcols],
    data=intmeans_gph, nest=TRUE, weights=~regarea)
  SCD.g2 <- as.data.frame(svytotal(as.matrix(intmeans_gph[, gcols]/1000000), SCD.g))
  SCD.g2ph <- as.data.frame(svymean(as.matrix(intmeans_gph[, gcols]), SCD.g))

  # summarize by the new "slices" (domains) ... ignore old "strata",
  domainest <- function(dat, type="total") {
  	d <- NA
  	if(dim(dat)[1]>0) {
      scd <- svydesign(ids=~Region_name, strata=~region, data=dat, nest=TRUE,
        weights=~regarea)
      varnames <- grep("\\.", names(intmeans_nph.domain), value=TRUE)
      form <- formula(paste("~",
        paste("`", varnames, "`", sep="", collapse=" + ")))
  		if(type=="total") {
  		  d <- svyby(form, ~slice, design=scd, svytotal, keep.var=FALSE,
  		    drop.empty.groups=FALSE)
      	d <- t(d[, -1])/1000000
  		}
  		if(type=="mean") {
  		  d <- svyby(form, ~slice, design=scd, svymean, keep.var=FALSE,
  		    drop.empty.groups=FALSE)
      	d <- t(d[, -1])
  		}
      row.names(d) <- varnames
  	}
  	d
  }

  SCD.n.d2 <- domainest(intmeans_nph.domain, type="total")
  SCD.n.d2ph <- domainest(intmeans_nph.domain, type="mean")
  SCD.g.d2 <- domainest(intmeans_gph.domain, type="total")
  SCD.g.d2ph <- domainest(intmeans_gph.domain, type="mean")

  # combine information
  laketots_millions <- cbind(SCD.n.d2, SCD.n2, rse=100*SCD.n2$SE / SCD.n2$total)
  lakemeans_nph <- cbind(SCD.n.d2ph, SCD.n2ph, rse=100*SCD.n2ph$SE / SCD.n2ph$mean)
  laketots_t <- cbind(SCD.g.d2, SCD.g2, rse=100*SCD.g2$SE / SCD.g2$total)
  lakemeans_gph <- cbind(SCD.g.d2ph, SCD.g2ph, rse=100*SCD.g2ph$SE / SCD.g2ph$mean)

  # Save estimates to csv files
  save2csv <- c("laketots_millions", "laketots_t",
    "lakemeans_nph", "lakemeans_gph", "intmeans_nph", "intmeans_gph",
    "intlaymeans_nph", "intlaymeans_gph")
  outfiles <- paste0(maindir, "L", LAKE, " Y", YEAR, " ACMT Estimates ",
    save2csv, " ", today(), ".csv")
  invisible(lapply(seq(save2csv), function(i)
    write.csv(eval(parse(text=save2csv[i])), outfiles[i])))

  # Save estimates to Rdata file
  newrdat <- paste0("L", LAKE, " Y", YEAR, " ACMT")
  save(list=save2csv, file=paste0(maindir, newrdat, ".RData"))

  mypalette <- RColorBrewer::brewer.pal(6, "Set3")
  fig <- function() {
  	par(mar=c(4, 5, 0, 1), oma=c(0, 0, 2, 0), mfrow=c(1, 2), cex=1.2)
  	barplot(t(as.matrix(laketots_millions[, 1:(length(laketots_millions)-3)])), col=mypalette,
  	  horiz=TRUE, las=1, xlab="Number of fish  (millions)")
  	barplot(t(as.matrix(laketots_t[, 1:(length(laketots_t)-3)])), col=mypalette,
  	  horiz=TRUE, las=1, xlab="Biomass of fish  (t)",
  		legend.text=TRUE, args.legend=list(x="topright"))
  }
  figu("Acoustic survey lakewide estimates in number (left) and biomass (right)",
    " for each species group.",
  	"  Groups are defined by length cut offs (L) in mm or ages (A).",
  	"  Colors are used to identify contributions from different slices.",
    FIG=fig, h=5.8, w=9, newpage="land")

  # numbers in millions
  tab <- format(round(laketots_millions), big.mark=",")
  tabl("Lakewide estimates in number (millions) for each species group and",
    " slice.",
  	"  Groups are defined by length cut offs (L) in mm or ages (A).", TAB=tab)

  # biomass in metric tons (t)
  tab <- format(round(laketots_t), big.mark=",")
  tabl("Lakewide biomass estimates (t) for each species group and slice.",
  	"  Groups are defined by length cut offs (L) in mm or ages (A).", TAB=tab)

  endrtf()

}
