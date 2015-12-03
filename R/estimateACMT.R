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
#'   A numeric vector of length 2, the targest strength range of interest,
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
#' @import rtf lubridate
#' @export
#'
estimateACMT <- function(maindir, rdat="ACMT", ageSp=NULL, region, regArea,
  TSrange=c(-60, -30), psi=0.007997566, soi=c(106, 109, 203, 204),
  spInfo, sliceDef) {

  maindir=mydir
  rdat="ACMT"
  ageSp=NULL
  region=c("nn", "sn", "wn", "no", "so")
  regArea=c(10933, 8716, 6010, 12630, 10487)
  TSrange=c(-60, -30)
  psi=0.007997566
  soi=c(106, 109, 203, 204)
  spInfo=data.frame(
    sp = c(106, 109, 129, 130, 202, 203, 204, 217, 504),
    spname = c("alewife", "bloater", "cisco", "emerald shiner",
      "lake whitefish", "ninespine stickleback", "rainbow smelt",
      "threespine stickleback", "unid coregonine"),
    lcut = c(100, 90, 0, 0, 0, 0, 120, 0, 0),
    lwa = c(1.413254e-05, 4.851203e-06, 3.954044e-05, 3.954044e-05,
      2.214019e-06, 5.3115e-06, 3.434977e-07, 2.214019e-06, 7.916946e-06),
    lwb = c(2.867839, 3.031524, 2.591946, 2.591946,
      3.207, 3.0709, 3.561819, 3.207, 2.968708))
  sliceDef=list(
    nn.epi  = list( fdp=c(-Inf,  40), reg="nn" ),
    nn.hypo = list( fdp=c(  40, Inf), reg="nn" ),
    no.epi  = list( fdp=c(-Inf,  40), reg="no" ),
    no.hypo = list( fdp=c(  40, Inf), reg="no" ),
    sn.epi  = list( fdp=c(-Inf,  40), reg="sn" ),
    sn.hypo = list( fdp=c(  40, Inf), reg="sn" ),
    so.epi  = list( fdp=c(-Inf,  40), reg="so" ),
    so.hypo = list( fdp=c(  40, Inf), reg="so" ),
    wn.epi  = list( fdp=c(-Inf,  40), reg="wn" ),
    wn.hypo = list( fdp=c(  40, Inf), reg="wn" )
  )
  getpkgs(c("class", "rgdal", "RColorBrewer", "survey", "maps", "mapdata",
    "lubridate"))



  # 1.  Initial stuff ####

  load(paste0(maindir, rdat, ".RData"), envir=environment())
  LAKE <- keyvals[1]
  YEAR <- keyvals[2]

  old_options <- options(stringsAsFactors=FALSE, survey.lonely.psu="remove")
  on.exit(options(old_options))

  newrdatname <- paste0("L", LAKE, " Y", YEAR, " ACMT Data.RData")

  # make sure selected lake and year is represented in data provided
  ly <- LAKE %in% optrop$Lake & YEAR %in% optrop$Year
  if(length(ly)<1) stop(paste0("\nNo information from ",
    Lakenames[LAKE], " in ", YEAR, " in RVCAT data.\n\n"))

  # make sure species names are character (not factor)
  spInfo$spname <- as.character(spInfo$spname)

  # create rtf document to save printed output (tables and figures)
  docname <- paste0("L", LAKE, " Y", YEAR, " ACMT Estimate ", today(), ".doc")
  doc <- startrtf(file=docname, dir=maindir)
  heading(paste0(YEAR, " Lake ", Lakenames[LAKE],
    " Estimation from Acoustic and Trawl Data   ", today()))
  para("R code written by Jean Adams for Dave Warner.")
  para(paste0(docname, " = this document."))
  heading("INPUTS", 2)
  para(paste0("maindir = ", maindir, " = main input/output directory."))
  para(paste0("TSrange = ", TSrange[1], " to ", TSrange[2],
    " = TS range of interest."))
  para(paste0("psi = ", psi,
    " = the transducer-specific two-way equivalent beam angle in steradians."))

  if(is.null(ageSp)) {
    para("Ages will NOT be used.")
  } else {
    para("Ages will be used for ", with(spInfo, spname[sp==ageSp]), ".")
  }

  # TODO
  # make sure we have age-length keys for the species that need it
  # if(use.alewife.ages & !("key106" %in% ls()))
  #   warning("\nNo age length key available for alewife.\n\n")


  # 2.  Estimate sigma for each cell using TS frequency dist file ####

  # Sigma is estimated as the mean of the linearized TS (sigma)
  # weighted by the number of targets in each dB bin
  tsbin.colz <- grep("X[[:punct:]]", names(ts))
  db <- -as.numeric(substring(names(ts)[tsbin.colz], 3, 20))
  lin.TS <- 10^(db/10)
  in.range <- db >= TSrange[1] & db <= TSrange[2]
  ts$sigma <- apply(ts[, tsbin.colz[in.range]], 1, function(w)
    weighted.mean(lin.TS[in.range], w))


  # 3.  Merge Sv and sigma data ####

  # use region.interval.layer as unique identifier
  sv$UID <- interaction(gsub(" ", "", sv$Region_name), sv$Interval, sv$Layer)
  sv$source.sv <- sv$source

  ts$UID <- interaction(gsub(" ", "", ts$Region_name), ts$Interval, ts$Layer)
  ts$source.ts <- ts$source

  # merge sv and ts files
  svts <- merge(sv[, c("UID", "Region_name", "Interval", "Layer",
    "Layer_depth_min", "Layer_depth_max", "Lat_M", "Lon_M", "year", "Date_M",
    "Sv_min", "Sv_max", "Sv_mean", "Depth_mean", "PRC_ABC", "source.sv")],
  	ts[, c("UID", "source.ts", "sigma")],
  	by="UID", all=TRUE)

  # get rid of blanks in Region_name
  svts$Region_name <- gsub(" ", "", svts$Region_name)

  # if there are more rows in the merged data frame than in
  # the original sv file, somethings wrong
  if(dim(svts)[1] > dim(sv)[1]) {
  	sel <- is.na(svts$Interval)
  	tab <- ts[ts$UID %in% svts$UID[sel], c("Region_name", "Interval", "Layer",
  	  "source.ts")]
  	tabl("There is at least one region-interval-layer combination that occurs",
      " in the TS data but not in the SV data.",
  		"  These data will be removed from further calculations.")
  	svts <- svts[!sel, ]
  }

  # before making changes to sigma, keep the original value for later reference
  svts$sigma.orig <- svts$sigma

  # assign the value of zero to sigmas where there were no single targets
  # There will be cells without single targets, so not all rows of Sv can
  #   get sigma.
  # I assign these a fish density of zero, because I never have zero targets
  #   because of high-density inability ot detect targets.
  svts$sigma[is.na(svts$sigma)] <- 0


  # 4.  Estimate Nv ####

  svts$n1 <- with(svts, estN1(Sv=Sv_mean, sigma=sigma))
  svts$nv <- with(svts, estNv(psi=psi, R=Depth_mean, n1=n1)


  # 5.  Replace "biased" sigmas where Nv>0.1 with mean "unbiased" sigma ####
  # from cells in the same layer and (if possible) transect

  # calculate mean of "unbiased" sigmas by year-transect-layer
  svts.unbiased <- svts[svts$nv <= 0.1 & !is.na(svts$nv), ]
  tranlay <- aggregate(sigma ~ year + Region_name + Layer, mean,
    data=svts.unbiased)
  names(tranlay)[names(tranlay)=="sigma"] <- "sigunb.tranlay"
  lay <- aggregate(sigma ~ year + Layer, mean, data=svts.unbiased)
  names(lay)[names(lay)=="sigma"] <- "sigunb.lay"

  svts2 <- merge(svts, tranlay, by=c("year", "Region_name", "Layer"), all=TRUE)
  svts3 <- merge(svts2, lay, by=c("year", "Layer"), all=TRUE)

  # if Nv > 0.1 (or Nv is missing), replace sigma with transect-layer mean of
  # unbiased sigma
  svts3$sigma[svts3$nv > 0.1 | is.na(svts3$nv)] <-
    svts3$sigunb.tranlay[svts3$nv > 0.1 | is.na(svts3$nv)]

  # if Nv > 0.1 (or Nv is missing) and there is no transect-layer mean,
  # replace sigma with layer mean of unbiased sigma
  svts3$sigma[
    (svts3$nv > 0.1 | is.na(svts3$nv)) & is.na(svts3$sigunb.tranlay)] <-
  	svts3$sigunb.lay[(svts3$nv > 0.1 | is.na(svts3$nv)) &
    is.na(svts3$sigunb.tranlay)]

  sel <- is.na(svts3$sigma)
  if(sum(sel)>0) {
  	look3 <- svts3[sel, ]
  	tab <- table(look3$Region_name, look3$Layer)
  	tabl("Frequency of observations with missing sigmas by transect (row) and",
      " layer (column).",
  		"  These are layers that had no targets in any transect.",
  		"  They will be removed from further calculations.",
  		"  ")
  	svts3 <- svts3[!sel, ]
  }


  # 6.  Recalculate Nv and estimate density ####

  svts3$n1 <- with(svts3, estN1(Sv=Sv_mean, sigma=sigma))
  svts3$nv <- with(svts3, estNv(psi=psi, R=Depth_mean, n1=n1)
  svts3$fish_ha <- ((svts3$PRC_ABC / svts3$sigma) * 10000)


  # 7.  Add classifiers to acoustic data ####

  # bottom depth range in each interval
  depth.botmin <- aggregate(Layer_depth_min ~ Interval + Region_name, max,
    data=svts3)
  names(depth.botmin)[names(depth.botmin)=="Layer_depth_min"] <- "depth.botmin"
  depth.botmax <- aggregate(Layer_depth_max ~ Interval + Region_name, max,
    data=svts3)
  names(depth.botmax)[names(depth.botmax)=="Layer_depth_max"] <- "depth.botmax"
  depth.bot <- merge(depth.botmin, depth.botmax, all=TRUE)
  svts4 <- merge(svts3, depth.bot, all=TRUE)
  svts4$depth_botmid <- (svts4$depth.botmin + svts4$depth.botmax)/2

  # define slice
  svts5 <- data.frame(svts4,
    slice=sliceCat(sliceDef, fdp=svts4$Depth_mean, bdp=svts4$depth_botmid,
      lat=svts4$Lat_M, lon=svts4$Lon_M, reg=substring(svts4$Region_name, 1, 2)))


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
      lat=optrop$Latitude, lon=optrop$Longitude,
      reg=substring(optrop$Transect, 1, 2)))


  # 9.  Calculate mean proportion and mean weight of catch for trawl data ####

  # summarize trcatch by species and op.id
  trcatch2 <- aggregate(cbind(N, Weight) ~ Op.Id + Species, sum, data=trcatch)

  # estimate weight from length for each fish
  indx <- match(trlf$Species, spInfo$sp)
  trlf$estfw <- spInfo$lwa[indx] * trlf$Length ^ spInfo$lwb[indx]

  # calculate proportion of catch and mean weight for each MT and each
  # species-age-length group

  # determine ages of measured fish first, if necessary
  # TODO
  if(!is.null(ageSp)) {
  	allspsel <- c("106", soi)
  	# create vector of ops for all selected species
  	allops <- aggregate(N ~ Op.Id, sum,
  	  data=trcatch2[trcatch2$Species %in% allspsel, ])$Op.Id
  	# create list for results of all selected species
  	sum.n <- vector("list", length(soi)+1)
  	names(sum.n) <- allspsel
  	mean.w <- sum.n
  	add.sp <- 1
  	# tally up lengths by mmgroup
  	lf106 <- trlf[trlf$Species==106, ]
  	lf106$mmgroup <- 10*round((lf106$Length+5)/10)-5
  	# total count and mean weight
  	g106 <- aggregate(cbind(N, estfw) ~ Op.Id + mmgroup, sum, data=lf106)
  	gkey106 <- merge(g106, key106, all.x=TRUE)
  	# rename ages
  	agecolz <- grep("Age", names(gkey106))
  	names(gkey106)[agecolz] <-
  	  paste0("106.A", substring(names(gkey106)[agecolz], 4, 10))
  	# apply probabilities from key to both counts and weights
  	# total numbers and mean weight by age group
  	tot.n <- apply(gkey106$N * gkey106[, agecolz], 2,
  	  tapply, gkey106$Op.Id, sum)
  	m.w <- apply(gkey106$estfw * gkey106[, agecolz], 2,
  	  tapply, gkey106$Op.Id, sum)/tot.n
  	tidyup <- function(x, uniq) {
  		y <- x[match(uniq, dimnames(x)[[1]]), , drop=FALSE]
  		dimnames(y)[[1]] <- uniq
  		y[is.na(y)] <- 0
  		y[, apply(y, 2, sum)>0]
  	}
  	sum.n[[1]] <- tidyup(tot.n, allops)
  	mean.w[[1]] <- tidyup(m.w, allops)
  } else {
  	allspsel <- soi
  	# create vector of ops for all selected species
  	allops <- aggregate(N ~ Op.Id, sum,
  	  data=trcatch2[trcatch2$Species %in% allspsel, ])$Op.Id
  	# create list for results of all selected species
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

  # TODO ---- (maybe I did this already?  I dunno ... used to say "fix" here)

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
    		"  The locations of these tows are highlighted in Figure 1.")
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
    		"  The locations of these tows are highlighted in Figure 1.")
    	mtops <- if(exists("mtops")) c(mtops, names(propother)[sel]) else
    	  names(propother)[sel]
    }
  }

  # bring together total counts and mean weights
  counts <- do.call(cbind, sum.n)
  mnwts <- do.call(cbind, mean.w)

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
  # convert from lat/long to UTM
  MTutm <- with(opsub, latlon2utm(Longitude, Latitude))
  ACutm <- with(svts5, latlon2utm(Lon_M, Lat_M))
  # unique slice in AC and MT data
  sus <- names(sliceDef)

  # determine nearest trawl
  svts5$nearmt <- NA
  for(i in seq(sus)) {
  	# select records from the selected slice
  	# exclude any records with missing slice or missing lat/long info
  	selm <- opsub$slice==sus[i] & !is.na(opsub$slice) &
  	  !apply(is.na(MTutm), 1, any)
  	sela <- svts5$slice==sus[i] & !is.na(svts5$slice)&
  	  !apply(is.na(ACutm), 1, any)
  	# determine the nearest MT
  	if(sum(selm)) {
  		if(sum(selm) > 1) {
  			svts5$nearmt[sela] <- as.numeric(as.character(knn1(MTutm[selm, ],
  			  ACutm[sela, ], allops[selm])))
  		} else {
  			svts5$nearmt[sela] <- allops[selm]
  		}
  	}
  }









#### TODO #### this is where I left off ####

# plot of apportionment ####

# assign colors so that like colors are geographically separated
loc <- cmdscale(dist(opsub[, c("Latitude", "Longitude")]), k=1)
separate <- rep(1:3, length.out=length(loc))

colz1 <- rain.n(1:(dim(opsub)[1]),
  n=dim(opsub)[1], start=2/6, end=6/6)[order(loc)[order(separate)]]
colz2 <- recode(svts5$nearmt, opsub$Op.Id, colz1)

mf <- rev(n2mfrow(length(sliceDef)))
if(LAKE==3 & length(sliceDef)==3) mf <- c(2, 2)
iord <- 1:length(sliceDef)

fig <- function() {
	par(mfrow=mf)
	for(i in iord) {
		selm <- opsub$slice==sus[i] & !is.na(opsub$slice)
		map("worldHires", xlim=range(opsub$Longitude, svts5$Lon_M, na.rm=TRUE),
		  ylim=range(opsub$Latitude, svts5$Lat_M, na.rm=TRUE),
			mar=c(0, 0, 2.5, 0), col="gray")
		box(col="gray")
		if(sum(selm)>0) {
			lowhigh <- if(!exists("mtops")) 1 else ((opsub$Op.Id[selm] %in% mtops)
			  + 1)
			par(xpd=NA)
			text(opsub$Longitude[selm], opsub$Latitude[selm], opsub$Op.Id[selm],
			  col=colz1[selm], cex=lowhigh, font=lowhigh)
			par(xpd=FALSE)
			mtext(sus[i], side=3)
		} else {
			mtext(paste(sus[i], "- No Tows"), side=3, col="brown")
		}
	}
}
figu("Location of midwater trawl hauls in slices.",
	"  Numbers identify the OP_ID of each tow.  Colors are the same as",
  " in the next figure.",
	"  Tows with > 10% of their catch (by number or weight) in 'other' species",
  " are shown in large, bold font.", h=8, newpage="port")

fig <- function() {
	par(mfrow=mf)
	for(i in iord) {
		selm <- opsub$slice==sus[i] & !is.na(opsub$slice)
		sela <- svts5$slice==sus[i] & !is.na(svts5$slice)
		map("worldHires", xlim=range(opsub$Longitude, svts5$Lon_M, na.rm=TRUE),
		  ylim=range(opsub$Latitude, svts5$Lat_M, na.rm=TRUE),
			mar=c(0, 0, 2.5, 0), col="gray")
		box(col="gray")
		if(sum(selm)>0) {
			points(svts5$Lon_M[sela], svts5$Lat_M[sela], col=colz2[sela], pch=3)
			# add convex hull for each trawl haul
			sut <- sort(unique(svts5$nearmt[sela]))
			for(j in seq(along=sut)) {
				selz <- sela & svts5$nearmt==sut[j]
				hpts <- chull(svts5$Lon_M[selz], svts5$Lat_M[selz])
				hpts <- c(hpts, hpts[1])
				lines(svts5$Lon_M[selz][hpts], svts5$Lat_M[selz][hpts], lty=3)
			}
			mtext(sus[i], side=3)
			points(opsub$Longitude[selm], opsub$Latitude[selm], pch=16,
			  col=colz1[selm], cex=2)
			points(opsub$Longitude[selm], opsub$Latitude[selm], pch=16, cex=1.5)
			points(opsub$Longitude[selm], opsub$Latitude[selm], pch=16,
			  col="white", cex=1)
		} else {
			points(svts5$Lon_M[sela], svts5$Lat_M[sela], col="brown", pch=4)
			mtext(paste(sus[i], "- No Trawls"), side=3, col="brown")
		}
	}
}
figu("Apportionment using slices.",
	"  Each MT tow is shown as a white circle (o).",
	"  Each AC interval is shown as a colored plus sign (+).",
	"  Dotted lines encircle all the AC intervals (given the same color) that",
  " used each MT tow for apportionment.", h=8, newpage="port")

# plot of AC and MT data by slice ####
if(LAKE==2) {
	orient <- "port"
} else {
	orient <- "land"
}

svts5$slicecol <- match(svts5$slice, sus)
opsub$slicecol <- match(opsub$slice, sus)

fig <- function() {
	par(mfrow=c(length(sus), 2), mar=c(0, 0, 3, 3), oma=c(1.5, 2, 1.5, 2))
	for(i in iord) {
		# plot AC data
		sel <- svts5$slice==sus[i]
		plot(1, 1, xlim=range(svts5$depth_botmid, opsub$depth_botmid, na.rm=TRUE),
		  ylim=range(-svts5$Depth_mean, -opsub$Fishing_Depth, na.rm=TRUE), type="n",
		  xlab="", ylab="", axes=FALSE)
		abline(0, -1, col="gray")
		points(jitter(svts5$depth_botmid)[sel], -jitter(svts5$Depth_mean)[sel],
		  col=svts5$slicecol[sel])
		axis(3, col="gray", col.axis="gray", cex.axis=1.5)
		axis(4, las=1, at=axTicks(4), labels=-axTicks(4), col="gray",
		  col.axis="gray", cex.axis=1.5)
		box(bty="7", col="gray")
		mtext(sus[i], side=2, cex=1.2)
		if(i==1) mtext("AC", side=3, line=2.5, cex=1.2)
		# plot MT data
		sel2 <- opsub$slice==sus[i]
		plot(1, 1, xlim=range(svts5$depth_botmid, opsub$depth_botmid, na.rm=TRUE),
		  ylim=range(-svts5$Depth_mean, -opsub$Fishing_Depth, na.rm=TRUE), type="n",
		  xlab="", ylab="", axes=FALSE)
		abline(0, -1, col="gray")
		points(opsub$depth_botmid[sel2], -opsub$Fishing_Depth[sel2],
		  col=opsub$slicecol[sel2], lwd=2, cex=2)
		axis(3, col="gray", col.axis="gray", cex.axis=1.5)
		axis(4, las=1, at=axTicks(4), labels=-axTicks(4), col="gray",
		  col.axis="gray", cex.axis=1.5)
		box(bty="7", col="gray")
		if(i==1) mtext("MT", side=3, line=2.5, cex=1.2)
		if(i==2) mtext("Bottom depth  (m)", side=3, line=2.5, col="darkgray",
		  cex=1.2)
	}
	mtext("Water depth  (m)", side=4, outer=TRUE, line=0.5, col="darkgray",
	  cex=1.2)
	# levels in AC that are NOT in MT
	misslev <- sus[!(sus %in% sus)]
	if(length(misslev)>0) {
		mtext(paste("Slices not sampled by midwater trawls:",
		  paste(misslev, collapse=", ")), side=1, outer=TRUE)
		warning(paste("\nSlices not sampled by midwater trawls:",
		  paste(misslev, collapse=", "), "\n\n"))
	}
}
figu("Acoustic (left) and midwater trawl (right) data by slice.",
  newpage=orient)




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

rcol <- as.numeric(as.factor(svts5$region))
fig <- function() {
	map("worldHires", xlim=range(svts5$Lon_M, na.rm=TRUE) + 0.1*c(-1, 1),
	  ylim=range(svts5$Lat_M, na.rm=TRUE) + 0.1*c(-1, 1), mar=c(0, 0, 0, 0),
	  col="gray")
	points(svts5$Lon_M, svts5$Lat_M, col=rcol)
	text(tapply(svts5$Lon_M, svts5$region, mean), tapply(svts5$Lat_M,
	  svts5$region, mean), names(tapply(svts5$Lon_M, svts5$region, mean)), cex=2,
		col=tapply(rcol, svts5$region, mean))
}
figu("Acoustic transect data, color coded by design-based strata.",
  newpage="port")
look <- tapply(svts5$Region_name, svts5$region, function(x) sort(unique(x)))
if(sum(sapply(look, length) < 2)) {
	tab <- cbind(names(look), sapply(look, paste, collapse=", "))
	tabl("Only one transect in at least one region.  Variance will be estimated",
    " with this region(s) removed.")
}



# 12. Generate estimates for the species groups. ####

# apply species group proportions to AC densities
nph <- svts5$fish_ha * nprops[match(svts5$nearmt, allops), ]
gph <- nph * mnwts[match(svts5$nearmt, allops), ]

# summary of density by interval (summed densities over layers)
nph.int <- aggregate(nph ~ region + regarea + Region_name + Interval +
    depth_botmid + Lat_M + Lon_M, sum, data=svts5)
names(nph.int)[is.na(names(nph.int))] <- sp.grps
gph.int <- aggregate(gph ~ region + regarea + Region_name + Interval +
    depth_botmid + Lat_M + Lon_M, sum, data=svts5)
names(gph.int)[is.na(names(gph.int))] <- sp.grps


nph.int.domain <- aggregate(nph ~ region + regarea + Region_name + Interval +
    depth_botmid + Lat_M + Lon_M + slice, sum, data=svts5)
names(nph.int.domain)[is.na(names(nph.int.domain))] <- sp.grps
gph.int.domain <- aggregate(gph ~ region + regarea + Region_name + Interval +
    depth_botmid + Lat_M + Lon_M + slice, sum, data=svts5)
names(gph.int.domain)[is.na(names(gph.int.domain))] <- sp.grps

plotbygrp <- function(xph.int) {
	# come up with break points that divide the nonzero data into
  # 7 groups on a log scale
	v <- unlist(xph.int[, match(sp.grps, names(xph.int))])
	v2 <- v[v>0]
	mybrks <- 10^quantile(log10(v2), seq(0, 1, length=8))
	symsize <- seq(0.5, 2.5, length=7)
	npanels <- length(grp.sp) + length(unique(paste(grp.sp, grp.type))) - 1
	if(LAKE==2) {
		nrows <- 3
		ncols <- ceiling(npanels/3)
	} else {
		nrows <- 4
		ncols <- ceiling(npanels/4)
	}
	par(mfrow=c(nrows, ncols), mar=c(0, 0, 3, 0))
	for(i in seq(sp.grps)) {
		if(i>1) if(grp.sp[i]!=grp.sp[i-1] | grp.type[i]!=grp.type[i-1]) frame()
		selcol <- match(sp.grps[i], names(xph.int))
		selrow <- xph.int[, selcol] > 0
		quant9 <- as.numeric(cut(xph.int[selrow, selcol], breaks=mybrks,
		  include.lowest=TRUE))
		map("worldHires", xlim=range(xph.int$Lon_M) + 0.1*c(-1, 1),
		  ylim=range(xph.int$Lat_M) + 0.1*c(-1, 1), mar=c(0, 0, 3, 0), col="gray")
		mtext(sp.grps[i], side=3)
		points(xph.int$Lon_M[selrow], xph.int$Lat_M[selrow], cex=symsize[quant9],
		  col=mypalette[quant9])
	}
}
# a palette of 7 colors for non-zero data
mypalette <- brewer.pal(9, "GnBu")[-(1:2)]
fig <- function() plotbygrp(xph.int=nph.int)
figu("Acoustic density for each species group.  Groups are defined by",
  " length cut offs (L) in mm or ages (A).",
	"  Darker and larger circles indicate higher density.", newpage="port")
fig <- function() plotbygrp(xph.int=gph.int)
figu("Acoustic biomass for each species group.  Groups are defined by",
  " length cut offs (L) in mm or ages (A).",
	"  Darker and larger circles indicate greater biomass.", newpage="port")




# 13. Calculate lakewide totals based on stratified cluster sampling design ####

# stratified cluster design ... regions are strata, transects are clusters
# (nested in regions)
SCD.n <- svydesign(id=~Region_name, strata=~region,
  variables=nph.int[, grep("\\.", names(nph.int))], data=nph.int, nest=TRUE,
	weights=~regarea)
SCD.n2 <- as.data.frame(svytotal(as.matrix(
  nph.int[, grep("\\.", names(nph.int))]/1000000), SCD.n))
SCD.n2ph <- as.data.frame(svymean(as.matrix(
  nph.int[, grep("\\.", names(nph.int))]), SCD.n))

SCD.g <- svydesign(id=~Region_name, strata=~region,
  variables=gph.int[, grep("\\.", names(gph.int))], data=gph.int, nest=TRUE,
	weights=~regarea)
SCD.g2 <- as.data.frame(svytotal(as.matrix(
  gph.int[, grep("\\.", names(gph.int))]/1000000), SCD.g))
SCD.g2ph <- as.data.frame(svymean(as.matrix(
  gph.int[, grep("\\.", names(gph.int))]), SCD.g))

# summarize by the new "slices" (domains) ... ignore old "strata",
domainest <- function(dat, type="total") {
	d <- NA
	if(dim(dat)[1]>0) {
    scd <- svydesign(id=~Region_name, strata=~region, data=dat, nest=TRUE,
      weights=~regarea)
    varnames <- grep("\\.", names(nph.int.domain), value=TRUE)
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

SCD.n.d2 <- domainest(nph.int.domain, type="total")
SCD.n.d2ph <- domainest(nph.int.domain, type="mean")
SCD.g.d2 <- domainest(gph.int.domain, type="total")
SCD.g.d2ph <- domainest(gph.int.domain, type="mean")

# combine information
laketots.n <- cbind(SCD.n.d2, SCD.n2, rse=100*SCD.n2$SE / SCD.n2$total)
lakemeans.n <- cbind(SCD.n.d2ph, SCD.n2ph, rse=100*SCD.n2ph$SE / SCD.n2ph$mean)
laketots.g <- cbind(SCD.g.d2, SCD.g2, rse=100*SCD.g2$SE / SCD.g2$total)
lakemeans.g <- cbind(SCD.g.d2ph, SCD.g2ph, rse=100*SCD.g2ph$SE / SCD.g2ph$mean)

# Save estimates to csv files
save2csv <- c("millions", "nph", "t", "gph", "nphint", "gphint")
outfiles <- paste0(maindir, "L", LAKE, " Y", YEAR, " ACMT Estimates ",
  save2csv, " ", today(), ".csv")
write.csv(laketots.n, outfiles[1])
write.csv(lakemeans.n, outfiles[2])
write.csv(laketots.g, outfiles[3])
write.csv(lakemeans.g, outfiles[4])
write.csv(nph.int, outfiles[5])
write.csv(gph.int, outfiles[6])


mypalette <- brewer.pal(6, "Set3")
fig <- function() {
	par(mar=c(4, 5, 0, 1), oma=c(0, 0, 2, 0), mfrow=c(1, 2), cex=1.2)
	barplot(t(as.matrix(laketots.n[, 1:(length(laketots.n)-3)])), col=mypalette,
	  horiz=TRUE, las=1, xlab="Number of fish  (millions)")
	barplot(t(as.matrix(laketots.g[, 1:(length(laketots.g)-3)])), col=mypalette,
	  horiz=TRUE, las=1, xlab="Biomass of fish  (t)",
		legend.text=TRUE, args.legend=list(x="topright"))
}
figu("Acoustic survey lakewide estimates in number (left) and biomass (right)",
  " for each species group.",
	"  Groups are defined by length cut offs (L) in mm or ages (A).",
	"  Colors are used to identify contributions from different slices.",
  h=5.8, w=9, newpage="land")

# numbers in millions
tab <- format(round(laketots.n), big.mark=",")
tabl("Lakewide estimates in number (millions) for each species group and",
  " slice.",
	"  Groups are defined by length cut offs (L) in mm or ages (A).")

# biomass in metric tons (t)
tab <- format(round(laketots.g), big.mark=",")
tabl("Lakewide biomass estimates (t) for each species group and slice.",
	"  Groups are defined by length cut offs (L) in mm or ages (A).")



endrtf()
