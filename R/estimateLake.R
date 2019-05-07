estimateLake <-
function (maindir, rdat = "ACMT", ageSp = NULL, region, regArea,
          TSrange = c(-60, -30), TSthresh = 1, psi = NULL, chngBinCntToZero =FALSE,
          BinCntZeroParams =c(depth = 40,TS = -45), SpeciesFromDepthTS = FALSE,
          DepthTSParams = c(SpDepth = 40, SpTS = -45, Species = 204 ), rmBycatch = FALSE,
          ByCatchParams = c(bcdepth = 40, bcSpecies = c(106, 109)),
          soi = c(106,109, 203, 204), spInfo, sliceDef, short = TRUE, descr = "ACMT Estimates")
{
  load(paste0(maindir, rdat, ".RData"), envir = environment())
  LAKE <- keyvals[1]
  YEAR <- keyvals[2]
  old_options <- options(stringsAsFactors = FALSE, survey.lonely.psu = "remove")
  on.exit(options(old_options))
  ly <- LAKE %in% optrop$Lake & YEAR %in% optrop$Year
  if (length(ly) < 1)
    stop(paste0("\nNo information from ", Lakenames[LAKE],
                " in ", YEAR, " in RVCAT data.\n\n"))
  spInfo$spname <- as.character(spInfo$spname)
  xtra <- setdiff(soi, spInfo$sp)

  PsiReader()

  if (length(xtra) > 0)
    stop(paste0("\nThere is at least one species listed in soi= that has no information in spInfo=: ",
                paste(xtra, collapse = ", "), "."))
  docname <- paste0("L", LAKE, " Y", YEAR, " ", descr, " ",
                    lubridate::today(), ".doc")
  doc <<- startrtf(file = docname, dir = maindir)
  heading(paste0(YEAR, " Lake ", Lakenames[LAKE], " Estimation from Acoustic and Trawl Data   ",
                 lubridate::today()))
  para("Created using the R package EchoNet2Fish (https://github.com/JVAdams/EchoNet2Fish), written by Jean V. Adams for Dave Warner.")
  para(paste0(docname, " = this document."))
  heading("INPUTS", 2)
  para(paste0("maindir = ", maindir, " = main input/output directory."))
  para(paste0("TSrange = ", TSrange[1], " to ", TSrange[2],
              " = TS range of interest."))
  para(paste0("TSthresh = ", TSthresh, " = minimum threshold for binned targets in a cell."))
  para(paste0("psi = ", psi, " = the transducer-specific two-way equivalent beam angle in steradians."))
  if (is.null(ageSp)) {
    para("Ages will NOT be used.")
  }
  else {
    ageSp <- sort(ageSp)
    para("Ages will be used for ", paste(with(spInfo, spname[sp %in%
                                                               ageSp]), collapse = ", "), ".")
    if (exists("key1")) {
      if (exists("key2")) {
        ageksp <- c(key1$sp, key2$sp)
        agekey <- list(key1, key2)
        sdf <- setdiff(ageSp, ageksp)
        if (length(sdf) > 0)
          stop("No age-length key available for ", sdf)
      }
      else {
        ageksp <- key1$sp
        agekey <- list(key1)
        sdf <- setdiff(ageSp, ageksp)
        if (length(sdf) > 0)
          stop("No age-length key available for ", sdf)
      }
    }
    else {
      stop("No age-length key(s) available.")
    }
  }
  TS.range.abs <- abs(TSrange)
  ts.names <- paste0("X.", seq(TS.range.abs[[1]], TS.range.abs[[2]], -1))
  ts$ts.range.binned <- rowSums(ts[,ts.names])
  ts$sigma <- sigmaAvg(TSdf = ts, TSrange = TSrange)
  ts <- subset(ts, ts.range.binned > TSthresh)

  #if mean TS is < -45 and deeperwe set # of targets in each bin to zero
  # Replace the # of targets binned with zero
  if(chngBinCntToZero == TRUE) ts[which(ts$Layer_depth_min >= 40 & ts$sigma < 3.162278e-05),
              ts %in% c(ts.names)] <- 0

  #This section asks the user to enter psi for as many different transducers are being use.


  sv$UID <- interaction(gsub(" ", "", sv$Region_name), sv$Interval,
                        sv$Layer)
  sv$source.sv <- sv$source
  ts$UID <- interaction(gsub(" ", "", ts$Region_name), ts$Interval,
                        ts$Layer)
  ts$source.ts <- ts$source
  svdup <- sv[sv$UID %in% sv$UID[duplicated(sv$UID)], ]
  tsdup <- ts[ts$UID %in% ts$UID[duplicated(ts$UID)], ]
  if (dim(svdup)[1] > 0) {
    print(svdup[, c("Region_name", "Interval", "Layer", "source.sv")])
    stop("There should be only one row in SV for each Region_name, Interval, and Layer.")
  }
  if (dim(tsdup)[1] > 0) {
    print(tsdup[, c("Region_name", "Interval", "Layer", "source.ts")])
    stop("There should be only one row in TS for each Region_name, Interval, and Layer.")
  }
  svts <- merge(sv[, c("UID", "Region_name", "Interval", "Layer",
                       "Layer_depth_min", "Layer_depth_max", "Lat_M", "Lon_M",
                       "year", "Date_M", "Sv_mean", "Depth_mean", "PRC_ABC",
                       "source.sv")], ts[, c("UID", "source.ts", "sigma")],
                by = "UID", all = TRUE)
  svts$Region_name <- gsub(" ", "", svts$Region_name)
  svx <- setdiff(sv$UID, ts$UID)
  tsx <- setdiff(ts$UID, sv$UID)
  if (length(tsx) > 0) {
    sel <- svts$UID %in% tsx
    tab <- svts[sel, c("UID", "sigma", "source.ts")]
    tabl("There is at least one region-interval-layer combination that occurs",
         " in the TS data but not in the SV data.", "  These data will be removed from further calculations.",
         TAB = tab)
    svts <- svts[!sel, ]
  }
  svts$sigma.orig <- svts$sigma
  svts$n1 <- with(svts, estrhov(Sv = Sv_mean, sigma = sigma))
  svts$nv <- with(svts, estNv(psi = psi, R = Depth_mean, rhov = n1))
  laymid <- with(svts, -(Layer_depth_min + Layer_depth_max)/2)
  lat.r <- with(svts, tapply(Lat_M, Region_name, mean, na.rm = TRUE))
  Region_ord <- names(lat.r)[order(lat.r, decreasing = T)]
  fig <- function(x, xname) {
    with(svts, plotIntLay(Interval, laymid, Region_name,
                          Region_ord, colorVal(x), paste0("Colors indicate ",
                                                          xname)))
  }
  prefix <- "Interval by layer plots for Sv TS files.  Colors indicate "
  figu(prefix, "Nv", FIG = function() fig(svts$nv, "Nv"), newpage = "port")
  svts$sigma <- replaceBiasedSigma(df = svts, varNv = "nv",
                                   varsigmabs = "sigma", varTranLay = c("Region_name", "Layer",
                                                                        "year"))
  svts$sigma[is.na(svts$sigma)] <- 0
  svts$n1 <- with(svts, estrhov(Sv = Sv_mean, sigma = sigma))
  svts$nv <- with(svts, estNv(psi = psi, R = Depth_mean, rhov = n1))
  svts$fish_ha <- with(svts, paDens(sigma, PRC_ABC, hectare = TRUE))
  depth.botmin <- aggregate(Layer_depth_min ~ Interval + Region_name,
                            max, data = svts)
  names(depth.botmin)[names(depth.botmin) == "Layer_depth_min"] <- "depth.botmin"
  depth.botmax <- aggregate(Layer_depth_max ~ Interval + Region_name,
                            max, data = svts)
  names(depth.botmax)[names(depth.botmax) == "Layer_depth_max"] <- "depth.botmax"
  depth.bot <- merge(depth.botmin, depth.botmax, all = TRUE)
  svts4 <- merge(svts, depth.bot, all = TRUE)
  svts4$depth_botmid <- (svts4$depth.botmin + svts4$depth.botmax)/2
  svts5 <- data.frame(svts4, slice = sliceCat(sliceDef, fdp = svts4$Depth_mean,
                                              bdp = svts4$depth_botmid, lon = svts4$Lon_M, lat = svts4$Lat_M,
                                              reg = substring(svts4$Region_name, 1, 2)))
  optrop$depth.botmin <- 10 * floor(pmin(optrop$Beg.Depth,
                                         optrop$End.Depth)/10)
  optrop$depth.botmax <- 10 * ceiling(pmax(optrop$Beg.Depth,
                                           optrop$End.Depth)/10)
  optrop$depth_botmid <- (optrop$Beg.Depth + optrop$End.Depth)/2
  overallmaxdep <- max(depth.bot$depth.botmax, optrop$depth.botmax,
                       na.rm = TRUE) + 10
  optrop$layer <- cut(optrop$Fishing_Depth, seq(0, overallmaxdep,
                                                10), right = FALSE)
  optrop <- data.frame(optrop, slice = sliceCat(sliceDef, fdp = optrop$Fishing_Depth,
                                                bdp = optrop$depth_botmid, lon = optrop$Longitude, lat = optrop$Latitude,
                                                reg = substring(optrop$Transect, 1, 2)))
  trcatch2 <- aggregate(cbind(N, Weight) ~ Op.Id + Species,
                        sum, data = trcatch)
  trlf2 <- aggregate(N ~ Op.Id + Species, sum, data = trlf)
  look <- trcatch2 %>% full_join(trlf2, by = c("Op.Id", "Species"),
                                 suffix = c(".caught", ".measured")) %>% mutate(scaleup = N.caught/N.measured)
  warnsub <- filter(look, scaleup < 1)
  if (dim(warnsub)[1] > 0) {
    cat("\n\n")
    warning("There was at least one case where the number of fish captured was LESS THAN the number of fish measured.")
    print(select(warnsub, -scaleup))
    cat("\n\n")
  }
  trlf3 <- trlf %>% left_join(select(look, Op.Id, Species,
                                     scaleup), by = c("Op.Id", "Species")) %>% mutate(N.scaled = N *
                                                                                        scaleup)
  indx <- match(trlf3$Species, spInfo$sp)
  trlf3$estwal <- estWeight(trlf3$Length, spInfo$lwa[indx],
                            spInfo$lwb[indx])
  trlf3$estfw <- trlf3$estwal * trlf3$N.scaled
  if (!is.null(ageSp)) {
    allspsel <- c(ageSp, soi)
    allops <- sort(unique(optrop$Op.Id))
    sum.n <- vector("list", length(allspsel))
    names(sum.n) <- allspsel
    mean.w <- sum.n
    add.sp <- length(ageSp)
    tidyup <- function(x, uniq) {
      y <- x[match(uniq, dimnames(x)[[1]]), , drop = FALSE]
      dimnames(y)[[1]] <- uniq
      y[is.na(y)] <- 0
      y[, apply(y, 2, sum) > 0]
    }
    for (i in seq_along(ageSp)) {
      lfa <- trlf3[trlf3$Species %in% ageSp[i], ]
      lfa$mmgroup <- 10 * round((lfa$Length + 5)/10) -
        5
      ga <- aggregate(cbind(N.scaled, estfw) ~ Op.Id +
                        mmgroup, sum, data = lfa)
      gkeya <- merge(ga, agekey[[i]], all.x = TRUE)
      agecolz <- grep("Age", names(gkeya))
      names(gkeya)[agecolz] <- paste0(ageSp[i], ".A", substring(names(gkeya)[agecolz],
                                                                4, 10))
      tot.n <- apply(gkeya$N.scaled * gkeya[, agecolz],
                     2, tapply, gkeya$Op.Id, sum)
      m.w <- apply(gkeya$estfw * gkeya[, agecolz], 2, tapply,
                   gkeya$Op.Id, sum)/tot.n
      sum.n[[i]] <- tidyup(tot.n, allops)
      mean.w[[i]] <- tidyup(m.w, allops)
    }
  }
  else {
    allspsel <- soi
    allops <- sort(unique(optrop$Op.Id))
    sum.n <- vector("list", length(soi))
    names(sum.n) <- soi
    mean.w <- sum.n
    add.sp <- 0
  }
  tidyup2 <- function(x, uniqops, uniqlens) {
    m <- array(0, dim = c(length(uniqops), length(lclong)),
               dimnames = list(uniqops, paste0(sp, ".L", lclong)))
    if (dim(x)[1] > 0) {
      m[match(dimnames(x)[[1]], uniqops), match(dimnames(x)[[2]],
                                                uniqlens)] <- x
    }
    m
  }
  allops <- sort(unique(optrop$Op.Id))
  for (i in seq(soi)) {
    sp <- soi[i]
    lc <- spInfo$lcut[spInfo$sp == sp]
    lclong <- unique(c(0, lc))
    lf <- trlf3[trlf3$Species == sp, ]
    lf$mmgroup <- lc * (lf$Length > lc)
    tot.n <- tapply(lf$N.scaled, list(lf$Op.Id, lf$mmgroup),
                    sum)
    tot.n[is.na(tot.n)] <- 0
    m.w <- tapply(lf$estfw, list(lf$Op.Id, lf$mmgroup), sum)/tot.n
    m.w[is.na(m.w)] <- 0
    sum.n[[add.sp + i]] <- tidyup2(tot.n, allops, lclong)
    mean.w[[add.sp + i]] <- tidyup2(m.w, allops, lclong)
  }
  if (length(setdiff(unique(trcatch2$Species), soi)) > 0) {
    sumbyspec <- tapply(trcatch2$N, list(trcatch2$Op.Id,
                                         trcatch2$Species %in% soi), sum)
    sumbyspec[is.na(sumbyspec)] <- 0
    propother <- sumbyspec[, 1]/apply(sumbyspec, 1, sum)
    sel <- propother > 0.1 & !is.na(propother)
    if (sum(sel) > 0) {
      look <- trcatch2[trcatch2$Op.Id %in% names(propother)[sel] &
                         !(trcatch2$Species %in% soi), ]
      tab <- look[order(look$Op.Id, -look$N, look$Species),
                  c("Op.Id", "Species", "N", "Weight")]
      tabl("Species other than those selected (", paste(soi,
                                                        collapse = ", "), ") are ignored when calculating proportions, but other species make up",
           " > 10% of the *NUMBER* in at least one trawl haul.",
           "  The locations of these tows are highlighted in Figure 1.",
           TAB = tab)
      mtops <- names(propother)[sel]
    }
    sumbyspec <- tapply(trcatch2$Weight, list(trcatch2$Op.Id,
                                              trcatch2$Species %in% soi), sum)
    sumbyspec[is.na(sumbyspec)] <- 0
    propother <- sumbyspec[, 1]/apply(sumbyspec, 1, sum)
    sel <- propother > 0.1 & !is.na(propother)
    if (sum(sel) > 0) {
      look <- trcatch2[trcatch2$Op.Id %in% names(propother)[sel] &
                         !(trcatch2$Species %in% soi), ]
      tab <- look[order(look$Op.Id, -look$Weight, look$Species),
                  c("Op.Id", "Species", "N", "Weight")]
      tabl("Species other than those selected (", paste(soi,
                                                        collapse = ", "), ") are ignored when calculating proportions, but other species make up",
           " > 10% of the *WEIGHT* in at least one trawl haul.",
           "  The locations of these tows are highlighted in Figure 1.",
           TAB = tab)
      mtops <- if (exists("mtops"))
        c(mtops, names(propother)[sel])
      else names(propother)[sel]
    }
  }
  ord <- order(names(sum.n))
  counts <- do.call(cbind, sum.n[ord])
  mnwts <- do.call(cbind, mean.w[ord])
  sp.grps <- dimnames(counts)[[2]]
  grp.sp <- sapply(strsplit(sp.grps, "\\."), "[", 1)
  grp.type <- substring(sapply(strsplit(sp.grps, "\\."), "[",
                               2), 1, 1)
  if (!is.null(ageSp) & any(sapply(ageSp, function(x) x %in%
                                   soi))) {
    sum.counts <- apply(counts[, grp.type == "L"], 1, sum)
  }
  else {
    sum.counts <- apply(counts, 1, sum)
  }
  nprops <- sweep(counts, 1, sum.counts, "/")
  nprops[is.na(nprops)] <- 0
  opsub <- optrop[match(allops, optrop$Op.Id), ]
  MTutm <- with(opsub, latlon2utm(Longitude, Latitude))
  ACutm <- with(svts5, latlon2utm(Lon_M, Lat_M))
  sus <- names(sliceDef)
  svts5$nearmt <- NA
  for (i in seq(sus)) {
    selm <- opsub$slice == sus[i] & !is.na(opsub$slice) &
      !apply(is.na(MTutm), 1, any)
    sela <- svts5$slice == sus[i] & !is.na(svts5$slice) &
      !apply(is.na(ACutm), 1, any)
    if (sum(selm)) {
      if (sum(selm) > 1) {
        tempx <- as.character(class::knn1(MTutm[selm,
                                                ], ACutm[sela, ], allops[selm]))
        if (sum(grepl("^[0-9]+$", tempx)) > 0) {
          svts5$nearmt[sela] <- as.numeric(tempx)
        }
        else {
          svts5$nearmt[sela] <- tempx
        }
      }
      else {
        svts5$nearmt[sela] <- allops[selm]
      }
    }
  }
  fig <- function() {
    with(opsub, mapMulti(bygroup = slice, sug = names(sliceDef),
                         plottext = TRUE, ID = Op.Id, short = short, lon = Longitude,
                         lat = Latitude, rlon = range(Longitude, svts5$Lon_M,
                                                      na.rm = TRUE), rlat = range(Latitude, svts5$Lat_M,
                                                                                  na.rm = TRUE), misstext = " - No tows"))
  }
  figu("Location of midwater trawl hauls in slices.", "  Numbers identify the OP_ID of each tow.  Colors are the same as",
       " in the next figure.", "  Tows with > 10% of their catch (by number or weight) in 'other' species",
       " are shown in large, bold font.", FIG = fig, h = 8,
       newpage = "port")
  fig <- function() {
    mapAppor(MTgroup = opsub$slice, ACgroup = svts5$slice,
             sug = names(sliceDef), MTID = opsub$Op.Id, ACID = svts5$nearmt,
             short = short, MTlon = opsub$Longitude, MTlat = opsub$Latitude,
             AClon = svts5$Lon_M, AClat = svts5$Lat_M, misstext = " - No tows")
  }
  figu("Apportionment using slices.", "  Each MT tow is shown as a white circle (o).",
       "  Each AC interval is shown as a colored plus sign (+).",
       "  Dotted lines encircle all the AC intervals (given the same color) that",
       " used each MT tow for apportionment.", FIG = fig, h = 8,
       newpage = "port")
  if (length(unique(c(opsub$slice, ACgroup = svts5$slice))) >
      4) {
    orient <- "port"
  }
  else {
    orient <- "land"
  }
  fig <- function() {
    plotACMTslice(MTgroup = opsub$slice, ACgroup = svts5$slice,
                  MTbd = opsub$depth_botmid, ACbd = svts5$depth_botmid,
                  MTwd = opsub$Fishing_Depth, ACwd = svts5$Depth_mean)
  }
  figu("Acoustic (left) and midwater trawl (right) data by slice.",
       FIG = fig, newpage = orient)
  svts5$region <- substring(svts5$Region_name, 1, 2)
  svts5$regarea <- regArea[match(svts5$region, region)]
  sur <- sort(unique(svts5$region))
  if (!identical(sort(region), sur))
    warning(paste0("\nStrata used in laying out the sampling design (",
                   paste(sort(region), collapse = ", "), ") do not match up with the strata actually sampled (",
                   paste(sur, collapse = ", "), ").\n\n"))
  if (short) {
    orient <- "land"
  }
  else {
    orient <- "port"
  }
  fig <- function() {
    mapByGroup(bygroup = svts5$region, lon = svts5$Lon_M,
               lat = svts5$Lat_M)
  }
  figu("Acoustic transect data, color coded by design-based strata.",
       FIG = fig, newpage = orient)
  look <- tapply(svts5$Region_name, svts5$region, function(x) sort(unique(x)))
  if (sum(sapply(look, length) < 2)) {
    tab <- cbind(names(look), sapply(look, paste, collapse = ", "))
    tabl("Only one transect in at least one region.", "  Variance will be estimated with this region(s) removed.",
         TAB = tab)
  }
  nph <- svts5$fish_ha * nprops[match(svts5$nearmt, allops),
                                ]
  nph[!is.finite(nph)] <- 0
  gph <- nph * mnwts[match(svts5$nearmt, allops), ]
  gph[!is.finite(gph)] <- 0
  rownames(nph) <- NULL
  intlaymeans_nph <- cbind(svts5, nph)
  rownames(gph) <- NULL
  intlaymeans_gph <- cbind(svts5, gph)
  intmeans_nph <- aggregate(nph ~ region + regarea + Region_name +
                              Interval + depth_botmid + Lat_M + Lon_M, sum, data = svts5)
  names(intmeans_nph)[is.na(names(intmeans_nph))] <- sp.grps
  intmeans_gph <- aggregate(gph ~ region + regarea + Region_name +
                              Interval + depth_botmid + Lat_M + Lon_M, sum, data = svts5)
  names(intmeans_gph)[is.na(names(intmeans_gph))] <- sp.grps
  ncols <- grep("\\.", names(intmeans_nph))
  fig <- function() {
    mapBy2Groups(df = intmeans_nph[, ncols], lon = intmeans_nph$Lon_M,
                 lat = intmeans_nph$Lat_M, nrows = c(3, 4)[short +
                                                             1])
  }
  figu("Acoustic density for each species group.  Groups are defined by",
       " length cut offs (L) in mm or ages (A).", "  Darker and larger circles indicate higher density.",
       FIG = fig, newpage = "port")
  gcols <- grep("\\.", names(intmeans_gph))
  fig <- function() {
    mapBy2Groups(df = intmeans_gph[, gcols], lon = intmeans_gph$Lon_M,
                 lat = intmeans_gph$Lat_M, nrows = c(3, 4)[short +
                                                             1])
  }
  figu("Acoustic biomass for each species group.  Groups are defined by",
       " length cut offs (L) in mm or ages (A).", "  Darker and larger circles indicate greater biomass.",
       FIG = fig, newpage = "port")
  areas <- data.frame(region = region, regArea = regArea, stringsAsFactors = FALSE)
  nphests <- intmeans_nph %>% select(-regarea) %>% gather(spgrp,
                                                          nph, -region, -Region_name, -Interval, -depth_botmid,
                                                          -Lat_M, -Lon_M) %>% split(.$spgrp) %>% purrr::map(stratClust,
                                                                                                            stratum = "region", cluster = "Region_name", response = "nph",
                                                                                                            sizedf = areas, size = "regArea")
  nphTrans <- purrr::map_df(nphests, "Cluster", .id = "spgrp")
  nphRegion <- purrr::map_df(nphests, "Stratum", .id = "spgrp")
  nphLake <- purrr::map_df(nphests, "Population", .id = "spgrp")
  gphests <- intmeans_gph %>% select(-regarea) %>% gather(spgrp,
                                                          gph, -region, -Region_name, -Interval, -depth_botmid,
                                                          -Lat_M, -Lon_M) %>% split(.$spgrp) %>% purrr::map(stratClust,
                                                                                                            stratum = "region", cluster = "Region_name", response = "gph",
                                                                                                            sizedf = areas, size = "regArea")
  gphTrans <- purrr::map_df(gphests, "Cluster", .id = "spgrp")
  gphRegion <- purrr::map_df(gphests, "Stratum", .id = "spgrp")
  gphLake <- purrr::map_df(gphests, "Population", .id = "spgrp")
  Regions <- bind_rows(nph = nphRegion, gph = gphRegion, .id = "metric") %>%
    select(metric, spgrp, region = h, n.intervals = m_h,
           reg.total = y_h, n.transects = n_h, reg.mean = ybar_h,
           reg.se.mean = s_ybar_h, area = A_h, rel.area = W_h)
  Lakes <- bind_rows(count = nphLake, biomass = gphLake, .id = "metric") %>%
    gather(estimate, value, ybar_str, s_ybar_str, ytot_str,
           s_ytot_str) %>% mutate(level = ifelse(grepl("ybar",
                                                       estimate), "mean", "total"), value = ifelse(level ==
                                                                                                     "total", value/1e+06, value), units = case_when(metric ==
                                                                                                                                                       "biomass" & level == "mean" ~ "gph", metric == "biomass" &
                                                                                                                                                       level == "total" ~ "t", metric == "count" & level ==
                                                                                                                                                       "mean" ~ "nph", metric == "count" & level == "total" ~
                                                                                                                                                       "millions"), type = ifelse(substring(estimate, 1, 1) ==
                                                                                                                                                                                    "s", "SE", "Estimate")) %>% select(-estimate) %>% spread(type,
                                                                                                                                                                                                                                             value) %>% mutate(RSE = 100 * SE/Estimate) %>% select(metric,
                                                                                                                                                                                                                                                                                                   level, units, area = A, spgrp, Estimate, SE, RSE) %>%
    arrange(metric, level, units)
  save2csv <- c("Regions", "Lakes", "intmeans_nph", "intmeans_gph",
                "intlaymeans_nph", "intlaymeans_gph", "svts5")
  outfiles <- paste0(maindir, "L", LAKE, " Y", YEAR, " ", descr,
                     " ", save2csv, " ", lubridate::today(), ".csv")
  invisible(lapply(seq(save2csv), function(i) write.csv(eval(parse(text = save2csv[i])),
                                                        outfiles[i])))
  newrdat <- paste0("L", LAKE, " Y", YEAR, " ", descr)
  save(list = save2csv, file = paste0(maindir, newrdat, ".RData"))
  bp <- with(Regions, tapply(reg.mean * area/1e+06, list(region,
                                                         spgrp, metric), mean))
  mypalette <- RColorBrewer::brewer.pal(6, "Set3")
  fig <- function() {
    par(mar = c(4, 5, 0, 1), oma = c(0, 0, 2, 0), mfrow = c(1,
                                                            2), cex = 1.2)
    barplot(bp[, , "nph"], col = mypalette, horiz = TRUE,
            las = 1, xlab = "Number of fish  (millions)")
    barplot(bp[, , "gph"], col = mypalette, horiz = TRUE,
            las = 1, xlab = "Biomass of fish  (t)", legend.text = TRUE,
            args.legend = list(x = "topright"))
  }
  figu("Acoustic survey lakewide estimates in number (left) and",
       " biomass (right) for each species group.", "  Groups are defined by length cut offs (L) in mm or ages (A).",
       "  Colors are used to identify contributions from different regions",
       FIG = fig, h = 5.8, w = 9, newpage = "land")
  tab <- mutate_if(Lakes, is.numeric, function(x) format(round(x),
                                                         big.mark = ","))
  tabl("Lakewide estimates in biomass density (g per ha), biomass(t), fish",
       " density (number per ha), and number (millions) for each species group.",
       "  Groups are defined by length cut offs (L) in mm or ages (A).",
       TAB = tab)
  endrtf()
}
