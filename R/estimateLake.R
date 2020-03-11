#' Lake-Wide Fish Estimates from Acoustic and Midwater Trawl Data
#'
#' Estimate lake-wide fish density and total number
#' (in number per ha and millions) and
#' biomass density and total biomass (in g per ha and t) from
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
#' @param TSthresh
#'   A numeric scalar, the minimum number of binned targets required to
#'   incorporate the TS information from a given (interval by layer) cell.
#' @param psi
#'   A numeric scalar, the transducer-specific two-way equivalent beam angle
#'   in steradians, default 0.01.
#' @param chngBinCntToZero
#'   Logical scalar, indicating if the user wishes to convert the number
#'   of targets in specific TS bins to zero so they don't influence sigma.
#' @param BinCntZeroParams
#'   A numeric vector, depth and TS ranges below which the number of targets
#'   in the specified TS bins will be converted to zero
#' @param SpeciesFromDepthTS
#'   Logical scalar indicating if the user wishes to use depth and TS to
#'   identify species.
#' @param DepthTSParams
#'   A numeric vector of depth (meters) and TS (dB) as in (DepthTSParams = c(40, -45)).
#'   Currently this function can only be used to assign density in cells >= the values
#'   provided here. Future modifications may provide more flexibility.
#' @param rmBycatch
#'   Logical scalar indicating whether or not the user wishes to remove particular
#'   species from midwater trawl data because they are believed to be bycatch. One
#'   common example of a need for this was reported by Warner et al. (2012), who
#'   found that catch observed over the course of three decades varied with fishing
#'   depth. At depths > 40 m below the surface, the vast majority (> 70 percent) of fish
#'   were bloater > 120 mm.
#' @param ByCatchParams
#'   A vector with the first entry being depth, below which the user believes
#'   the species (the remaining portion of the vector) are bycatch and
#'   should be removed from the fishing data.
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
#' @param descr
#'   A character scalar to be incorporated in the name of the saved output
#'   files, default "ACMT Estimates".
#' @inheritParams sliceCat
#' @return
#'   A rich text file (rtf) with a *.doc file extension (so that it will be
#'   opened with Word by default) is saved to \code{maindir}.
#'
#'   Seven different data frames are saved as objects in an Rdata
#'   file and are written to csv files in \code{maindir}:
#'   \itemize{
#'     \item \code{Lakes} = lake-wide totals (in millions and t) and
#'       means (in numbers and g per ha), with a row for each species group
#'       and estimate type and columns for estimates, standard errors, and
#'       relative standard errors.
#'     \item \code{Regions} = region means (in fish per ha and g per ha), with
#'       a row for each region, species group, and estimate type and columns for
#'       estimates and corresponding (surface) areas.
#'     \item \code{intmeans_nph} = interval means (in fish per ha), with a row
#'       for each region and interval, a column for each species group, and
#'       additional columns for region area, and the interval bottom depth,
#'       latitude and longitude.
#'     \item \code{intmeans_gph} = interval means (in g per ha), similar to
#'       \code{intmeans_nph}.
#'     \item \code{intlaymeans_nph} = interval and layer means (in fish per ha),
#'       with a row for each region, interval, and layer, a column for each
#'       species group, and many additional columns.
#'     \item \code{intlaymeans_gph} = interval and layer means (in g per ha),
#'       similar to \code{intlaymeans_nph}.
#'     \item \code{svts5} = the result of merging the SV and TS data, with
#'       several changes made: subsetted to valid regions, original and modified
#'       sigma estimates of n1, nv, fish_ha, depth_botmid (bottom depth range),
#'       and identity of slice, nearmt (nearest midwater trawl), region, and
#'       region area.
#'   }
#'   The Rdata and csv files are named using the lake and the year.
#'
#' @details
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
#' @importFrom class knn1
#' @importFrom RColorBrewer brewer.pal
#' @importFrom purrr map map_df
#' @importFrom magrittr "%>%"
#' @importFrom lubridate today decimal_date
#' @importFrom stringr str_extract
#' @import dplyr rtf graphics utils tidyr
#' @export
#'

estimateLake <-
  function (maindir,
            rdat = "ACMT",
            ageSp = NULL,
            region,
            regArea,
            TSrange = c(-60,-30),
            TSthresh = 1,
            psi = 0.01,
            chngBinCntToZero = FALSE,
            BinCntZeroParams = c(depth = 40, TS = -45),
            SpeciesFromDepthTS = FALSE,
            DepthTSParams = c(SpDepth = 40,
                              SpTS = -45,
                              SPECIES = 204),
            rmBycatch = FALSE,
            ByCatchParams = c(bcdepth = 40, bcSPECIES = c(106, 109)),
            soi = c(106, 109, 203, 204),
            spInfo,
            sliceDef,
            short = TRUE,
            IntMeansPlots = FALSE,
            descr = "ACMT Estimates")
  {
    load(paste0(maindir, rdat, ".RData"), envir = environment())
    LAKE <- keyvals[1]
    YEAR <- keyvals[2]
    old_options <-
      options(stringsAsFactors = FALSE, survey.lonely.psu = "remove")
    on.exit(options(old_options))
    ly <- LAKE %in% optrop$LAKE & YEAR %in% optrop$YEAR
    if (length(ly) < 1)
      stop(paste0(
        "\nNo information from ",
        Lakenames[LAKE],
        " in ",
        YEAR,
        " in RVCAT data.\n\n"
      ))
    spInfo$spname <- as.character(spInfo$spname)
    xtra <- setdiff(soi, spInfo$sp)
    ###################################################################
    ###################################################################
    #
    # 1. new function/argument implementation
    # Prompts user for EBA from unique transducers
    ##################################################################
    #EV_filename_parts <- strsplit(sv$EV_filename, "[\\]")

    sv$EVfolder <- str_extract(sv$EV_filename,
   "EV_files_sturgeon|EV_files_steelhead|EV_files_baird|EV_files_LTBB|EVfiles_sturgeon|EVfiles_steelhead|EVfiles_baird|EVfiles_LTBB")


    sv$dat.source <-paste0(sv$EVfolder, " - ", sv$Frequency, " kHz")
    ev.source.freq <- unique(sv[c("EVfolder", "Frequency")])
    x <- 1
    psi.df = data.frame()
    unique.transducer <- paste0(ev.source.freq$EVfolder, " - ", ev.source.freq$Frequency, " kHz" )
    while(x<=length(unique.transducer)) {
      df <- data.frame(dat.source = NA, psi = NA, EBA = NA)
      df$dat.source <- unique.transducer[x]
      df$EBA <- as.numeric(svDialogs::dlg_input(GUI = EBA, paste0("Enter Equivalent beam angle in dB for ", unique.transducer[x], ":"))$res)
      df$psi <- 10^(df$EBA/10)
      psi.df <- rbind(psi.df, df)
      x <- x+1
    }
    x <- NULL
    #write.csv(psi.df, paste0(maindir, substr(strsplit(maindir, "[/]")[[1]][4], 1,2), YEAR, "sources_EBA_psi.csv"), row.names = FALSE)
    write.csv(psi.df, paste0(maindir, "sources_EBA_psi_", YEAR, ".csv"), row.names = FALSE)
    sv$psi <- NA
    sv$psi <- psi.df$psi[match(sv$dat.source, psi.df$dat.source)]

    if (length(xtra) > 0)
      stop(
        paste0(
          "\nThere is at least one species listed in soi= that has no information in spInfo=: ",
          paste(xtra, collapse = ", "),
          "."
        )
      )
    docname <- paste0("L", LAKE, " Y", YEAR, " ", descr, " ",
                      lubridate::today(), ".doc")
    doc <<- startrtf(file = docname, dir = maindir)
    heading(
      paste0(
        YEAR,
        " Lake ",
        Lakenames[LAKE],
        " Estimation from Acoustic and Trawl Data   ",
        lubridate::today()
      )
    )
    para(
      "Created using the R package EchoNet2Fish (https://github.com/JVAdams/EchoNet2Fish), written by Jean V. Adams for Dave Warner."
    )
    para(paste0(docname, " = this document."))
    heading("INPUTS", 2)
    para(paste0("maindir = ", maindir, " = main input/output directory."))
    para(paste0("TSrange = ", TSrange[1], " to ", TSrange[2],
                " = TS range of interest."))
    para(paste0(
      "TSthresh = ",
      TSthresh,
      " = minimum threshold for binned targets in a cell."
    ))

    psi.list <- unique(psi.df$dat.source)
    para(paste0('The number of vessels involved = ', length(psi.list)))

    for (i in seq_along(psi.list)) {
      para(
        paste0("For ", psi.df$dat.source[i],
               " psi = ",
               round(psi.df$psi[i], 6),
               " = the transducer-specific two-way equivalent beam angle(s) in steradians."
        )
      )
    }

    if (is.null(ageSp)) {
      para("Ages will NOT be used.")
    } else {
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
    ts.names <-
      paste0("X.", seq(TS.range.abs[[1]], TS.range.abs[[2]],-1))
    ts$ts.range.binned <- rowSums(ts[, ts.names])
    ts$sigma <- sigmaAvg(TSdf = ts, TSrange = TSrange)
    ts <- subset(ts, ts.range.binned > TSthresh)

    ##################################################################################
    ##################################################################################
    #
    # 2. This is an implementation of a new argument to the function.
    # If argument chngBinCntToZero == TRUE, replace the # of targets binned with zero
    #
    # ################################################################################
    bin.num.st <- which( colnames(ts)==paste0("X.", abs(TSrange[[1]])))
    bin.num.end <- which( colnames(ts)==paste0("X.", abs(TSrange[[2]])))

    if (chngBinCntToZero == TRUE) ts[which(ts$Layer_depth_min >= BinCntZeroParams[[1]] & ts$sigma < 10^(BinCntZeroParams[[2]]/10)), bin.num.st:bin.num.end] <- 0

    # Combine sv and ts
    sv$UID <-interaction(gsub(" ", "", sv$Region_name), sv$Interval,
                         sv$Layer)
    sv$source.sv <- sv$source
    ts$UID <-
      interaction(gsub(" ", "", ts$Region_name), ts$Interval,
                  ts$Layer)
    ts$source.ts <- ts$source
    svdup <- sv[sv$UID %in% sv$UID[duplicated(sv$UID)],]
    tsdup <- ts[ts$UID %in% ts$UID[duplicated(ts$UID)],]
    if (dim(svdup)[1] > 0) {
      print(svdup[, c("Region_name", "Interval", "Layer", "source.sv")])
      stop("There should be only one row in SV for each Region_name, Interval, and Layer.")
    }
    if (dim(tsdup)[1] > 0) {
      print(tsdup[, c("Region_name", "Interval", "Layer", "source.ts")])
      stop("There should be only one row in TS for each Region_name, Interval, and Layer.")
    }
    svts <- merge(sv[, c(
      "UID",
      "Region_name",
      "Interval",
      "Layer",
      "Layer_depth_min",
      "Layer_depth_max",
      "Lat_M",
      "Lon_M",
      "year",
      "Date_M",
      "Sv_mean",
      "Depth_mean",
      "PRC_ABC",
      "source.sv",
      "psi"
    )], ts[, c("UID", "source.ts", "sigma")],
    by = "UID", all = TRUE)
    svts$Region_name <- gsub(" ", "", svts$Region_name)
    svx <- setdiff(sv$UID, ts$UID)
    tsx <- setdiff(ts$UID, sv$UID)
    if (length(tsx) > 0) {
      sel <- svts$UID %in% tsx
      tab <- svts[sel, c("UID", "sigma", "source.ts")]
      tabl(
        "There is at least one region-interval-layer combination that occurs",
        " in the TS data but not in the SV data.",
        "  These data will be removed from further calculations.",
        TAB = tab
      )
      svts <- svts[!sel,]
    }
    svts$sigma.orig <- svts$sigma
    svts$n1 <- with(svts, estrhov(Sv = Sv_mean, sigma = sigma))
    svts$nv <-
      with(svts, estNv(psi = psi, R = Depth_mean, rhov = n1))
    laymid <- with(svts,-(Layer_depth_min + Layer_depth_max) / 2)
    lat.r <-
      with(svts, tapply(Lat_M, Region_name, mean, na.rm = TRUE))
    Region_ord <- names(lat.r)[order(lat.r, decreasing = T)]
    fig <- function(x, xname) {
      with(svts,
           plotIntLay(
             Interval,
             laymid,
             Region_name,
             Region_ord,
             colorVal(x),
             paste0("Colors indicate ",
                    xname)
           ))
    }
    prefix <-
      "Interval by layer plots for Sv TS files.  Colors indicate "
    figu(
      prefix,
      "Nv",
      FIG = function()
        fig(svts$nv, "Nv"),
      newpage = "port"
    )
    svts$sigma <- replaceBiasedSigma(
      df = svts,
      varNv = "nv",
      varsigmabs = "sigma",
      varTranLay = c("Region_name", "Layer",
                     "year")
    )
    svts$sigma[is.na(svts$sigma)] <- 0
    svts$n1 <- with(svts, estrhov(Sv = Sv_mean, sigma = sigma))
    svts$nv <-
      with(svts, estNv(psi = psi, R = Depth_mean, rhov = n1))
    svts$fish_ha <-
      with(svts, paDens(sigma, PRC_ABC, hectare = TRUE))
    depth.botmin <-
      aggregate(Layer_depth_min ~ Interval + Region_name,
                max, data = svts)
    names(depth.botmin)[names(depth.botmin) == "Layer_depth_min"] <-
      "depth.botmin"
    depth.botmax <-
      aggregate(Layer_depth_max ~ Interval + Region_name,
                max, data = svts)
    names(depth.botmax)[names(depth.botmax) == "Layer_depth_max"] <-
      "depth.botmax"
    depth.bot <- merge(depth.botmin, depth.botmax, all = TRUE)
    svts4 <- merge(svts, depth.bot, all = TRUE)
    svts4$depth_botmid <-
      (svts4$depth.botmin + svts4$depth.botmax) / 2
    svts5 <-
      data.frame(
        svts4,
        slice = sliceCat(
          sliceDef,
          fdp = svts4$Depth_mean,
          bdp = svts4$depth_botmid,
          lon = svts4$Lon_M,
          lat = svts4$Lat_M,
          reg = substring(svts4$Region_name, 1, 2)
        )
      )

    ###############################################################################
    ###############################################################################
    #
    # 3. Implement new argument to use TS and depth to ID species, length, and weight.
    #    I debated how/where it was best to do this. It could be done at a later point.
    #    I chose to do it this way, essentially creating simulated trawl tows for
    #    the species I am ID'ing with TS and depth so that there are no issues with
    #    any slice NOT having trawl data and so I am able to generate plots of length
    #    and weight for this species.
    #
    ################################################################################

    if (SpeciesFromDepthTS == TRUE) {
      tsdeep <- subset(ts, Layer_depth_min>=DepthTSParams[[1]] & !(is.nan(ts$sigma)) &
                         ts$sigma !=0)
      #keep only the hypo layers with actual sigma
      tsdeep$TS <- log10(tsdeep$sigma)*10
      tran.sig <- tsdeep %>% group_by(Region_name, Interval, Layer, Layer_depth_min) %>%
        summarise(mn.sigma = mean(sigma), Latitude = mean(Lat_M),
                  Longitude = mean(Lon_M), FISHING_DEPTH =mean(Layer_depth_min)) %>%
        mutate(TS = log10(mn.sigma)*10) %>%
        filter(TS >= DepthTSParams[[2]] & TS < -35.9) %>%
        mutate(LENGTH.m = round((10^(3.2 + 0.047*TS))*10)) %>%
        mutate(WEIGHT.m = 10^(7.449 + 0.141*TS), N=round(rnorm(1,50,8)))%>%
        mutate(WEIGHT = round(N * WEIGHT.m))
      tran.sig$TRANSECT <- tran.sig$Region_name
      tran.sig$Region_name <- NULL

      tran.sim <- tran.sig %>% group_by(TRANSECT, Interval) %>%
        summarise(LENGTH = round(mean(LENGTH.m)), fish.wt = mean(WEIGHT.m), N=round(mean(N)),
                  cat.wt =mean(WEIGHT))
      ts.op <- data.frame(tran.sim[1:2]) # this will be used
      # in merge wth sim.op to reduce the data to the transect-interval combos where
      # we had TS
      #############################################################################
      # Now we need to get the different variables required to make
      # Op, catch, and lf data.
      #
      #Op needs
      #OP_ID YEAR  BEG_DEPTH END_DEPTH FISHING_DEPTH TRANSECT Latitude Longitude
      #We will get all of these from the Sv data (sv data.frame), merge it with our TS data,
      #and generate OP_ID there.
      #
      sim.op <- data.frame(YEAR = YEAR, LAKE = LAKE, TRANSECT = sv$Region_name, Interval = sv$Interval,
                           Layer = sv$Layer, FISHING_DEPTH = sv$Layer_depth_min, BEG_DEPTH = sv$Exclude_below_line_depth_min,
                           END_DEPTH = sv$Exclude_below_line_depth_max, Latitude = sv$Lat_M, Longitude = sv$Lon_M)
      sim.op <- subset(sim.op, FISHING_DEPTH>=40)
      sim.op2 <- sim.op %>% group_by(TRANSECT, Interval) %>%
        summarise_all(mean)
      sim.op3 <- merge(sim.op2, ts.op, by=c("TRANSECT", "Interval"))

      #need OP_ID, we have 90 unique tran-interval combos
      sim.op3$OP_ID <- seq(1,length(sim.op3$Interval ),1)

      #now have to add OP_ID to tran.hoyi.sim to create catch data and tr_lf data
      #will do this by merging sim.op3 OP_ID, Tran, and Interval
      names(sim.op3)
      cols <- c("TRANSECT","Interval","OP_ID")
      opid.for.catch.lf <- sim.op3[,cols]

      #now use above data frame to create catch and lf data by merging on tran-interval
      sim.catch <- merge(opid.for.catch.lf, tran.sim, by=c("TRANSECT", "Interval"))
      place <- ifelse(keyvals[1]==2, 'MI', "HU")
      png(paste0(maindir, "TSsimulated_bloater_weight.png"))
      hist(sim.catch$fish.wt)
      dev.off()

      sim.catch$SPECIES <- 204
      names(sim.catch)
      cols <- c("OP_ID","N","cat.wt","SPECIES")
      sim.catch2 <- sim.catch[,cols]
      sim.catch2$WEIGHT <- sim.catch2$cat.wt
      sim.catch2$cat.wt <- NULL
      sim.catch2$fish.wt <- NULL
      # now we have a catch file
      #
      #now make tr_lf file from the catch data
      names(sim.catch)
      cols <- c("OP_ID", "SPECIES", "LENGTH", "N")
      sim.tr_lf <- sim.catch[,cols]

      png(paste0(maindir, "TSsimulated_bloater_length.png"))
      hist(sim.tr_lf$LENGTH, breaks = seq(100,350,25))
      dev.off()

      ##################################Now we have to add the simulated op, catch,
      #and tr_lf to the actual data.
      deepops <- subset(optrop$OP_ID, optrop$FISHING_DEPTH >= 40)
      nonbloatdeepcatch <- unique(subset(trcatch$OP_ID, trcatch$OP_ID %in% deepops & trcatch$SPECIES %in% c(106,109)))
      #remove tows from op where tow was deep and catch was nonbloater
      optrop.sub <- subset(optrop, !(OP_ID %in% nonbloatdeepcatch))

      # same with catch now
      trcatch.sub <- subset(trcatch, !(OP_ID %in% nonbloatdeepcatch))

      #and finally trlf
      trlf.sub <- subset(trlf, !(OP_ID %in% nonbloatdeepcatch))

      optrop.new <- plyr::rbind.fill(optrop.sub, sim.op3)
      optrop.new$Layer <- NULL
      optrop.new$Interval <- NULL
      optrop <- optrop.new

      trcatch.new <- plyr::rbind.fill(trcatch.sub, sim.catch2)
      names(trcatch.new)
      cols <- c("OP_ID","N","WEIGHT","SPECIES")
      trcatch <- trcatch.new[,cols]

      trlf.new <- plyr::rbind.fill(trlf.sub, sim.tr_lf)
      names(trlf.new)
      cols <- c("OP_ID","SPECIES","LENGTH","N")
      trlf <- trlf.new[,cols]
    }

    # Trawl stuff
    optrop$depth.botmin <- 10 * floor(pmin(optrop$BEG_DEPTH,
                                           optrop$END_DEPTH) / 10)
    optrop$depth.botmax <- 10 * ceiling(pmax(optrop$BEG_DEPTH,
                                             optrop$END_DEPTH) / 10)
    optrop$depth_botmid <- (optrop$BEG_DEPTH + optrop$END_DEPTH) / 2
    overallmaxdep <-
      max(depth.bot$depth.botmax, optrop$depth.botmax,
          na.rm = TRUE) + 10
    optrop$layer <- cut(optrop$FISHING_DEPTH, seq(0, overallmaxdep,
                                                  10), right = FALSE)
    optrop <-
      data.frame(
        optrop,
        slice = sliceCat(
          sliceDef,
          fdp = optrop$FISHING_DEPTH,
          bdp = optrop$depth_botmid,
          lon = optrop$Longitude,
          lat = optrop$Latitude,
          reg = substring(optrop$TRANSECT, 1, 2)
        )
      )
    trcatch2 <- aggregate(cbind(N, WEIGHT) ~ OP_ID + SPECIES,
                          sum, data = trcatch)

    trlf2 <- aggregate(N ~ OP_ID + SPECIES, sum, data = trlf)
    look <-
      trcatch2 %>% full_join(
        trlf2,
        by = c("OP_ID", "SPECIES"),
        suffix = c(".caught", ".measured")
      ) %>% mutate(scaleup = N.caught / N.measured)
    warnsub <- filter(look, scaleup < 1)
    if (dim(warnsub)[1] > 0) {
      cat("\n\n")
      warning(
        "There was at least one case where the number of fish captured was LESS THAN the number of fish measured."
      )
      print(select(warnsub,-scaleup))
      cat("\n\n")
    }
    trlf3 <- trlf %>% left_join(select(look, OP_ID, SPECIES,
                                       scaleup),
                                by = c("OP_ID", "SPECIES")) %>% mutate(N.scaled = N *
                                                                         scaleup)
    indx <- match(trlf3$SPECIES, spInfo$sp)
    trlf3$estwal <- estWEIGHT(trlf3$LENGTH, spInfo$lwa[indx],
                              spInfo$lwb[indx])
    trlf3$estfw <- trlf3$estwal * trlf3$N.scaled
    if (!is.null(ageSp)) {
      allspsel <- c(ageSp, soi)
      allops <- sort(unique(optrop$OP_ID))
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
        lfa <- trlf3[trlf3$SPECIES %in% ageSp[i],]
        lfa$mmgroup <- 10 * round((lfa$LENGTH + 5) / 10) -
          5
        ga <- aggregate(cbind(N.scaled, estfw) ~ OP_ID +
                          mmgroup, sum, data = lfa)
        gkeya <- merge(ga, agekey[[i]], all.x = TRUE)
        agecolz <- grep("Age", names(gkeya))
        names(gkeya)[agecolz] <-
          paste0(ageSp[i], ".A", substring(names(gkeya)[agecolz],
                                           4, 10))
        tot.n <- apply(gkeya$N.scaled * gkeya[, agecolz],
                       2, tapply, gkeya$OP_ID, sum)
        m.w <- apply(gkeya$estfw * gkeya[, agecolz], 2, tapply,
                     gkeya$OP_ID, sum) / tot.n
        sum.n[[i]] <- tidyup(tot.n, allops)
        mean.w[[i]] <- tidyup(m.w, allops)
      }
    } else {
      allspsel <- soi
      allops <- sort(unique(optrop$OP_ID))
      sum.n <- vector("list", length(soi))
      names(sum.n) <- soi
      mean.w <- sum.n
      add.sp <- 0
    }
    tidyup2 <- function(x, uniqops, uniqlens) {
      m <- array(0,
                 dim = c(length(uniqops), length(lclong)),
                 dimnames = list(uniqops, paste0(sp, ".L", lclong)))
      if (dim(x)[1] > 0) {
        m[match(dimnames(x)[[1]], uniqops), match(dimnames(x)[[2]],
                                                  uniqlens)] <- x
      }
      m
    }
    allops <- sort(unique(optrop$OP_ID))
    for (i in seq(soi)) {
      sp <- soi[i]
      lc <- spInfo$lcut[spInfo$sp == sp]
      lclong <- unique(c(0, lc))
      lf <- trlf3[trlf3$SPECIES == sp,]
      lf$mmgroup <- lc * (lf$LENGTH > lc)
      tot.n <- tapply(lf$N.scaled, list(lf$OP_ID, lf$mmgroup),
                      sum)
      tot.n[is.na(tot.n)] <- 0
      m.w <- tapply(lf$estfw, list(lf$OP_ID, lf$mmgroup), sum) / tot.n
      m.w[is.na(m.w)] <- 0
      sum.n[[add.sp + i]] <- tidyup2(tot.n, allops, lclong)
      mean.w[[add.sp + i]] <- tidyup2(m.w, allops, lclong)
    }
    if (length(setdiff(unique(trcatch2$SPECIES), soi)) > 0) {
      sumbyspec <- tapply(trcatch2$N,
                          list(trcatch2$OP_ID,
                               trcatch2$SPECIES %in% soi),
                          sum)
      sumbyspec[is.na(sumbyspec)] <- 0
      propother <- sumbyspec[, 1] / apply(sumbyspec, 1, sum)
      sel <- propother > 0.1 & !is.na(propother)
      if (sum(sel) > 0) {
        look <- trcatch2[trcatch2$OP_ID %in% names(propother)[sel] &
                           !(trcatch2$SPECIES %in% soi),]
        tab <- look[order(look$OP_ID,-look$N, look$SPECIES),
                    c("OP_ID", "SPECIES", "N", "WEIGHT")]
        tabl(
          "SPECIES other than those selected (",
          paste(soi,
                collapse = ", "),
          ") are ignored when calculating proportions, but other species make up",
          " > 10% of the *NUMBER* in at least one trawl haul.",
          "  The locations of these tows are highlighted in Figure 1.",
          TAB = tab
        )
        mtops <- names(propother)[sel]
      }
      sumbyspec <- tapply(trcatch2$WEIGHT,
                          list(trcatch2$OP_ID,
                               trcatch2$SPECIES %in% soi),
                          sum)
      sumbyspec[is.na(sumbyspec)] <- 0
      propother <- sumbyspec[, 1] / apply(sumbyspec, 1, sum)
      sel <- propother > 0.1 & !is.na(propother)
      if (sum(sel) > 0) {
        look <- trcatch2[trcatch2$OP_ID %in% names(propother)[sel] &
                           !(trcatch2$SPECIES %in% soi),]
        tab <- look[order(look$OP_ID,-look$WEIGHT, look$SPECIES),
                    c("OP_ID", "SPECIES", "N", "WEIGHT")]
        tabl(
          "SPECIES other than those selected (",
          paste(soi,
                collapse = ", "),
          ") are ignored when calculating proportions, but other species make up",
          " > 10% of the *WEIGHT* in at least one trawl haul.",
          "  The locations of these tows are highlighted in Figure 1.",
          TAB = tab
        )
        mtops <- if (exists("mtops"))
          c(mtops, names(propother)[sel])
      } else {
        names(propother)[sel]
      }
    }
    ord <- order(names(sum.n))
    counts <- do.call(cbind, sum.n[ord])
    mnwts <- do.call(cbind, mean.w[ord])
    sp.grps <- dimnames(counts)[[2]]
    grp.sp <- sapply(strsplit(sp.grps, "\\."), "[", 1)
    grp.type <- substring(sapply(strsplit(sp.grps, "\\."), "[",
                                 2), 1, 1)
    if (!is.null(ageSp) & any(sapply(ageSp, function(x)
      x %in%
      soi))) {
      sum.counts <- apply(counts[, grp.type == "L"], 1, sum)
    } else {
      sum.counts <- apply(counts, 1, sum)
    }
    nprops <- sweep(counts, 1, sum.counts, "/")
    nprops[is.na(nprops)] <- 0
    opsub <- optrop[match(allops, optrop$OP_ID),]
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
          tempx <- as.character(class::knn1(MTutm[selm,], ACutm[sela,], allops[selm]))
          if (sum(grepl("^[0-9]+$", tempx)) > 0) {
            svts5$nearmt[sela] <- as.numeric(tempx)
          }
          else {
            svts5$nearmt[sela] <- tempx
          }
        } else {
          svts5$nearmt[sela] <- allops[selm]
        }
      }
    }
    fig <- function() {
      with(
        opsub,
        mapMulti(
          bygroup = slice,
          sug = names(sliceDef),
          plottext = TRUE,
          ID = OP_ID,
          short = short,
          lon = Longitude,
          lat = Latitude,
          rlon = range(Longitude, svts5$Lon_M,
                       na.rm = TRUE),
          rlat = range(Latitude, svts5$Lat_M,
                       na.rm = TRUE),
          misstext = " - No tows"
        )
      )
    }
    figu(
      "Location of midwater trawl hauls in slices.",
      "  Numbers identify the OP_ID of each tow.  Colors are the same as",
      " in the next figure.",
      "  Tows with > 10% of their catch (by number or weight) in 'other' species",
      " are shown in large, bold font.",
      FIG = fig,
      h = 8,
      newpage = "port"
    )
    fig <- function() {
      mapAppor(
        MTgroup = opsub$slice,
        ACgroup = svts5$slice,
        sug = names(sliceDef),
        MTID = opsub$OP_ID,
        ACID = svts5$nearmt,
        short = short,
        MTlon = opsub$Longitude,
        MTlat = opsub$Latitude,
        AClon = svts5$Lon_M,
        AClat = svts5$Lat_M,
        misstext = " - No tows"
      )
    }
    figu(
      "Apportionment using slices.",
      "  Each MT tow is shown as a white circle (o).",
      "  Each AC interval is shown as a colored plus sign (+).",
      "  Dotted lines encircle all the AC intervals (given the same color) that",
      " used each MT tow for apportionment.",
      FIG = fig,
      h = 8,
      newpage = "port"
    )
    if (length(unique(c(opsub$slice, ACgroup = svts5$slice))) >
        4) {
      orient <- "port"
    } else {
      orient <- "land"
    }
    fig <- function() {
      plotACMTslice(
        MTgroup = opsub$slice,
        ACgroup = svts5$slice,
        MTbd = opsub$depth_botmid,
        ACbd = svts5$depth_botmid,
        MTwd = opsub$FISHING_DEPTH,
        ACwd = svts5$Depth_mean
      )
    }
    figu(
      "Acoustic (left) and midwater trawl (right) data by slice.",
      FIG = fig,
      newpage = orient
    )
    svts5$region <- substring(svts5$Region_name, 1, 2)
    svts5$regarea <- regArea[match(svts5$region, region)]
    sur <- sort(unique(svts5$region))
    if (!identical(sort(region), sur))
      warning(
        paste0(
          "\nStrata used in laying out the sampling design (",
          paste(sort(region), collapse = ", "),
          ") do not match up with the strata actually sampled (",
          paste(sur, collapse = ", "),
          ").\n\n"
        )
      )
    if (short) {
      orient <- "land"
    } else {
      orient <- "port"
    }
    fig <- function() {
      mapByGroup(
        bygroup = svts5$region,
        lon = svts5$Lon_M,
        lat = svts5$Lat_M
      )
    }
    figu(
      "Acoustic transect data, color coded by design-based strata.",
      FIG = fig,
      newpage = orient
    )
    look <-
      tapply(svts5$Region_name, svts5$region, function(x)
        sort(unique(x)))
    if (sum(sapply(look, length) < 2)) {
      tab <- cbind(names(look), sapply(look, paste, collapse = ", "))
      tabl(
        "Only one transect in at least one region.",
        "  Variance will be estimated with this region(s) removed.",
        TAB = tab
      )
    }
    nph <- svts5$fish_ha * nprops[match(svts5$nearmt, allops),]
    nph[!is.finite(nph)] <- 0
    gph <- nph * mnwts[match(svts5$nearmt, allops),]
    gph[!is.finite(gph)] <- 0
    rownames(nph) <- NULL
    intlaymeans_nph <- cbind(svts5, nph)
    rownames(gph) <- NULL
    intlaymeans_gph <- cbind(svts5, gph)
    intmeans_nph <- aggregate(nph ~ region + regarea + Region_name +
                                Interval + depth_botmid + Lat_M + Lon_M,
                              sum,
                              data = svts5)
    names(intmeans_nph)[is.na(names(intmeans_nph))] <- sp.grps
    intmeans_gph <- aggregate(gph ~ region + regarea + Region_name +
                                Interval + depth_botmid + Lat_M + Lon_M,
                              sum,
                              data = svts5)
    names(intmeans_gph)[is.na(names(intmeans_gph))] <- sp.grps
    ncols <- grep("\\.", names(intmeans_nph))
    ncols <- ncols[colSums(intmeans_nph[, ncols] != 0)>3]
    fig <- function() {
      mapBy2Groups(
        df = intmeans_nph[, ncols],
        lon = intmeans_nph$Lon_M,
        lat = intmeans_nph$Lat_M,
        nrows = c(3, 4)[short +
                          1]
      )
    }
    figu(
      "Acoustic density for each species group.  Groups are defined by",
      " length cut offs (L) in mm or ages (A).",
      "  Darker and larger circles indicate higher density.",
      FIG = fig,
      newpage = "port"
    )
    gcols <- grep("\\.", names(intmeans_gph))
    gcols <- gcols[colSums(intmeans_nph[, gcols] != 0)>3]
    fig <- function() {
      mapBy2Groups(
        df = intmeans_gph[, gcols],
        lon = intmeans_gph$Lon_M,
        lat = intmeans_gph$Lat_M,
        nrows = c(3, 4)[short +
                          1]
      )
    }
    figu(
      "Acoustic biomass for each species group.  Groups are defined by",
      " length cut offs (L) in mm or ages (A).",
      "  Darker and larger circles indicate greater biomass.",
      FIG = fig,
      newpage = "port"
    )
    areas <-
      data.frame(region = region,
                 regArea = regArea,
                 stringsAsFactors = FALSE)
    nphests <- intmeans_nph %>% select(-regarea) %>% gather(spgrp,
                                                            nph,
                                                            -region,
                                                            -Region_name,
                                                            -Interval,
                                                            -depth_botmid,-Lat_M,
                                                            -Lon_M) %>% split(.$spgrp) %>% purrr::map(
                                                              stratClust,
                                                              stratum = "region",
                                                              cluster = "Region_name",
                                                              response = "nph",
                                                              sizedf = areas,
                                                              size = "regArea"
                                                            )
    nphTrans <- purrr::map_df(nphests, "Cluster", .id = "spgrp")
    nphRegion <- purrr::map_df(nphests, "Stratum", .id = "spgrp")
    nphLake <- purrr::map_df(nphests, "Population", .id = "spgrp")
    gphests <- intmeans_gph %>% select(-regarea) %>% gather(spgrp,
                                                            gph,
                                                            -region,
                                                            -Region_name,
                                                            -Interval,
                                                            -depth_botmid,-Lat_M,
                                                            -Lon_M) %>% split(.$spgrp) %>% purrr::map(
                                                              stratClust,
                                                              stratum = "region",
                                                              cluster = "Region_name",
                                                              response = "gph",
                                                              sizedf = areas,
                                                              size = "regArea"
                                                            )
    gphTrans <- purrr::map_df(gphests, "Cluster", .id = "spgrp")
    gphRegion <- purrr::map_df(gphests, "Stratum", .id = "spgrp")
    gphLake <- purrr::map_df(gphests, "Population", .id = "spgrp")
    Regions <-
      bind_rows(nph = nphRegion, gph = gphRegion, .id = "metric") %>%
      select(
        metric,
        spgrp,
        region = h,
        n.intervals = m_h,
        reg.total = y_h,
        n.transects = n_h,
        reg.mean = ybar_h,
        reg.se.mean = s_ybar_h,
        area = A_h,
        rel.area = W_h
      )
    Lakes <-
      bind_rows(count = nphLake,
                biomass = gphLake,
                .id = "metric") %>%
      gather(estimate, value, ybar_str, s_ybar_str, ytot_str,
             s_ytot_str) %>% mutate(
               level = ifelse(grepl("ybar",
                                    estimate), "mean", "total"),
               value = ifelse(level ==
                                "total", value / 1e+06, value),
               units = case_when(
                 metric ==
                   "biomass" &
                   level == "mean" ~ "gph",
                 metric == "biomass" &
                   level == "total" ~ "t",
                 metric == "count" & level ==
                   "mean" ~ "nph",
                 metric == "count" & level == "total" ~
                   "millions"
               ),
               type = ifelse(substring(estimate, 1, 1) ==
                               "s", "SE", "Estimate")
             ) %>% select(-estimate) %>% spread(type,
                                                value) %>% mutate(RSE = 100 * SE /
                                                                    Estimate) %>% select(metric,
                                                                                         level, units, area = A, spgrp, Estimate, SE, RSE) %>%
      arrange(metric, level, units)
    save2csv <-
      c(
        "Regions",
        "Lakes",
        "intmeans_nph",
        "intmeans_gph",
        "intlaymeans_nph",
        "intlaymeans_gph",
        "svts5"
      )
    outfiles <- paste0(maindir,
                       "L",
                       LAKE,
                       " Y",
                       YEAR,
                       " ",
                       descr,
                       " ",
                       save2csv,
                       " ",
                       lubridate::today(),
                       ".csv")
    invisible(lapply(seq(save2csv), function(i)
      write.csv(eval(
        parse(text = save2csv[i]),
      ),
      outfiles[i], row.names = F)))
    newrdat <- paste0("L", LAKE, " Y", YEAR, " ", descr)
    save(list = save2csv,
         file = paste0(maindir, newrdat, ".RData"))
    bp <- with(Regions, tapply(reg.mean * area / 1e+06, list(region,
                                                             spgrp, metric), mean))
    mypalette <- RColorBrewer::brewer.pal(6, "Set3")
    fig <- function() {
      par(
        mar = c(4, 5, 0, 1),
        oma = c(0, 0, 2, 0),
        mfrow = c(1,
                  2),
        cex = 1.2
      )
      barplot(
        bp[, , "nph"],
        col = mypalette,
        horiz = TRUE,
        las = 1,
        xlab = "Number of fish  (millions)"
      )
      barplot(
        bp[, , "gph"],
        col = mypalette,
        horiz = TRUE,
        las = 1,
        xlab = "Biomass of fish  (t)",
        legend.text = TRUE,
        args.legend = list(x = "topright")
      )
    }
    figu(
      "Acoustic survey lakewide estimates in number (left) and",
      " biomass (right) for each species group.",
      "  Groups are defined by length cut offs (L) in mm or ages (A).",
      "  Colors are used to identify contributions from different regions",
      FIG = fig,
      h = 5.8,
      w = 9,
      newpage = "land"
    )
    tab <- mutate_if(Lakes, is.numeric, function(x)
      format(round(x),
             big.mark = ","))
    tabl(
      "Lakewide estimates in biomass density (g per ha), biomass(t), fish",
      " density (number per ha), and number (millions) for each species group.",
      "  Groups are defined by length cut offs (L) in mm or ages (A).",
      TAB = tab
    )
    endrtf()
  }


