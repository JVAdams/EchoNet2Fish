## ----install_package, eval=FALSE----------------------------------------------
#  devtools::install_github("dmwarn/EchoNet2Fish")

## ----load_package1, message=FALSE---------------------------------------------
library(EchoNet2Fish)

## ----refcsv, echo=FALSE-------------------------------------------------------
refdir <- "C:/Temp"
refdat <- data.frame(
  LAKE = c(3, 3, 2), 
  YEAR = c(2013, 2014, 2014), 
  subdir = c("H13", "H14", "M14"), 
  svsubdir = c("SV", "SV", "SV"), 
  tssubdir = c("TS", "TS", "TS"), 
  optropf = c("H.mtr.op13", "H.mtr.op14", "MtrawlOp"), 
  trcatchf = c("H.catch13", "H.catch14", "M.catch"), 
  trlff = c("Htr_lf13", "H.tr_lf14", "M.tr_lf"), 
  keysp1 = c(NA, NA, 106), 
  keyfile1 = c(NA, NA, "aleagelenkey"), 
  keysp2 = c(NA, NA, NA), 
  keyfile2 = c(NA, NA, NA))
write.csv(refdat, paste(refdir, "Reference.csv", sep="/"))
knitr::kable(refdat)

