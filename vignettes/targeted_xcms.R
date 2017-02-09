## ----load_libraries, eval=FALSE------------------------------------------
#  library(dplyr)
#  library(xcms)
#  library(Rcpm)
#  WorkDir <- "/home/rjederks/Documents/Projects/TargetedXCMS/"
#  DataDir <- paste0(WorkDir, "Data")

## ----process_settings, eval=FALSE----------------------------------------
#  settings <- list()
#  
#  settings$xcmsRaw$profparam <- list(step = 0.01)
#  
#  settings$std_match$ppm <- 10 # this needs to be quite large for this strategy to work. Only scans inside this is used AFAIK. The centwave ppm setting will limit appropiately
#  settings$std_match$rt_tol <- 10
#  
#  settings$findPeaks$method <- "centWave"
#  settings$findPeaks$snthr <- 10
#  settings$findPeaks$ppm <- 10
#  settings$findPeaks$peakwidth  <- c(5, 20)
#  settings$findPeaks$scanrange <- NULL      # use everyhting
#  settings$findPeaks$prefilter <- c(3, 1000)
#  settings$findPeaks$integrate <- 1
#  settings$findPeaks$verbose.columns <- TRUE
#  settings$findPeaks$fitgauss <- TRUE

## ----load_targeted_peaks, eval=FALSE-------------------------------------
#  target_peaks <- read.table(file = paste0(WorkDir, "141010_QCS.csv"),
#                             header = TRUE,
#                             sep = ",")
#  target_peaks %>% head(nrow(target_peaks))

## ----load_mzxml_data, eval=FALSE-----------------------------------------
#  list_mzxml <- list.files(path = DataDir,
#                           pattern = "^QCS.*[Mm][Zz][Xx][Mm][Ll]$",
#                           full.names = TRUE)
#  
#  raw_data <- xcmsraw_to_tbl(files = list_mzxml,
#                             profparam = settings$xcmsRaw)

