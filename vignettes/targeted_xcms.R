## ----load_libraries, eval=FALSE------------------------------------------
#  library(dplyr)
#  library(xcms)
#  library(Rcpm)
#  library(purrr)

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
#                             profparam = settings$xcmsRaw$profparam)

## ----merge_raw_standards, eval=FALSE-------------------------------------
#  data <- target_peaks %>%
#    mutate(rt = rt * 60) %>%
#    list %>%
#    rep(nrow(raw_data)) %>%
#    data_frame(stds = .) %>%
#    bind_cols(raw_data, .)
#  
#  # remove raw data datatable
#  rm(raw_data)

## ----add_roi, eval=FALSE-------------------------------------------------
#  data <- data %>%
#    mutate(ROI = map2(raw, stds, ~ tbl2roi(tbl = .y,
#                                           raw = .x,
#                                           ppm = settings$std_match$ppm,
#                                           rt_tol = settings$std_match$rt_tol)))

## ----detect_peaks, eval=FALSE--------------------------------------------
#  findPeaks_l <- lift_dl(findPeaks) # trick to make findPeaks accept a list of arguments.
#  
#  # run it
#  data <- data %>%
#    mutate(peakTable = map2(.x = raw,
#                            .y = ROI,
#                            .f = ~ findPeaks_l(settings$findPeaks,
#                                               object = .x,
#                                               ROI.list = .y) %>%
#                              as.data.frame %>%
#                              as.tbl))

