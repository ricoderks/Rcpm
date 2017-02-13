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
#  settings$std_match$mz_tol <- 10 # this needs to be quite large for this strategy to work. Only scans inside this is used AFAIK. The centwave ppm setting will limit appropiately
#  settings$std_match$mz_tol_unit <- "ppm"
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
#                                           mz_tol = settings$std_match$mz_tol,
#                                           mz_tol_unit = settings$std_match$mz_tol_unit,
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

## ----best_matching, eval=FALSE-------------------------------------------
#  # we add a row index we can match by later
#  data <- data %>% mutate(peakTable = map(peakTable, ~ mutate(.x,
#                                                      row = 1:nrow(.x))))
#  
#  data <- data %>% mutate(matched_peaks = map2(.x = stds,
#                                              .y = peakTable,
#                                              ~ closest_match(stds = .x,
#                                                              peakTable = .y,
#                                                              rt_tol = settings$std_match$rt_tol,
#                                                              mz_tol = settings$std_match$mz_tol,
#                                                              mz_tol_unit = settings$std_match$mz_tol_unit) %>%
#                                                data_frame(row = .) %>%                 # a vector is returned so convert to data frame
#                                                bind_cols(.x) %>%                       # bind the returned vector (now data frame) to data frame stds
#                                                left_join(.y,
#                                                          by = "row",
#                                                          suffix = c(".stds", ".peaks")) %>%    # return all rows from x, and all columns from x and y.
#                                                mutate(found = ifelse(is.na(row), FALSE, TRUE)) %>%   # if row is NA the peak was not found, create column with true / false if peak was found
#                                                select(-row)))                # remove column row
#  
#  # show the matched peaks from the first sample with all it's info
#  data$matched_peaks[[1]] %>% kable

