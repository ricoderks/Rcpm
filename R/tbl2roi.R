#' @title Convert a list of peaks (rt / m/z pairs) to a Region of Interest (ROI) list for use
#' with \code{\link{findPeaks}}
#' 
#' @description  Convert a list of peaks (rt / m/z pairs) to a Region of Interest (ROI) list for use
#' with \code{\link{findPeaks}}
#'
#' @param tbl \code{\link{tibble}} containing the columns "rt" and "mz". rt needs to be in seconds.
#' @param raw \code{\link{xcmsRaw}} object to create ROI for. It needs to be a specific 
#' \code{\link{xcmsRaw}} to match retention times to scan nubmers.
#' @param ppm ppm tolerance for the generated ROI.
#' @param rt_tol Retention time tolerance (in sec!) for the generated ROI.
#'
#' @return List containing the ROIs. Each list contains mz, mzmin, mzmax, scmin, scmax, 
#' length (set to -1, not used by centWave) and intensity (set to -1, not used by centWave) columns.
#' 
#' @details This is based on the work of Jan Stanstrup (see references).
#' 
#' @export
#' @importFrom dplyr %>% rowwise transmute ungroup mutate
#' @importFrom purrr by_row map
#'
#' @author Jan Stanstrup
#' @author Rico Derks
#' @references https://github.com/stanstrup/QC4Metabolomics
tbl2roi <- function(tbl, raw, ppm, rt_tol) {
  
  rt <- mz <- .out <- NULL
  
  out <- tbl %>% 
    rowwise %>% 
    transmute(mz = mz, # not used!
              mzmin = mz - ppm * mz * 1E-6, 
              mzmax = mz + ppm * mz * 1E-6,
              scmin = which.min(abs((rt - rt_tol) - raw@scantime)),
              scmax = which.min(abs((rt + rt_tol) - raw@scantime)),
              length = -1, # not used!
              intensity = -1  # not used!
    ) %>%
    ungroup %>% 
    by_row(as.list) %>%
    mutate(.out = map(.out, ~ attr_rem(.x, "indices"))) %>%        # we get some indices attribute. Dunno why. But lets remove it.
    `[[`(".out")
  
  return(out)
}