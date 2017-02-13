#' @title Convert a list of peaks (rt / m/z pairs) to a Region of Interest (ROI) list for use
#' with \code{\link{findPeaks}}
#' 
#' @description  Convert a list of peaks (rt / m/z pairs) to a Region of Interest (ROI) list for use
#' with \code{\link{findPeaks}}
#'
#' @param tbl \code{\link{tibble}} containing the columns "rt" and "mz". rt needs to be in seconds.
#' @param raw \code{\link{xcmsRaw}} object to create ROI for. It needs to be a specific 
#' \code{\link{xcmsRaw}} to match retention times to scan nubmers.
#' @param mz_tol m/z tolorance
#' @param mz_tol_unit set the unit of the m/z tolerance, 'ppm' or 'Da'.
#' @param rt_tol Retention time tolerance (in seconds!) for the generated ROI.
#'
#' @return List containing the ROIs. Each list contains m/z, mzmin, mzmax, scmin, scmax, 
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
tbl2roi <- function(tbl, raw, mz_tol = 0.005, mz_tol_unit = c("Da", "ppm"), rt_tol = 10) {

  rt <- mz <- .out <- NULL

  if (is.null(mz_tol_unit)) {
    mz_tol_unit <- "Da"
    warning("mz_tol_unit set to Da!\n")
  }
  
  if (is.character(mz_tol_unit[1])) {
    mz_tol_unit <- match.arg(mz_tol_unit)
  } else {
    stop("mz_tol_unit should be character: 'ppm' or 'Da'\n")
  }

  out <- tbl %>% 
    rowwise %>% 
    transmute(mz = mz, # not used!
              mzmin = ifelse(mz_tol_unit == "Da", mz - mz_tol, mz - mz_tol * mz * 1E-6),
              mzmax = ifelse(mz_tol_unit == "Da", mz + mz_tol, mz + mz_tol * mz * 1E-6),
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