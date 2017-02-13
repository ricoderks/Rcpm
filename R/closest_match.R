#' @title Match list of standards to peak table
#'
#' @description 
#' This function will match a table of standard compounds and a peak table by m/z and retention time.
#'
#' @param stds \code{\link{tibble}} of standards to match to a peak table
#' @param peakTable \code{\link{tibble}} containing peak table supplied by \code{\link{findPeaks}} (but converted to \code{\link{tibble}}/\code{\link{data.frame}}).
#' @param rt_tol Retention time tolerance for matching peaks. Pay attention to the unit of your tables.
#' rt_tol should match and stds and peakTable should use same units (i.e. minutes of seconds).
#' @param mz_tol m/z tolerance.
#' @param mz_tol_unit set the unit of the m/z tolerance, 'ppm' or 'Da'.
#' @param rt_col Character string giving the column containing the retention times. Must be same in standards and peak table.
#' @param mz_col Character string giving the column containing the m/z values. Must be same in standards and peak table. 
#' @param int_col Character string giving the column containing the intensities in the peak table. 
#'
#' @return A vector having the length equivalent to the number of rows in stds giving the indices of the hits in peakTable.
#' 
#' @details 
#' This function will match a table of standard compounds and a peak table by m/z and retention time.
#' If there is more than one possible hit the highest intensity peak will be chosen.
#' 
#' @export
#'
#' @importFrom dplyr %>% slice
#'
#' @author Jan Stanstrup
#' @author Rico Derks
#' @references https://github.com/stanstrup/QC4Metabolomics
closest_match <- function(stds, 
                          peakTable, 
                          rt_tol = 0.25, 
                          mz_tol = 30, 
                          mz_tol_unit = c("Da", "ppm")
                          rt_col = "rt", 
                          mz_col = "mz", 
                          int_col = "into"){

  if (is.null(mz_tol_unit)) {
    mz_tol_unit <- "Da"
    warning("mz_tol_unit set to Da!\n")
  }
  
  if (is.character(mz_tol_unit[1])) {
    mz_tol_unit <- match.arg(mz_tol_unit)
  } else {
    stop("mz_tol_unit should be character: 'ppm' or 'Da'\n")
  }
    
  indices <- rep_len(as.numeric(NA), nrow(stds))
  
  peakTable_mz <- peakTable %>% `[[`(mz_col)
  peakTable_rt <- peakTable %>% `[[`(rt_col)
  
  for(i in 1:nrow(stds)) {
    # this is for each standard / target
    stds_mz <- stds %>% slice(i) %>% `[[`(mz_col)
    stds_rt <- stds %>% slice(i) %>% `[[`(rt_col)

    if (mz_tol_unit == "ppm") {
      # if unit m/z tolerance is in ppm
      idx <- (abs(stds_rt - peakTable_rt) < rt_tol) & ((abs(stds_mz - peakTable_mz) / stds_mz) * 1E6 <  mz_tol)
    } else {
      # if unit m/z tolerance is in Da
      idx <- (abs(stds_rt - peakTable_rt) < rt_tol) & (abs(stds_mz - peakTable_mz) <  mz_tol)
    }
    
    # no match is found
    if (sum(idx) == 0) { 
      next # we filled the vector with NA so we don't need to do anyting
    }
    
    # 1 match is found
    if (sum(idx) == 1) {
      indices[i] <- which(idx) 
      next
    }
    
    # More than one match is found. We take the highest intensity peak then
    if (sum(idx) > 1) {
      idx2 <- peakTable %>% 
        slice(which(idx)) %>% 
        extract2(int_col) %>% 
        which.max
      indices[i] <- which(idx)[idx2]
      next
    }
  } # end for loop
  
  return(indices)
}