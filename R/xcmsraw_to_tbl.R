#' Convert raw data into a tibble of xcmsRaw objects.
#'
#' @param files character vector of file names/paths.
#' @param ... further arguments to \code{\link{xcmsRaw}}.
#'
#' @return A \code{\link{tibble}} containing the columns: 
#' \itemize{
#'   \item \strong{sample_name:} Filename without path.
#'   \item \strong{raw:} The xcmsRaw objects.
#' }
#' 
#' @export
#' @import purrr
#' @import dplyr
#' @import xcms
#'
#' @examples
xcmsraw_to_tbl <- function(files, ...){
  data <- files %>% 
    data_frame(sample_name = .) %>% 
    dplyr::as.tbl %>%                  # string to tbl
    dplyr::mutate(raw = purrr::map(.$sample_name, ~ xcms::xcmsRaw(.x, ...))) %>%           # read raw data
    dplyr::select(sample_name, raw)                       # just re-arrange for readability
  
  return(data)
}