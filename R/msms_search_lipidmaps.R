#' @title Do a MSMS search on the Glycerophospholipid MS/MS Prediction database from LipidMaps
#'
#' @description Search the Glycerophospholipid MS/MS Prediction database from LipidMaps (http://www.lipidmaps.org/).
#'
#' @param intensity_threshold set the intensity threshold
#' @param prec_tol precursor ion mass tolerance (+/- m/z)
#' @param prod_tol product ion mass tolerance (+/- m/z)
#' @param headgroup search all headgroups or one off "PC", "PA", "PS", "PE", "PG", "PI
#' @param ion set the ion type (see details for more info)
#' @param min_matches how many matches should the search at least have
#' @param LIMIT limit the chains to 'even' or 'odd' and 'even' (All)
#' @param peaklist in PKL format
#' See detail section for more information.
#'
#' @return he function returns a dataframe with the search results. If there is a connection problem NULL is returned and a 
#' warning message showing the status code of the connection.
#' 
#' @details This function uses the Programmatic Access functionality of LipidMaps to search the LMSD database.
#'
#' The ion types are defined as: 
#' * chloride : [M-H]- / [M+Cl]-
#' * acetate : [M-H]- / [M+Acetate-H]-
#' * formate : [M-H]- / [M+Formate-H]-
#'
#' @export
#' @importFrom curl new_handle handle_setform curl_fetch_memory
#' @importFrom utils read.table
#' @importFrom xml2 read_html
#' @importFrom rvest html_node html_table
#'
#' @author Rico Derks
#' @examples mypeaks <- c("818.59 10000 1\n758.57 20\n687.50 20\n506.33 50\n494.33 15\n488.31 2")
#' results <- msms_search_lipidmaps(intensity_threshold = 10,
#'                      prec_tol = 0.01,
#'                      prod_tol = 0.01,
#'                      headgroup = "All",
#'                      ion = "formate",
#'                      min_matches = 1,
#'                      LIMIT = "All",
#'                      peaklist = mypeaks)

msms_search_lipidmaps <- function(intensity_threshold = 10, 
                                  prec_tol = 0.01,
                                  prod_tol = 0.01,
                                  headgroup = c("All", "PC", "PA", "PS", "PE", "PG", "PI"),
                                  ion = c("chloride", "acetate", "formate"),
                                  min_matches = 5,
                                  LIMIT = c("All", "even"),
                                  peaklist = NULL) {
  # make sure not to connect more than once per 20 seconds
  # using package curl for the communication with lipidmaps
  
  ### do some checking if all is fine
  # make sure ExactMass is set and set as number
   if (is.numeric(intensity_threshold)) {
    # is number positive
    if (intensity_threshold > 0) {
      # for the webform it needs to be character
      intensity_threshold <- as.character(intensity_threshold)
    } else {
      stop("'intensity_threshold' should be possitive!")
    }
  } else {
    stop("'intensity_threshold' needs to be a number!")
  }

  # make sure prec_tol is set and set as number
  if (is.numeric(prec_tol)) {
    if (prec_tol > 0) {
      # for the webform it needs to be character
      prec_tol <- as.character(prec_tol)
    } else {
      stop("'prec_tol' should be possitive!")
    }
  } else {
    stop("'prec_tol' needs to be a number!")
  }
  
    # make sure min_matches is set and set as number
  if (is.numeric(min_matches)) {
    if (min_matches > 0) {
      # for the webform it needs to be character
      min_matches <- as.character(min_matches)
    } else {
      stop("'min_matches' should be possitive!")
    }
  } else {
    stop("'min_matches' needs to be a number!")
  }
  
      # make sure prod_tol is set and set as number
  if (is.numeric(prod_tol)) {
    if (prod_tol > 0) {
      # for the webform it needs to be character
      prod_tol <- as.character(prod_tol)
    } else {
      stop("'prod_tol' should be possitive!")
    }
  } else {
    stop("'prod_tol' needs to be a number!")
  }
 
  if (is.null(headgroup)) {
    headgroup <- ""
    warning("headgroup set to 'All'!\n")
  } else {
    if (is.character(headgroup[1])) {
      headgroup <- match.arg(headgroup)
      if (headgroup == "All") {
        headgroup <- ""
      }
    } else {
      stop("headgroup should be character: 'All', 'PC', 'PA', 'PS', 'PE', 'PG' or 'PI'\n")
    }
  }
  
  ### define some things
  # define the website
  http_lipidmaps <- "http://www.lipidmaps.org/tools/ms/GP_prod_search.php"
  
  ### do the search
  # create a new handle to set the form variables
  h <- curl::new_handle()
  curl::handle_setform(h,
                       intensity_threshold = intensity_threshold, 
                       prec_tol = prec_tol,
                       prod_tol = prod_tol,
                       headgroup = headgroup,
                       ion = ion,
                       min_matches = min_matches,
                       LIMIT = LIMIT,
                       peaklist = peaklist)
  
  # do the request and get the results
  req <- curl::curl_fetch_memory(http_lipidmaps, h)
  
  if (req$status_code == 200) {
    # if correct response continue to parse the document
    results <- rawToChar(req$content) %>%
      xml2::read_html() %>%
      rvest::html_node("table") %>%
      rvest::html_table()
  } else {
    # if status code is not 200 return nothing and add warning with code
    warning("Status code is ", req$status_code)
    results <- NULL
  }
  
  # return the results
  return(results)
}
