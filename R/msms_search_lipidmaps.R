#' @title Do a MSMS search on the Glycerophospholipid MS/MS Prediction database from LipidMaps
#'
#' @description Search the Glycerophospholipid MS/MS Prediction database from LipidMaps (http://www.lipidmaps.org/).
#'
#' @param lipidclass define the lipidclass you want to search, currently available phospholipics and glycerolipids
#' @param intensity_threshold set the intensity threshold
#' @param prec_tol precursor ion mass tolerance (+/- m/z)
#' @param prod_tol product ion mass tolerance (+/- m/z)
#' @param headgroup search all headgroups of a lipidclass, see details for more inforamtion
#' @param ion set the ion type (see details for more info)
#' @param min_matches how many matches should the search at least have
#' @param LIMIT limit the chains to 'even' or 'odd' and 'even' (All)
#' @param peaklist in PKL format
#' See detail section for more information.
#'
#' @return The function returns a dataframe with the search results. If there is a connection problem NULL is returned and a 
#' warning message showing the status code of the connection.
#' 
#' @details This function uses the Programmatic Access functionality of LipidMaps to search the LMSD database.
#'
#' Phospholipids:
#' 
#' Headgroups are defined as:  "Any" or one off "PC", "PA", "PS", "PE", "PG", "PI"
#'
#' The ion types are defined as: 
#' * chloride : [M-H]- / [M+Cl]-
#' * acetate : [M-H]- / [M+Acetate-H]-
#' * formate : [M-H]- / [M+Formate-H]-
#' 
#' Glycerolipids:
#' 
#' Headgroups are defined as:  "Any" or one off "MD", "DG", "TG"
#'
#' The ion types are defined as: 
#' * ammonium : [M+NH4]+
#'
#' @importFrom curl new_handle handle_setform curl_fetch_memory
#' @importFrom utils read.table
#' @importFrom xml2 read_html
#' @importFrom rvest html_node html_table
#'
#' @author Rico Derks
#' @examples 
#' mypeaks <- c("818.59 10000 1\n758.57 20\n687.50 20\n506.33 50\n494.33 15\n488.31 2")
#' 
#' results <- msms_search_lipidmaps(lipidclass = "phospholipids",
#'                      intensity_threshold = 10,
#'                      prec_tol = 0.01,
#'                      prod_tol = 0.01,
#'                      headgroup = "Any",
#'                      ion = "formate",
#'                      min_matches = 2,
#'                      LIMIT = "All",
#'                      peaklist = mypeaks)

#' @export
#' @rdname msms_search_lipidmaps
msms_search_lipidmaps <- function(lipidclass = c("glycerolipids", "phospholipids"),
                                  intensity_threshold = 10, 
                                  prec_tol = 0.01,
                                  prod_tol = 0.01,
                                  headgroup = "",
                                  ion = "",
                                  min_matches = 5,
                                  LIMIT = c("All", "even"),
                                  peaklist = NULL) {
  
  lipidclass <- match.arg(arg = lipidclass)
  
  if (is.null(peaklist)) {
    stop("No 'peaklist' given!")
  }
  
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
    if (min_matches > 1) {
      # for the webform it needs to be character
      min_matches <- as.character(min_matches)
    } else {
      stop("'min_matches' should be at least 2!")
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
  
  switch(lipidclass,
         "glycerolipids" = msms_search_lipidmaps.glycerolipids(intensity_threshold = intensity_threshold, 
                                                               prec_tol = prec_tol,
                                                               prod_tol = prod_tol,
                                                               headgroup = headgroup,
                                                               ion = ion,
                                                               min_matches = min_matches,
                                                               LIMIT = LIMIT,
                                                               peaklist = peaklist),
         "phospholipids" = msms_search_lipidmaps.phospholipids(intensity_threshold = intensity_threshold, 
                                                               prec_tol = prec_tol,
                                                               prod_tol = prod_tol,
                                                               headgroup = headgroup,
                                                               ion = ion,
                                                               min_matches = min_matches,
                                                               LIMIT = LIMIT,
                                                               peaklist = peaklist))
}


## glycerolipids
msms_search_lipidmaps.glycerolipids <- function(intensity_threshold = 10, 
                                                prec_tol = 0.01,
                                                prod_tol = 0.01,
                                                headgroup = c("All", "MG", "DG", "TG"),
                                                ion = c("ammonium"),
                                                min_matches = 5,
                                                LIMIT = c("All", "even"),
                                                peaklist = NULL) {
  # make sure not to connect more than once per 20 seconds
  # using package curl for the communication with lipidmaps
  
  # check if the ion is set correctly
  ion <- match.arg(arg = ion)
  # check if the headgroup is set correctly
  if (is.null(headgroup)) {
    headgroup <- "All"
  }
  headgroup <- match.arg(arg = headgroup)
  
  ### define some things
  # define the website
  http_lipidmaps <- "https://www.lipidmaps.org/tools/ms/GL_prod_search.php"
  
  ### do the search
  # create a new handle to set the form variables
  h <- new_handle()
  handle_setform(handle = h,
                 intensity_threshold = intensity_threshold, 
                 prec_tol = prec_tol,
                 prod_tol = prod_tol,
                 headgroup = headgroup,
                 ion = ion,
                 min_matches = min_matches,
                 LIMIT = LIMIT,
                 peaklist = peaklist)
  
  # do the request and get the results
  req <- curl_fetch_memory(url = http_lipidmaps,
                           handle = h)
  
  if (req$status_code == 200) {
    # if correct response continue to parse the document
    results <- rawToChar(req$content) %>%
      read_html() %>%
      html_node("table") %>%
      html_table()
  } else {
    # if status code is not 200 return nothing and add warning with code
    warning("Status code is ", req$status_code)
    results <- NULL
  }
  
  # return the results
  return(results)
}


## phospholipids
msms_search_lipidmaps.phospholipids <- function(intensity_threshold = 10, 
                                                prec_tol = 0.01,
                                                prod_tol = 0.01,
                                                headgroup = c("Any", "PC", "PA", "PS", "PE", "PG", "PI"),
                                                ion = c("chloride", "acetate", "formate"),
                                                min_matches = 5,
                                                LIMIT = c("All", "even"),
                                                peaklist = NULL) {
  # make sure not to connect more than once per 20 seconds
  # using package curl for the communication with lipidmaps
  
  # check if the ion is set correctly
  ion <- match.arg(arg = ion)
  # check if the headgroup is set correctly
  if (is.null(headgroup)) {
    headgroup <- "Any"
  }
  headgroup <- match.arg(arg = headgroup)
  
  ### define some things
  # define the website
  http_lipidmaps <- "https://www.lipidmaps.org/tools/ms/GP_prod_search.php"
  
  ### do the search
  # create a new handle to set the form variables
  h <-new_handle()
  handle_setform(handle = h,
                 intensity_threshold = intensity_threshold, 
                 prec_tol = prec_tol,
                 prod_tol = prod_tol,
                 headgroup = headgroup,
                 ion = ion,
                 min_matches = min_matches,
                 LIMIT = LIMIT,
                 peaklist = peaklist)
  
  # do the request and get the results
  req <- curl_fetch_memory(url = http_lipidmaps, 
                           handle = h)
  
  if (req$status_code == 200) {
    # if correct response continue to parse the document
    results <- rawToChar(req$content) %>%
      read_html() %>%
      html_node("table") %>%
      html_table()
  } else {
    # if status code is not 200 return nothing and add warning with code
    warning("Status code is ", req$status_code)
    results <- NULL
  }
  
  # return the results
  return(results)
}
