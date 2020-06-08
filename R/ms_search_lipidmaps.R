#' @title Do a MS search on the LMSD database from LipidMaps
#'
#' @description Search the LMSD database from LipidMaps (http://www.lipidmaps.org/).
#'
#' @param ExactMass The exact mass to search.
#' @param ExactMassOffSet The search window in dalton.
#' @param Name Optional parameter to filter the search results. Name is search in any part of common name.
#' @param Formula Optional parameter to filter the search results. Formula is search in any part of formula.
#' @param CoreClass Optional parameter to filter the search results. At the moment there are 8 core classes defined (1-8). 
#' See detail section for more information.
#'
#' @return he function returns a dataframe with the search results. If there is a connection problem NULL is returned and a 
#' warning message showing the status code of the connection.
#' 
#' @details This function uses the Programmatic Access functionality of LipidMaps to search the LMSD database. 
#' There are 8 core classes defined :
#'  \describe{
#'    \item{1:}{Fatty Acids [FA]}
#'    \item{2:}{Glycerolipids [GL]}
#'    \item{3:}{Glycerophospholipids [GP]}
#'    \item{4:}{Sphingolipids [SP]}
#'    \item{5:}{Sterol Lipids [ST]}
#'    \item{6:}{Prenol Lipids [PR]}
#'    \item{7:}{Saccharolipids [SL]}
#'    \item{8:}{Polyketides [PK]}
#'  }
#' 
#' @export
#' @importFrom curl new_handle handle_setform curl_fetch_memory
#' @importFrom utils read.table
#'
#' @author Rico Derks
#' @examples 
#' 
#' results <- ms_search_lipidmaps(ExactMass = 537.37944, ExactMassOffSet = 0.01)
#' 
ms_search_lipidmaps <- function(ExactMass = NULL, 
                                ExactMassOffSet = 0.01,
                                Name = NULL,
                                Formula = NULL,
                                CoreClass = NULL) {
  # make sure not to connect more than once per 20 seconds
  # using package curl for the communication with lipidmaps
  
  ### do some checking if all is fine
  # make sure ExactMass is set and set as number
  if (is.null(ExactMass)) {
    stop("'ExactMass' not set!")
  } else if (is.numeric(ExactMass)) {
    # is number positive
    if (ExactMass > 0) {
      # for the webform it needs to be character
      ExactMass <- as.character(ExactMass)
    } else {
      stop("'ExactMass' should be possitive!")
    }
  } else {
    stop("'ExactMass' needs to be a number!")
  }
  
  # make sure ExactMassOffSet is set and set as number
  if (is.numeric(ExactMassOffSet)) {
    if (ExactMassOffSet > 0) {
      # for the webform it needs to be character
      ExactMassOffSet <- as.character(ExactMassOffSet)
    } else {
      stop("'ExactMassOffSet' should be possitive!")
    }
  } else {
    stop("'ExactMassOffSet' needs to be a number!")
  }
  
  # is 'Name' set correctly
  if (is.null(Name)) {
    Name <- ""
  } else if (!is.character(Name)) {
    Name <- as.character(Name)
  }
  
  # is 'Formula' set correctly
  if (is.null(Formula)) {
    Formula <- ""
  } else if (!is.character(Formula)) {
    Formula <- as.character(Formula)
  }
  
  # is 'CoreClass' set correctly
  if (is.null(CoreClass)) {
    CoreClass <- ""
  } else if (is.numeric(CoreClass)) {
    # should be between 1 and 8
    if (CoreClass >= 1 & CoreClass <= 8) {
      CoreClass <- as.character(as.integer(CoreClass))
    } else {
      stop("'CoreClass' needs to be between 1 and 8!")
    }
  } else {
    stop("'CoreClass' needs to be a number!")
  }
  
  ### define some things
  # define the website
  http_lipidmaps <- "http://www.lipidmaps.org/data/structure/LMSDSearch.php"
  
  ### do the search
  # create a new handle to set the form variables
  h <- new_handle()
  handle_setform(handle = h, 
                 Mode = "ProcessTextSearch",
                 OutputMode = "File",
                 OutputType = "TSV",
                 ExactMass = ExactMass,
                 ExactMassOffSet = ExactMassOffSet,
                 Name = Name,
                 Formula = Formula,
                 CoreClass = CoreClass)
  
  # do the request and get the results
  req <- curl_fetch_memory(url = http_lipidmaps, 
                           handle = h)
  
  if (req$status_code == 200) {
    # if correct response continue to parse the document
    results <- read.table(text = rawToChar(req$content),
                          sep = "\t", 
                          header = TRUE);
  } else {
    # if status code is not 200 return nothing and add warning with code
    warning("Status code is ", req$status_code)
    results <- NULL
  }
  
  # return the results
  return(results)
}
