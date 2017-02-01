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
    }
  }
  
  ### define some things
  # define the website
  http_lipidmaps <- "http://www.lipidmaps.org/data/structure/LMSDSearch.php"
  
  ### do the search
  # create a new handle to set the form variables
  h <- curl::new_handle()
  curl::handle_setform(h, 
                 Mode = "ProcessTextSearch",
                 OutputMode = "File",
                 OutputType = "TSV",
                 ExactMass = ExactMass,
                 ExactMassOffSet = ExactMassOffSet,
                 Name = Name,
                 Formula = Formula,
                 CoreClass = CoreClass)
  
  # do the request and get the results
  req <- curl::curl_fetch_memory(http_lipidmaps, h)
  
  if (req$status_code == 200) {
    # if correct response continue to parse the document
    results <- read.table(text = rawToChar(req$content), sep = "\t", header = TRUE);
  } else {
    # if status code is not 200 return nothing and add warning with code
    warning("Status code is ", req$status_code)
    results <- NULL
  }
  
  # return the results
  return(results)
}
