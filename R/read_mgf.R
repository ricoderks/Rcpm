#' @title Read mgf file
#' 
#' @description Read a mgf file.
#' 
#' @param filename filename of the mgf file.
#' 
#' @details An extra column **idx** is created in the data frame.
#' 
#' @returns a dataframe 
#' 
#' @export
#' 
#' @importFrom data.table fread
#' 
#' @author Rico Derks
#' 
read_mgf <- function(filename = NULL) {
  ## sanity checks
  # is a filename given
  if (is.null(filename)) {
    stop("No filename given!")
  }
  
  # does the file exist
  if (!file.exists(filename)) {
    stop("File doesn't exists!")
  }
  ###############################################################
  # define connection
  # read the file
  buf <-
    as.matrix(fread(
      filename,
      header = F,
      sep = '~',
      blank.lines.skip = T
    ))
  
  ind1 <- grep(pattern = "SCANS=", x = buf)
  ind2 <- ind1 + 1
  ind3 <- ind1 + 2
  ind4 <- ind1 + 3
  ind5 <- ind1 + 4
  ind6 <- ind1 + 5
  ind7 <- grep(pattern = "END IONS", x = buf) - 1
  
  mgf_data <- as.data.frame(matrix(ncol = 8, nrow = length(ind2)))
  
  names(mgf_data) <-
    c(
      "idx",
      "scans",
      "pepmass",
      "mslevel",
      "charge",
      "rtminutes",
      "ion",
      "msms_spectrum"
    )
  
  mgf_data$scans <- gsub("\n", "", gsub("SCANS=", "", buf[ind1, ]))
  mgf_data$pepmass <- gsub("PEPMASS=", "", buf[ind2, ])
  mgf_data$mslevel <- gsub("MSLEVEL=", "", buf[ind3, ])
  mgf_data$charge <- gsub("CHARGE=", "", buf[ind4, ])
  mgf_data$rtminutes <- gsub("RTINMINUTES=", "", buf[ind5, ])
  mgf_data$ion <- gsub("ION=", "", buf[ind6, ])
  #ind6
  
  mgf_data$msms_spectrum <- sapply(1:length(ind6), function(i) {
    paste(buf[(ind6[i] + 1):ind7[i], ], collapse = ",")
  })
  
  # add unique ID
  mgf_data$idx <- 1:length(ind2)
  
  return(mgf_data)
}