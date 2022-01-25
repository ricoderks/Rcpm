#' @title Write mgf file
#' 
#' @description Write a dataframe to a mgf file.
#' 
#' @param filename filename of the mgf file
#' @param mgf_data data frame containing all the data. Each row is a MSMS spectrum.
#' 
#' @details Columns which will be written to the mgf file: scans, pepmass, mslevel, 
#'     charge, rtminutes, ion, msms_spectrum
#' 
#' @export
#' 
#' @author Rico Derks
#' 
write_mgf <- function(filename = NULL,
                      mgf_data = NULL) {
  ## sanity checks
  # is a filename given
  if (is.null(filename)) {
    stop("No filename given!")
  }
  
  # does the file exist
  # if (file.exists(filename)) {
  #   stop("File already exists!")
  # }
  # 
  # write to file
  # define connection
  conn <- file(description = filename,
               open = "w")
  
  # this is probably horribly slow
  for (i in 1:nrow(mgf_data)) {
    writeLines(text = "BEGIN IONS", con = conn)
    writeLines(text = paste0("SCANS=", mgf_data$scans[i]), con = conn)
    writeLines(text = paste0("PEPMASS=", mgf_data$pepmass[i]), con = conn)
    writeLines(text = paste0("MSLEVEL=", mgf_data$mslevel[i]), con = conn)
    writeLines(text = paste0("CHARGE=", mgf_data$charge[i]), con = conn)
    writeLines(text = paste0("RTINMINUTES=", mgf_data$rtminutes[i]), con = conn)
    writeLines(text = paste0("ION=", mgf_data$ion[i]), con = conn)
    
    # write MSMS spectrum
    writeLines(text = gsub(pattern = ",",
                           replacement = "\n",
                           x = mgf_data$msms_spectrum[i]),
               con = conn)
    
    writeLines(text = "END IONS", con = conn)
    
    # end with empty line
    writeLines(text = "", con = conn)
  }
  
  # close connection
  close(conn)
}