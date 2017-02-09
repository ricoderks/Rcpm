#' @title Reshape data frame for the summary_fit function.
#' 
#' @description This functions reshapes the data frame for making a bar plot 
#' graph with ggplot2.
#'
#' @param mydata The data frame to be reshaped to a long data frame
#'
#' @return reshaped data frame. A long data frame is created.
#' 
#' @details It is not nescessary to call this function. This function is called 
#' from summary_fit. The reshaping is done with the \link{melt} function from the 
#' package reshape2
#' 
#' @importFrom reshape2 melt
#'
#' @author Rico Derks
summary_fit_data <- function(mydata) {
  # input checking
  # mydata needs to be a data frame
  if (!is.data.frame(mydata)) {
    stop("mydata needs to be a data frame!")
  }
  
  # colnames need to be PC, R2cum and Q2cum
  col_names <- c("PC", "R2cum", "Q2cum")
  if (length(which(col_names %in% colnames(mydata))) != 3) {
    stop("mydata should contain only the column names PC, R2cum and Q2cum!")
  }
  
	return(reshape2::melt(mydata, id.vars = "PC"))
}
