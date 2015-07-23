#' Reshape data frame for the summary_fit function.
#' @description This functions reshapes the data frame for making a bar plot
#'   graph with ggplot2.
#' @param mydata
#' @return reshaped data frame. A long data frame is created.
#' @details It is not nescessary to call this function. This function is called
#'   from summary_fit. The reshaping is done with the \link{melt} function from the
#'   package reshape2.
#' @author Rico Derks
summary_fit_data <- function(mydata) {
  return(reshape2::melt(mydata));
}
