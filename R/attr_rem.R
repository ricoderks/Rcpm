#' @title Remove attributes
#' 
#' @description Remove attributes from objects
#'
#' @param x The object to remove attribute from
#' @param which name of the attribute to remove
#'
#' @return "x" without attribute "which"
#' 
#' @details This function comes from the package massageR from Jan Stanstrup
#'
#' @author Jan Stanstrup
#' @author Rico Derks
#' @references https://github.com/stanstrup/massageR
attr_rem <- function(x, which) {
  attr(x, which) <- NULL
  return(x)
}