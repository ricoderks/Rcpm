#' @title Create a Hotelling T2 ellipse for a PCA score plot per group
#' 
#' @description This function can be used to create a confidence (Hotelling T2) interval for a 
#' PCA score plot per group.
#' 
#' @param data data frame or table
#' @param x (name of) x vector
#' @param y (name of) y vector
#' @param alpha confidence interval
#' @param len number of points to create the ellipse
#' @param ellipse name of column of groups
#'
#' @return A data frame is returned with the points to create the ellipse.
#'
#' @details This is a helper function which is used to create a confidence (Hotelling T2) interval for a 
#' PCA score plot per group.
#'
#' @importFrom stats var qf
#' @importFrom dplyr pull
#'
#' @author Rico Derks, modified by Marco Bladergroen
#' 
#' # if ellipse is not NULL this function assumes you have data with a factorial column named as in 'ellipse' to draw multiple ellipses.
#' x and y should contain the name (string) of the columns of the data to be plotted. If ellipse is NULL the (single) ellipse is drawn according to data in x and y (vectors).
multi_ellipse <- function(data, x, y, alpha = 0.95, len = 200, ellipse = NULL) {
  result <- data.frame(el = NULL,
                       x = NULL,
                       y = NULL)
  
  if(!is.null(ellipse) && !ellipse==FALSE){
    ellipse <- ensym(ellipse)
    if(is.character(x)){
      x <- ensym(x)
    }
    if(is.character(y)){
      y <- ensym(y)
    }
    
    for(i in levels(pull(data[, as_label(ellipse)]))){
      filteredData <- data %>% filter({{ ellipse }} == i)
      x1 <- as.vector(t(filteredData[,x]))
      y1 <- as.vector(t(filteredData[,y]))
      
      result <- rbind(result, cbind(el = as.factor(rep(i, len)), Rcpm:::simple_ellipse(x = x1, y = y1, alpha = alpha, len = len)))
    }
  } else {
    result <- rbind(result, cbind(el = as.factor(rep("All", len)), Rcpm:::simple_ellipse(x = x, y = y, alpha = alpha, len = len)))
  }
  
  return(result)
}
