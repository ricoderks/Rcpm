#' @title Create a simple ellipse for a PCA score plot
#' 
#' @description This function can be used to create a confidence (Hotteling T2) interval for a 
#' PCA score plot.
#' 
#' @param x x vector
#' @param y y vector
#' @param alpha confidence interval
#' @param len number of points to create the ellipse
#'
#' @return A data frame is returned with the points to create the ellipse.
#'
#' @details This function can be used to create a confidence (Hotteling T2) interval for a 
#' PCA score plot. This is a helper function and therefore not exported.
#'
#' @importFrom stats var qf
#'
#' @author Rico Derks
simple_ellipse <- function(x, y, alpha = 0.95, len = 200) {
  N <- length(x)
  mypi <- seq(0, 2 * pi, length = len)
  
  r1 <- sqrt(var(x) * qf(alpha, 2, N - 2) * (2*(N^2 - 1)/(N * (N - 2))))
  r2 <- sqrt(var(y) * qf(alpha, 2, N - 2) * (2*(N^2 - 1)/(N * (N - 2))))
  
  result <- data.frame(x = (r1 * cos(mypi) + mean(x)),
                       y = (r2 * sin(mypi) + mean(y)))
  
  return(result)
}
