#' @title Do a reverse prediction on a linear regression
#' 
#' @description Predict x values from y values. As with calibration curves, determine the 
#' concentration (x) from the peak area (y).
#'
#' @param model regression model of class \code{lm}
#' @param new_y the new y value or multiplicate of y as a vector
#' @param level confidence interval, default = 0.95
#'
#' @return A list with :
#' \item{x}{the predict x value}
#' \item{confidence_interval}{the confidence interval of the predict value}
#' 
#' @export
#' @importFrom stats coef qt
#'
#' @author Rico Derks
#' @examples 
#' set.seed(123)
#' # dummy data
#' my_data <- data.frame(x = 1:10,
#'                       y = 1:10 + rnorm(10))
#' 
#' # Create linear model
#' model <- lm(y ~ x, 
#'             data = my_data)
#'             
#' # Single y
#' reverse_predict(model = model,
#'                 new_y = 5)
#' # $x
#' # [1] 4.874066
#' # $confidence_interval
#' # [1] 2.578921   
#'
#' # Triplicate y's
#' reverse_predict(model = model,
#'                 new_y = c(5.0, 4.9, 5.3))
#' # $x
#' # [1] 4.946685
#' # $confidence_interval
#' # [1] 1.622068             
reverse_predict <- function(model, new_y, level = 0.95) {
  # sanity check --------------------------------------------------
  if (!("lm" %in% is(model))) {
    stop("model needs to be of class lm!")
  }
  if (!is.atomic(new_y) || !is.numeric(new_y)) {
    stop("new_y needs to be a numeric vector!")
  }
  if (!is.numeric(level) || level < 0 || level > 1) {
    stop("level needs to be a number between 0 and 1!")
  }
  # ---------------------------------------------------------------
  # calculate the new x value (x_pred)
  x_pred <- (mean(new_y) - coef(model)[1]) / coef(model)[2]
  # remove the names
  names(x_pred) <- NULL
  
  # standard deviation in the y-direction, which estimates the random errors in the y-direction
  # this is the residual standard error
  syx <- sqrt(sum((model$model$y - model$fitted.values)^2) / (length(model$model$y) - 2))
  
  # need to find the error in x. This can be done with the standard deviation in x_pred
  sx_pred <- syx / coef(model)[2] * sqrt((1 / length(new_y)) + (1 / length(model$model$y)) + ((mean(new_y) - mean(model$model$y))^2 / (coef(model)[2]^2 * sum((model$model$x - mean(model$model$x))^2))))
  names(sx_pred) <- NULL
  
  # obtain 95% confidence interval two tailed t-statistic for n-2 degrees of freedom
  tv <- qt(p = (1 - (1 - level)/2), 
           lower.tail = TRUE, 
           df = model$df.residual)
  
  # calculate the confidence interval
  conf_interval <- tv * sx_pred
  
  return(list(x = x_pred, confidence_interval = conf_interval))
}
