#' @title Give a summary of a data frame
#' 
#' @description Gives count, mean, standard deviation, standard error of the mean, and confidence 
#' interval (default 0.95).
#'
#' @param data data frame
#' @param measurevar the name of a column that contains the variable to be summariezed
#' @param groupvars a vector containing names of columns that contain grouping variables
#' @param na.rm a boolean that indicates whether to ignore NA's
#' @param conf.interval the percent range of the confidence interval (default is 0.95)
#' @param .drop should empty fields be dropped
#'
#' @return Data frame with count, mean, standard devation, standard error of the mean and 
#' confidence interval.
#' 
#' @export
#' @importFrom plyr ddply rename
#' @importFrom stats sd qt
#'
#' @author Rico Derks
#' @references www.cookbook-r.com
summary_se <- function(data = NULL, 
                       measurevar, 
                       groupvars = NULL, 
                       na.rm = FALSE, 
                       conf.interval = 0.95, 
                       .drop = TRUE) {
  
 # New version of length which can handle NA's: if na.rm =  = T, don't count them
  length2 <- function (x, na.rm = FALSE) {
    if (na.rm) {
      sum(!is.na(x))
    } else {
      length(x)
    }
  }

  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- plyr::ddply(data,
                       groupvars,
                       .drop = .drop,
                       .fun = function(xx, col) {
                         c(N = length2(xx[[col]], na.rm = na.rm),
                           mean = mean(xx[[col]], na.rm = na.rm),
                           sd = stats::sd(xx[[col]], na.rm = na.rm)
                         )
                       },
                       measurevar)

  # Rename the "mean" column
  datac <- plyr::rename(datac, c("mean" = measurevar))

  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval:
  # e.g., if conf.interval is 0.95, use 0.975 (above/below), and use df = N-1
  ciMult <- stats::qt(conf.interval / 2 + 0.5, datac$N - 1)
  datac$ci <- datac$se * ciMult

  return(datac)
}
