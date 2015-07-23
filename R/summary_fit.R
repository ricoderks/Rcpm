#' show summary of fit of a PCA model
#' @description Create a plot which shows a summary of the fit your PCA model.
#' @param mydata data frame holding all data. See details for
#' @return A ggplot2 plot is returned.
#' @details The data frame mydata should contain 3 variables. \code{PC},
#'   \code{R2cum} and \code{Q2cum}.
#' @author Rico Derks
#' @examples
#' \dontrun{
#'    # Here a PCA model (M1) was build with package pcaMethods
#'    sumfit <- data.frame(PC=paste("PC", 1:5, sep=""), R2cum=M1@@R2cum, Q2cum=M1@@cvstat);
#'    p <- summary_fit(sumfit);
#' }
summary_fit <- function(mydata){
  mydata.long <- summary_fit_data(mydata);
  p <- ggplot2::ggplot(data=mydata.long, ggplot2::aes(x=PC, y=value, fill=variable));
  p <- p + ggplot2::geom_bar(stat="identity", position="dodge");
  return(p);
}
