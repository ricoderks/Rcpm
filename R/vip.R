#' Implementation of VIP (Variable Importance in Projection) for the pls package
#' @param object a pls object
#' @return vip returns all VIP values for all variables and all number of components
#' @description vip is an implementation of the Variance Importance in Projection, as described in Chong, Il-Gyo & Jun, Chi-Hyuck, 2005, Performance of some variable selection methods when multicollinearity is present, Chemometrics and Intelligent Laboratory Systems 78, 103–112. It currently only works with single-response ortghogonal scores plsr models.
#' @author Bjorn-Helge Mevik
#' @references http://mevik.net/work/software/pls.html
#' @references  As described in Chong, Il-Gyo & Jun, Chi-Hyuck, 2005, Performance of some variable selection methods when multicollinearity is present, Chemometrics and Intelligent Laboratory Systems 78, 103-112.
vip <- function(object) {
  if (object$method != "oscorespls")
    stop("Only implemented for orthogonal scores algorithm.  Refit with 'method = \"oscorespls\"'")
  if (nrow(object$Yloadings) > 1)
    stop("Only implemented for single-response models")

  SS <- c(object$Yloadings)^2 * colSums(object$scores^2)
  Wnorm2 <- colSums(object$loading.weights^2)
  SSW <- sweep(object$loading.weights^2, 2, SS / Wnorm2, "*")
  sqrt(nrow(SSW) * apply(SSW, 1, cumsum) / cumsum(SS))
}
