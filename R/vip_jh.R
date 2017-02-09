#' @title Implementation of VIP (Variable Importance in Projection) for the pls package.
#' 
#' @description vip is an implementation of the Variance Importance in Projection, as described in 
#' Chong, Il-Gyo & Jun, Chi-Hyuck, 2005, Performance of some variable selection methods when 
#' multicollinearity is present, Chemometrics and Intelligent Laboratory Systems 78, 103-112. 
#' It currently only works with single-response ortghogonal scores plsr models.
#'
#' @param object a pls object
#' @param j variable number
#' @param h component number
#'
#' @return vip_jh returns the VIP of variable j with h component.
#' 
#' @export
#'
#' @author Bjorn-Helge Mevik
#' @author Rico Derks
#' @references http://mevik.net/work/software/pls.html
#' @references As described in Chong, Il-Gyo & Jun, Chi-Hyuck, 2005, Performance of some 
#' variable selection methods when multicollinearity is present, Chemometrics and Intelligent 
#' Laboratory Systems 78, 103-112.
vip_jh <- function(object, j, h) {
  # VIPjh returns the VIP of variable j with h component
  if (object$method != "oscorespls")
    stop("Only implemented for orthogonal scores algorithm.  Refit with 'method = \"oscorespls\"'")
  if (nrow(object$Yloadings) > 1)
    stop("Only implemented for single-response models")

  b <- c(object$Yloadings)[1:h]
  T <- object$scores[,1:h, drop = FALSE]
  SS <- b^2 * colSums(T^2)
  W <- object$loading.weights[,1:h, drop = FALSE]
  Wnorm2 <- colSums(W^2)
  sqrt(nrow(W) * sum(SS * W[j,]^2 / Wnorm2) / sum(SS))
}
