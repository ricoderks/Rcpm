#' @title Modeling power
#'
#' @description Calculate the modeling power of each variable from a PCA model.
#'
#' @param data is a matrix of the original data (centered and scaled)
#' @param pca_model is the pca model from the package pcaMethods
#'
#' @return Vector of the modeling power for each variable
#' 
#' @details Calculates the modeling power of each variable from a PCA model as is done in Umetrics Simca.
#' 
#' @export
#' @importFrom stats sd residuals
#'
#' @author Rico Derks
m_pow_x <- function(data, pca_model) {
	# calculate initial standard deviation of each variable
	SV0k <- apply(data, 2, function(x) sd(x, na.rm = TRUE))
	
	# get the residuals of the PCA model
	res_pca <- residuals(pca_model)
	
	# calculate the standard deviation of the residuals of the PCA model 
	SVk <- apply(res_pca, 2, sd)
	
	# calculate modeling power of the variable
	MpowX <- 1 - SVk / SV0k
	
	return(MpowX)
}