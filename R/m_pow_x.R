m_pow_x <- function(data, pca_model) {
	# calculate initial standard deviation of each variable
	SV0k <- apply(data, 2, function(x) sd(x, na.rm=TRUE));
	# get the residuals of the PCA model
	res_pca <- residuals(pca_model);
	# calculate the standard deviation of the residuals of the PCA model 
	SVk <- apply(res_pca, 2, sd);
	# calculate modeling power of the variable
	MpowX <- 1 - SVk / SV0k;
	return(MpowX);
}