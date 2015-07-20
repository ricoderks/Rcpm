#' Perform QC-RLSC for batch correction of chromatographic signal
#'
#' @param tab table N*K (row * column) with N samples and K variables
#' @param colv vector N of numbers: 1 for QC samples and 2 for other samples
#' @param or vector of measuring order (see details)
#' @details Make sure that everything is sorted in measurement order!!!
#' @return corrected table N*K
#' @author E. Nevedomskaya
#' @references 1.Dunn et al. Nature Protocols 6, 1060-1083 (2011)
qc_rlsc <- function(tab, colv, or) {
	# create table of the same sizeas initial
	tab_corr <- tab;
	# For each variable (columns) in the initial table
	for (i in 1:ncol(tab)) {
		# fit loess curve to the QCs
		ll <- loess(tab[which(colv==1), i] ~ or[which(colv==1)]);
		# approximate the curve for all the samples
		aa <- approx(x=or[which(colv==1)], y=ll$fitted, xout=or);
		# correct the variable according to the curve for all the samples
		tab_corr[,i] <- tab[,i]/aa$y;
		# print which variable has been corrected in order to monitor the progress
		#print(i);
	}
	return(tab_corr);
}

