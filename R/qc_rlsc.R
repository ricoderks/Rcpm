###--- Written by E.Nevedomskaya 2007-2013---###

### Perform QC-RLSC for batch correction of chromatographic signal
### As described in Dunn et al. Nature Protocols 6, 1060–1083 (2011)
### INPUT
#	tab		= table N*K (row * column) with N samples and K variables
#	colv		= vector N of numbers: 1 for QC samples and 2 for other samples
#	or		= vector of measuring order
### OUTPUT
#	tab_corr	= corrected table N*K
#
#
# MAKE SURE TO SORT EVERYTHING IN MEASUREMENT ORDER!!!!!!

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

