###--- Written by E.Nevedomskaya 2007-2013---###

### Perform Probabilistic Quotient Normalization (Median Fold Change)
### INPUT
#	A		=	table to normalize samples * variables (rows * columns) 
#	n		=	normalization reference: "mean" for using the overall average of variables as reference or "median" for using the overall median of variables as reference; 
#	QC		=	vector of number(s) to specify samples which average to use as reference (e.g. QC samples)
###	OUTPUT
#	A.norm	=	normalized table samples * variables (rows * columns)

####---	NOTE: when using "mean" gives a warning, which can be ignored (only for PQN.org)---###

PQN <- function(X, n="median", QC) {
	# initialize output matrix
	X.norm <- matrix(nrow=nrow(X), ncol=ncol(X));
	colnames(X.norm) <- colnames(X);
	rownames(X.norm) <- rownames(X);

	if (exists("QC")) {
		# if QC vector exists, use this as reference spectrum
		if (length(QC)==1) {
			# only 1 reference spectrum given
			mX <- as.numeric(X[QC, ]);
		} else {
			if (n == "mean") {
				mX <- as.numeric(colMeans(X[QC, ]));
			}
			if (n == "median") {
				mX <- as.numeric(apply(X[QC, ], 2, median));
			}
		}
	} else {
		# otherwise use the mean or median of all spectra as referece spectrum
		if (n == "mean") {
			mX <- as.numeric(colMeans(X));
		}
		if (n == "median") {
			mX <- as.numeric(apply(X, 2, median));
		}
	}

	# do the actual normalisation	
	for (a in 1:nrow(X)) {
		X.norm[a,] <- as.numeric(X[a, ] / median(as.numeric(X[a, ] / mX)));
	}

	return(X.norm);	
}

