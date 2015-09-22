pqn <- function(X, n="median", QC) {
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

