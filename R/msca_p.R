msca_p <- function(X, persID) {
	
#	persID <- as.numeric(as.factor(as.character(persID)));
	np <- length(unique(persID));
	nn <- length(persID) / np;
	
	DesignMat <- matrix(0, ncol=np, nrow=length(persID));
	for (i in 1:np) DesignMat[which(persID==i), i] <- 1;
	
	I <- ncol(DesignMat);
	J <- ncol(X);
	K <- nrow(X);
	Ki <- c();
	for (i in 1:I) Ki <- c(Ki, length(which(DesignMat[, i]==1)));
	rm(i);
	
	### Part I: Calculate Offset
	offset <- colMeans(X, na.rm=TRUE);
	
	# Subtract offset from Data
	Xoff <- X - cbind(rep(1, K)) %*% offset;
	
	### Part II: Between individual scores
	# Calculate mean of each individual
	Xind <- c();
	
	for (i in 1:I) {
		Xind <- rbind(Xind, colMeans(Xoff[DesignMat[, i]==1, ]));
	}
	
	Xindsc <- Xind;
	
	W <- diag(sqrt(Ki));
	
	pca_b <- pcaMethods::svdPca(W %*% Xindsc, nPcs=I, center=F);
	
	pca_b@scores <- pca_b@scores %*% solve(W);
	colnames(pca_b@scores) <- paste("PC", 1:ncol(pca_b@scores), sep="");
	
	### Part III: Within individual scores
	# Center data for each individual
	Xwit <- Xoff - DesignMat %*% Xind;
	Xwitsc <- Xwit;
	
	# Calculate within-individual model
	pca_w <- vector(mode="list", length=I);
	
	for (i in 1:I) {
		pp <- pcaMethods::svdPca(Xwitsc[DesignMat[, i]==1, ], nPcs=Ki[i], center=F);
		pca_w[i] <- pp;
	}
	
	Model <- c();
	
	Model$between$data <- Xindsc;
	Model$between$scores <- pca_b@scores;
	Model$between$loadings <- pca_b@loadings;
	Model$between$percexp <- pca_b@R2cum;
	
	Model$within <- vector(mode="list", length=I);
	for (i in 1:I) {
		Model$within[[i]]$scores <- pca_w[[i]]@scores;
		Model$within[[i]]$loadings <- pca_w[[i]]@loadings;
		Model$within[[i]]$R2cum <- pca_w[[i]]@R2cum;
	}
	
	return(Model);
}