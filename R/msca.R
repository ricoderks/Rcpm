msca <- function(X, persID) {
	# Create DesignMat
	#persID <- as.numeric(as.factor(as.character(persID)))
	np <- length(unique(persID))
	nn <- length(persID) / np
	
	DesignMat <- matrix(data = 0, ncol = np, nrow = length(persID))
	for (i in 1:np) DesignMat[which(persID == i), i] <- 1
	
	I <- ncol(DesignMat)
	J <- ncol(X)
	K <- nrow(X)
	Ki <- c()
	for (i in 1:I) Ki <- c(Ki, length(which(DesignMat[, i] == 1)))
	rm(i)
	
	### Part I: Calculate Offset
	offset <- colMeans(X, na.rm = TRUE)
	
	# Subtract offset from Data
	Xoff <- X - cbind(rep(1, K)) %*% offset
	
	### Part II: Between individual scores
	# Calculate mean of each individual
	Xind <- c()
	
	for (i in 1:I) {
		Xind <- rbind(Xind, colMeans(Xoff[DesignMat[, i] == 1,]))
	}
	
	Xindsc <- Xind
	
	W <- diag(sqrt(Ki))
	
	pca_b <- pcaMethods::svdPca(scale(W %*% Xindsc), nPcs = I, center = FALSE)
	
	pca_b@scores <- pca_b@scores %*% solve(W)
	colnames(pca_b@scores) <- paste("PC", 1:ncol(pca_b@scores), sep = "")
	
	### Part III: Within individual scores
	# Center data for each individual
	Xwit <- Xoff - DesignMat %*% Xind
	Xwitsc <- Xwit
	
	# Calculate within-individual model
	pca_w <- pcaMethods::svdPca(scale(Xwitsc), nPcs = K, center = FALSE)
	
	Model <- c()
	
	Model$between$data <- Xindsc
	Model$between$scores <- pca_b@scores
	Model$between$loadings <- pca_b@loadings
	Model$between$percexp <- pca_b@R2cum
	
	Model$within$data <- Xwitsc
	Model$within$scores <- pca_w@scores
	Model$within$loadings <- pca_w@loadings
	Model$within$percexp <- pca_w@R2cum
	
	return(Model)
}