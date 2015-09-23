###--- Written by E.Nevedomskaya 2007-2013---###

####--------------------------------------------------------------
#### Based on the algorithm Jansen et al. ANALYTICA CHIMICA ACTA (2005) 530(2):173-183
#### Based on MatLab code available here http://www.bdagroup.nl/content/Downloads/software/software.php
####--------------------------------------------------------------

### Function for Multilevel Simulteneous Component Analysis (MSCA)
#  Inputs:
#      X          ((I x K) x J)        =       data table
#      persID			  			   =		vector length (IxK) person ids (numeric e.g. for the situation with 4 samples per person 1 1 1 1 2 2 2 2 3 3 3 3, or factor)
#  Output: list with following elements
#
#       $between$data                  =       table used for the between-individual level
#       $between$scores                =       between-individual scores
#       $between$loadings              =       between-individual loadings
#       $between$percexp               =       percentage of variation explained in the between-individual model
#
#       $within$data                   =       table used for the within-individual level
#       $within$scores                 =       within-individual scores
#       $within$loadings               =       within-individual loadings
#       $within$percexp                =       percentage of variation explained in the within-individual model
msca <- function(X, persID) {
	# Create DesignMat
	persID <- as.numeric(as.factor(as.character(persID)))
	np <- length(unique(persID))
	nn <- length(persID) / np
	
	DesignMat <- matrix(0, ncol=np, nrow=length(persID))
	for (i in 1:np) DesignMat[which(persID == i), i] <- 1
	
#	require(pcaMethods)
	
	I <- ncol(DesignMat)
	J <- ncol(X)
	K <- nrow(X)
	Ki <- c()
	for (i in 1:I) Ki <- c(Ki, length(which(DesignMat[, i] == 1)))
	rm(i)
	
	### Part I: Calculate Offset
	offset <- colMeans(X)
	
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
	
	pca_b <- pcaMethods::svdPca(scale(W %*% Xindsc), nPcs=I, center=F)
	
	pca_b@scores <- pca_b@scores %*% solve(W)
	
	### Part III: Within individual scores
	# Center data for each individual
	Xwit <- Xoff - DesignMat %*% Xind
	Xwitsc <- Xwit
	
	# Calculate within-individual model
	pca_w <- pcaMethods::svdPca(scale(Xwitsc), nPcs=K, center=F)
	
	Model <- c()
	
	Model$between$data <- Xindsc;
	Model$between$scores <- pca_b@scores;
	Model$between$loadings <- pca_b@loadings;
	Model$between$percexp <- pca_b@R2cum;
	
	Model$within$data <- Xwitsc;
	Model$within$scores <- pca_w@scores
	Model$within$loadings <- pca_w@loadings
	Model$within$percexp <- pca_w@R2cum
	
	return(Model)
}