#' @title Multilevel Simultaneous Component Analysis (MSCA)
#'
#' @description Multilevel Simultaneous Component Analysis (MSCA).
#'
#' @param X a data table with structure ((I x K) x J), see details for more information
#' @param persID vector length (IxK) person ids (numeric e.g. for the situation with 4 samples per person 1 1 1 1 2 2 2 2 3 3 3 3, or factor)
#'
#' @return A data frame with the following structure :
#' \item{$between$data}{table used for the between-individual level}
#' \item{$between$scores}{between-individual scores}
#' \item{$between$loadings}{between-individual loadings}
#' \item{$between$percexp}{percentage of variation explained in the between-individual model}
#' \item{$within$data}{table used for the within-individual level}
#' \item{$within$scores}{within-individual scores}
#' \item{$within$loadings}{within-individual loadings}
#' \item{$within$percexp}{percentage of variation explained in the within-individual model}
#' 
#' @details X needs to have the following data table structure ((I x K) x J). Where I are the individuals, J are the variables, and K are samples 
#' of an individual.
#' 
#' Note: msca can NOT handle NA's!
#' 
#' Based on the algorithm Jansen et al. ANALYTICA CHIMICA ACTA (2005) 530(2):173-183
#' Based on MatLab code available here http://www.bdagroup.nl/content/Downloads/software/software.php
#' 
#' @export
#' @importFrom pcaMethods svdPca
#'
#' @author E. Nevedomskaya
#' @references Jansen et al. ANALYTICA CHIMICA ACTA (2005) 530(2):173-183
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