#' @title Multilevel Simultaneous Component Analysis (MSCA-P)
#' 
#' @description Multilevel Simultaneous Component Analysis (MSCA-P).
#'
#' @param X a data table with structure ((I x K) x J), see details for more information
#' @param persID vector length (IxK) person ids (numeric e.g. for the situation with 4 samples per person 1 1 1 1 2 2 2 2 3 3 3 3, or factor)
#'
#' @return A data frame with the following structure :
#' \item{$between$data}{table used for the between-individual level}
#' \item{$between$scores}{between-individual scores}
#' \item{$between$loadings}{between-individual loadings}
#' \item{$between$percexp}{percentage of variation explained in the between-individual model}
#' \item{$within[[i]]$data}{table used for the within-individual level for individual i}
#' \item{$within[[i]]$scores}{within-individual scores for individual i}
#' \item{$within[[i]]$loadings}{within-individual loadings for individual i}
#' \item{$within[[i]]$percexp}{percentage of variation explained in the within-individual model for individual i}
#' 
#' @details Unlike MSCA, MSCA-P does not make any assumptions about the similarities between the time-dynamic variations of different individuals, 
#' so scores and loadings will be computed separately for each individual. X needs to have the following data table structure ((I x K) x J). 
#' Where I are the individuals, J are the variables, and K are samples of an individual.
#' 
#' Note: msca_p can NOT handle NA's!
#' 
#' Based on the algorithm Jansen et al. ANALYTICA CHIMICA ACTA (2005) 530(2):173-183
#' Based on MatLab code available here http://www.bdagroup.nl/content/Downloads/software/software.php
#' 
#' @export
#' @importFrom pcaMethods svdPca 
#'
#' @author E. Nevedomskaya
#' @references Jansen et al. ANALYTICA CHIMICA ACTA (2005) 530(2):173-183
msca_p <- function(X, persID) {
	
#	persID <- as.numeric(as.factor(as.character(persID)))
	np <- length(unique(persID))
	nn <- length(persID) / np
	
	DesignMat <- matrix(data = 0, ncol = np, nrow = length(persID))
	for (i in 1:np) {
	  DesignMat[which(persID == i), i] <- 1
	}
	
	I <- ncol(DesignMat)
	J <- ncol(X)
	K <- nrow(X)
	Ki <- c()
	for (i in 1:I) {
	  Ki <- c(Ki, length(which(DesignMat[, i] == 1)))
	}
	rm(i)
	
	### Part I: Calculate Offset
	offset <- colMeans(X, na.rm = TRUE)
	
	# Subtract offset from Data
	Xoff <- X - cbind(rep(1, K)) %*% offset
	
	### Part II: Between individual scores
	# Calculate mean of each individual
	Xind <- c()
	
	for (i in 1:I) {
		Xind <- rbind(Xind, colMeans(Xoff[DesignMat[, i] == 1, ]))
	}
	
	Xindsc <- Xind
	
	W <- diag(sqrt(Ki))
	
	pca_b <- pcaMethods::svdPca(W %*% Xindsc, nPcs = I, center = FALSE)
	
	pca_b@scores <- pca_b@scores %*% solve(W)
	colnames(pca_b@scores) <- paste("PC", 1:ncol(pca_b@scores), sep = "")
	
	### Part III: Within individual scores
	# Center data for each individual
	Xwit <- Xoff - DesignMat %*% Xind
	Xwitsc <- Xwit
	
	# Calculate within-individual model
	pca_w <- vector(mode = "list", length = I)
	
	for (i in 1:I) {
		pp <- pcaMethods::svdPca(Xwitsc[DesignMat[, i] == 1, ], nPcs = Ki[i], center = FALSE)
		pca_w[i] <- pp
	}
	
	Model <- c()
	
	Model$between$data <- Xindsc
	Model$between$scores <- pca_b@scores
	Model$between$loadings <- pca_b@loadings
	Model$between$percexp <- pca_b@R2cum
	
	Model$within <- vector(mode = "list", length = I)
	
	for (i in 1:I) {
		Model$within[[i]]$scores <- pca_w[[i]]@scores
		Model$within[[i]]$loadings <- pca_w[[i]]@loadings
		Model$within[[i]]$R2cum <- pca_w[[i]]@R2cum
	}
	
	return(Model)
}