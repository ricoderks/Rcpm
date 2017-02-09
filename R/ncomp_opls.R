#' @title Find the optimal number of OPLS components
#'
#' @description Find the optimal number of OPLS components based on the highest Q2 value.
#' 
#' @param Xi X matrix N samples x K variables
#' @param Yi Y matrix N samples x I variables (currently works only with single Y)
#' @param prep preprocessing ("uv" for UV scaling, "mc" for mean centering, "no" for no preprocessing)
#' @param nrcv number of folds in cross-validation
#'
#' @return Output is a list of three elements: 
#' \item{A}{number of correlated components} 
#' \item{ncox}{number of orthogonal components in X}
#' \item{ncoy}{number of orthogonal components in Y}
#' 
#' @details Find the optimal number of OPLS components based on the highest Q2 value.
#' 
#' @export
#'
#' @author E. Nevedmoskaya
#' @author Rico Derks
ncomp_opls <- function(Xi, Yi, prep, nrcv){
### Find the optimal number of OPLS components based on the highest Q2 value
### INPUT
#	Xi			=	X matrix N samples x K variables
#	Yi			=	Y matrix N samples x I variables (currently works only with single Y)
#	prep		=	preprocessing ("uv" for UV scaling, "mc" for mean centering, "no" for no preprocessing)
#	nrcv		=	number of folds in cross-validation
###	OUTPUT
#	List of three elements: 
#		$A 		=	number of correlated components
#		$ncox	=	number of orthogonal components in X
#		$ncoy	=	number of orthogonal components in Y
	#### Estimate number of components for O2PLS 
	if (prep == "no") {
		X <- Xi
		Y <- Yi
	}
	
	if (prep == "mc") {
		X <- scale(Xi, scale = FALSE)
		Y <- scale(Yi, scale = FALSE)
	}	
	
	if (prep == "uv") {
		X <- scale(Xi)
		Y <- scale(Yi)
	}	
	
	#else stop("unknown preprocessing")
#x <- t(Y)%*%X
#y <- t(X)
	
	
#CV <- t(Y)%*%X
#A <- qr(CV)$rank
#A <-1
	### Estimate number of Y-orthogonal components by nrcv-fold CV
	Q2 <- opls(X, Y, prep, 1, 0, 0, nrcv)$Q2Yhatcum
	
	A <- 1
	if (ncol(Yi) > 1){
		for (i in 1:(ncol(Xi) - 1)) {
			Q2 <- c(Q2, opls(X, Y, prep, i + 1, 0, 0, nrcv)$Q2Yhatcum)
			if (Q2[i + 1] < Q2[i]) {
			  break
			}
		}
		A <- i - 1
	}
	
	Q2 <- opls(X, Y, prep, A, 0, 0, nrcv)$Q2Yhatcum
	
	for (i in 1:ncol(Xi)) {
		Q2 <- c(Q2, opls(X, Y, prep, A, i, 0, nrcv)$Q2Yhatcum)
		if (Q2[i + 1] < Q2[i]) {
		  break
		}
	}
	ncox <- i - 1
	
	### Estimate number of X-orthogonal components by nrcv-fold CV
	Q2 <- opls(X, Y, prep, A, ncox, 0, nrcv)$Q2Xhatcum
	ncoy <- 0
	
	if (ncol(Yi) > 1) {
		for (i in 1:ncol(Yi)) {
			Q2 <- c(Q2, opls(X, Y, prep, A, ncox, i, nrcv)$Q2Xhatcum)
			if (Q2[i + 1] < Q2[i]) {
			  break
			}
		}
		ncoy <- i - 1
	}
	
	rr <- list(A, ncox, ncoy)
	names(rr) <- c("A", "ncox", "ncoy")
	return(rr)
}