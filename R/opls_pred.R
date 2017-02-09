#' @title Prediction function of the opls package
#' 
#' @description Prediction function of the opls package.
#' 
#' @param A Observations
#' @param model opls model
#' @param direction from x to y ('xy') or from y to x ('yx')
#'
#' @return Matrix :
#' \item{Bhat}{Predictions}
#' \item{Scorenew}{Corresponding scores}
#' 
#' @details Prediction function of the opls package.
#' 
#' @export
#'
#' @author E. Nevedomskaya
#' @author Rico Derks
opls_pred <- function(A, model, direction) {
	
# Prediction function of the o2pls package
#
# Input:
#   A: Observations
#   model: opls model
#   direction: from x to y ('xy') or from y to x ('yx')
# Output
#   Bhat : Predictions
#   Scorenew : Corresponding scores
	
	modelPred <- c()
	
	if (direction == "xy") {
		To <- c()
		
		if (length(model$TYosc) != 0) {
			for (i in 1:ncol(model$TYosc)) {
				to <- A %*% model$Wosc[ , i] %*% solve((t(model$Wosc[ , i]) %*% model$Wosc[ , i]))
				To <- cbind(To, to)
				A <- A - to %*% t(model$PYosc[ , i])
			}
		}
		
		T <- A %*% model$svd$u %*% solve((t(model$svd$u) %*% model$svd$u))
		Yhat <- T %*% model$Bt %*% t(model$svd$v)
		
		modelPred$T <- T
		modelPred$Yhat <- Yhat
		if (length(model$TYosc) != 0) modelPred$To <- To
	}
	
	if (direction == "yx"){
		Uo <- c()
		
		if (length(model$UXosc) !=0 ) {
			for (i in 1:ncol(model$UXosc)) { 
				uo <- A %*% model$Cosc[ , i] %*% solve((t(model$Cosc[ , i]) %*% model$Cosc[ , i]))
				Uo <- cbind(Uo, uo) 
				A <- A-uo %*% t(model$PXosc[ , i])
			}
		}
		
		U <- A %*% model$svd$v %*% solve((t(model$svd$v) %*% model$svd$v))
		Xhat <- U %*% model$Bu %*% t(model$svd$u)
		modelPred$U <- U
		modelPred$Xhat <- Xhat
		if (length(model$UXosc) != 0) {
			modelPred$Uo <- Uo
		}
	}		
	
	return(modelPred)
}