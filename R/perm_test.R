#' @title Permutation test for PLS and PLS-DA models
#' 
#' @description Permutation test for PLS and PLS-DA models. The Y variable is permutated and a 
#' model is then fitted. From this model the R2Ycum and Q2cum are calculated.
#'
#' @param pls_model The PLS or PLS-DA model created by the pls package
#' @param perm number of permutations (default = 200)
#' @param scale should scaling be used. Use the same settings as used in the original model 
#' (see details).
#'
#' @return Output is a plot showing the results.
#' 
#' @details The scaling for the PLS / PLS-DA model has to be given to the function. At the moment 
#' this can not be retrieved from the original model. The function \code{sample()} is used to 
#' randomize the Y variable. With \code{set.seed()} the random generator is seeded based on the 
#' current date time.
#' 
#' @export
#' @importFrom pls plsr
#' @importFrom stats cor
#' @import ggplot2
#'
#' @author Rico Derks
perm_test <- function(pls_model, perm = 200, scale = FALSE) {
  corr <- NULL            # keep check happy
  
	Y <- as.matrix(pls_model$model[1])
	X <- as.matrix(pls_model$model[2])
	nComps <- pls_model$ncomp
	# seed the random generator
	set.seed(unclass(Sys.time()))
	# dataframe to hold the results
	perm_results <- data.frame(corr = vector(mode = "numeric", length = perm),
	                           R2Ycum = vector(mode = "numeric", length = perm),
	                           Q2cum = vector(mode = "numeric", length = perm))
	for (a in 1:perm) {
		# do the permutation
		Yperm <- Y[sample(1:nrow(Y), nrow(Y)), ]
		Mperm_pls <- pls::plsr(Yperm ~ X,
		                       ncomp = nComps,
		                       validation = pls_model$validation$method,
		                       scale = scale,
		                       method = pls_model$method)
		
		# calculate correlation between original Y and permutated Y (default is Pearsons correlation)
		perm_results$corr[a] <- as.vector(abs(cor(Y, Yperm)))[1]
		
		# calculate R2Ycum
		rss_y <- colSums(colSums(Mperm_pls$residuals^2))
		rss0 <- sum(Mperm_pls$validation$PRESS0)
		perm_results$R2Ycum[a] <- (1 - (rss_y / rss0))[nComps]
		
		# calculate Q2cum
		press <- colSums(Mperm_pls$validation$PRESS)
		press0 <- Mperm_pls$validation$PRESS0
		Q2cum <- (1 - (press / press0))
		
#		if (length(which(Q2cum < -0.1)) > 0) {
#			Q2cum[which(Q2cum < -0.1)] <- -0.1
#		}
		perm_results$Q2cum[a] <- Q2cum[nComps]
	}
	#### add the values for the original model
	# calculate R2Ycum
	rss_y <- colSums(colSums(pls_model$residuals^2))
	rss0 <- sum(pls_model$validation$PRESS0)
	R2Ycum <- (1 - (rss_y / rss0))[nComps]
	# calculate Q2cum
	press <- colSums(pls_model$validation$PRESS)
	press0 <- sum(pls_model$validation$PRESS0)
	Q2cum <- (1 - (press / press0))[nComps]	
	perm_results <- rbind(perm_results, c(1, R2Ycum, Q2cum))
	
	# create the plot
	cols <- c("R2Ycum" = "green", "Q2cum" = "blue")
	p <- ggplot2::ggplot(data = perm_results, ggplot2::aes(x = corr))
	p <- p + ggplot2::geom_point(ggplot2::aes(y = R2Ycum, colour = "R2Ycum"), size=3)
	p <- p + ggplot2::geom_point(ggplot2::aes(y = Q2cum, colour = "Q2cum"), size=3)
	p <- p + ggplot2::geom_hline(ggplot2::aes(yintercept = 0), colour = "gray")
	p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = 0), colour = "gray")
	p <- p + ggplot2::scale_x_continuous(name = paste(perm, "permutations", nComps ,"components", sep = " "))
	p <- p + ggplot2::scale_y_continuous(name = "Value")
	p <- p + ggplot2::scale_colour_manual(values = cols, name = "Value")
	return(p)
}