perm_test <- function(pls_model, perm=200, scale=FALSE) {
	Y <- as.matrix(pls_model$model[1]);
	X <- as.matrix(pls_model$model[2]);
	nComps <- pls_model$ncomp;
	# seed the random generator
	set.seed(unclass(Sys.time()));
	# dataframe to hold the results
	perm_results <- data.frame(corr=vector(mode="numeric", length=perm), R2Ycum=vector(mode="numeric", length=perm), Q2cum=vector(mode="numeric", length=200));
	for (a in 1:perm) {
		# do the permutation
		Yperm <- Y[sample(1:nrow(Y), nrow(Y)), ];
		Mperm_pls <- pls::plsr(Yperm ~ X, ncomp=nComps, validation=pls_model$validation$method, scale=scale, method=pls_model$method);
		# calculate correlation between original Y and permutated Y (default is Pearsons correlation)
		perm_results$corr[a] <- abs(cor(Y, Yperm));
		# calculate R2Ycum
		rss_y <- colSums(colSums(Mperm_pls$residuals^2));
		rss0 <- sum(Mperm_pls$validation$PRESS0);
		perm_results$R2Ycum[a] <- (1 - (rss_y / rss0))[nComps];
		# calculate Q2cum
		press <- colSums(Mperm_pls$validation$PRESS);
		press0 <- Mperm_pls$validation$PRESS0;
		perm_results$Q2cum[a] <- (1 - (press / press0))[nComps];
	}
	#### add the values for the original model
	# calculate R2Ycum
	rss_y <- colSums(colSums(pls_model$residuals^2));
	rss0 <- sum(pls_model$validation$PRESS0);
	R2Ycum <- (1 - (rss_y / rss0))[nComps];
	# calculate Q2cum
	press <- colSums(pls_model$validation$PRESS);
	press0 <- pls_model$validation$PRESS0;
	Q2cum <- (1 - (press / press0))[nComps];	
	perm_results <- rbind(perm_results, c(1, R2Ycum, Q2cum));
	
	# create the plot
	cols <- c("R2Ycum"="green", "Q2cum"="blue");
	p <- ggplot2::ggplot(data=perm_results, ggplot2::aes(x=corr));
	p <- p + ggplot2::geom_point(ggplot2::aes(y=R2Ycum, colour="R2Ycum"), size=3);
	p <- p + ggplot2::geom_point(ggplot2::aes(y=Q2cum, colour="Q2cum"), size=3);
	p <- p + ggplot2::geom_hline(ggplot2::aes(yintercept=0), colour="gray");
	p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept=0), colour="gray");
	p <- p + ggplot2::scale_x_continuous(name=paste(perm, "permutations", nComps ,"components", sep=" "));
	p <- p + ggplot2::scale_y_continuous(name="Value");
	p <- p + ggplot2::scale_colour_manual(values=cols, name="Value");
	return(p);
}