reverse_predict <- function(model, new_y, level=0.95) {
	# sanity check --------------------------------------------------
	if (class(model) != "lm") {
		stop("model needs to be of class lm!");
	}
	if (!is.atomic(new_y) || !is.numeric(new_y)) {
		stop("new_y needs to be a numeric vector!");
	}
	if (!is.numeric(level) || level < 0 || level > 1) {
		stop("level needs to be a number between 0 and 1!");
	}
	# ---------------------------------------------------------------
	# calculate the new x value (x0)
	x0 <- (mean(new_y) - coef(model)[1]) / coef(model)[2];
	# remove the names
	names(x0) <- NULL;
	
	# standard deviation in the y-direction, which estimates the random errors in the y-direction
	# this is the residual standard error
	syx <- sqrt(sum((model$model$y - model$fitted.values)^2) / (length(model$model$y) - 2));
	
	# need to find the error in x. This can be done with the standard deviation in x0
	sx0 <- syx / coef(model)[2] * sqrt((1 / length(new_y)) + (1 / length(model$model$y)) + ((mean(new_y) - mean(model$model$y))^2 / (coef(model)[2]^2 * sum((model$model$x - mean(model$model$x))^2))));
	names(sx0) <- NULL;
	
	# obtain 95% confidence interval two tailed t-statistic for n-2 degrees of freedom
	tv <- qt(p=(1 - (1 - level)/2), lower.tail=TRUE, df=model$df.residual);
	
	# calculate the confidence interval
	conf_interval <- tv * sx0;
	return(list(x=x0, confidence_interval=conf_interval));
}